library(methods) #TO DO: figure out why this is here...
library(rsyncrosim)
library(roads)
library(raster)
Sys.setenv(TZ='EST')
optionMapping = list("Snapping"="snap","Minimum Spanning Tree"="mst","Sequential Least Cost Path"="lcp")

GetDataSheetExpectData <- function(name, ssimObj) {
  ds = datasheet(ssimObj, name)
  if (nrow(ds) == 0) { stop(paste0("No data for: ", name)) }
  return(ds)
}

GetSingleValueExpectData <- function(df, name) {
  v = df[, name]
  if (is.na(v)) { stop(paste0("Missing data for: ", name)) }
  return(v)
}

#e=list(LibraryFilePath="D:/JHMisc/Churchill",ProjectId=1,ScenarioId=428)

#Environment
e = ssimEnvironment()
GLOBAL_Session = session("D:/SyncroSim-2-1-0-Beta-2019-05-08/syncrosim-windows-2-1-0")
#GLOBAL_Session = session("c:/gitprojects/ssimbin")
GLOBAL_Library = ssimLibrary(name = e$LibraryFilePath, session = GLOBAL_Session)
GLOBAL_Project = project(GLOBAL_Library, project = as.integer(e$ProjectId))
GLOBAL_Scenario = scenario(GLOBAL_Library, scenario = as.integer(e$ScenarioId))
GLOBAL_RunControl = GetDataSheetExpectData("STSim_RunControl", GLOBAL_Scenario)
#datasheet(GLOBAL_Scenario,"ROADS_OutputRaster")
GLOBAL_MaxIteration = GetSingleValueExpectData(GLOBAL_RunControl, "MaximumIteration")
GLOBAL_MinIteration = GetSingleValueExpectData(GLOBAL_RunControl, "MinimumIteration")
GLOBAL_MinTimestep = GetSingleValueExpectData(GLOBAL_RunControl, "MinimumTimestep")
GLOBAL_MaxTimestep = GetSingleValueExpectData(GLOBAL_RunControl, "MaximumTimestep")
GLOBAL_TotalIterations = (GLOBAL_MaxIteration - GLOBAL_MinIteration + 1)
GLOBAL_TotalTimesteps = (GLOBAL_MaxTimestep - GLOBAL_MinTimestep + 1)

#Simulation
envBeginSimulation(GLOBAL_TotalIterations * GLOBAL_TotalTimesteps)

rdRoad  = datasheet(GLOBAL_Scenario,"ROADS_Road")
roadMethod = optionMapping[[rdRoad$RoadBuildingAlgorithm]]

myStratum = datasheetRaster(GLOBAL_Scenario,datasheet="STSim_InitialConditionsSpatial",column="StratumFileName")
initialRoads =  raster(rdRoad$InitialRoadRaster)
initialCost = raster(rdRoad$CostRaster)
#TO DO: check for alignment.

rdTransitionRaster = datasheet(GLOBAL_Scenario,"ROADS_TransitionRaster")
#TO DO: figure out how to use rdTransitionRaster inputs.

cSheet =  "ROADS_OutputRaster"
oDat = datasheet(GLOBAL_Scenario,cSheet)
for (iteration in GLOBAL_MinIteration:GLOBAL_MaxIteration) {
  #iteration=1
  
  #QUESTION: Why are all results copied? Only need SpatialTransitions.
  if(nrow(rdTransitionRaster)==0){
    newBlocks = datasheetRaster(GLOBAL_Scenario,datasheet="STSim_OutputSpatialTransition",iteration=iteration,timestep=max(1,GLOBAL_MinTimestep):GLOBAL_MaxTimestep,subset=expression(TransitionGroupID==rdRoad$RoadTransitionGroup))
  }else{stop("TO DO: Handle input transitions case.")}
  if(class(newBlocks)=="RasterLayer"){
    newBlocks=brick(newBlocks)
    names(newBlocks)=paste0("tg.it.ts",GLOBAL_MaxTimestep)
  }
  
  #NOTE: would be nice if maps were automatically pulled in correct order
  tag = paste(c(strsplit(names(newBlocks)[[1]],".",fixed=T)[[1]][1:2]),collapse=".")
  newBlocks=subset(newBlocks, paste0(tag,".ts",max(1,GLOBAL_MinTimestep):GLOBAL_MaxTimestep))
  newBlocks[newBlocks>0] = 1
  newBlocks[newBlocks<0] = NA
  
  #TO DO - sort out cost surface. For now cost on current roads and newBlocks is 0, otherwise 1. 1000 for water bodies.
  cost = simpleCost(initialRoads,newBlocks[[1]],initialCost)
  
  #QUESTION: checking for validity of initialRoads and initialCost already done?
  sim=list()
  for (timestep in GLOBAL_MinTimestep:GLOBAL_MaxTimestep) {
    #iteration = 1;timestep=1
    #NOTE: presuming this will work in SyncroSim context...
    envReportProgress(iteration, timestep)
    
    cm = paste0(tag,".ts",timestep)
    if(!is.element(cm,names(newBlocks))){
      envStepSimulation()
      next
    }
    
    if(length(sim)==0){
      sim = projectRoads(newBlocks[[cm]],cost,initialRoads,roadMethod=roadMethod,plotRoads=T)
    }else{
      #TO DO: how to preferentially route roads through cutblocks after the first iteration?
      sim = projectRoads(newBlocks[[cm]],plotRoads=T,sim=sim)
    }
    
    outRoads = sim$roads>0 #ignoring values for now.
    
    outRoadName = paste0("roads.it",iteration,".ts",timestep) 
    roadDir =paste0(filepath(GLOBAL_Scenario),".output/Scenario-",scenarioId(GLOBAL_Scenario),"/",cSheet)
    outRoadPath = paste0(roadDir,"/",outRoadName,".tif")
    dir.create(roadDir,recursive=T)
    writeRaster(outRoads,outRoadPath,overwrite=T)
    
    #datasheet(GLOBAL_Scenario,cSheet)
    oDat = addRow(oDat,data.frame(Iteration=iteration,Timestep=timestep,Filename=outRoadPath))
    
    envStepSimulation()
  }
}

#QUESTION: how do I load a result into the database?
saveDatasheet(GLOBAL_Scenario,data=oDat,name=cSheet,append=F)

envEndSimulation()

