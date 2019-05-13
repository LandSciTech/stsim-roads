library(methods)
library(rsyncrosim)
library(roads)
library(raster)
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

#e=list(LibraryFilePath="D:/JHMisc/Churchill",ProjectId=1,ScenarioId=416)

#Environment
e = ssimEnvironment()
GLOBAL_Session = session("D:/SyncroSim-2-1-0-Beta-2019-05-08/syncrosim-windows-2-1-0")
#GLOBAL_Session = session("c:/gitprojects/ssimbin")
GLOBAL_Library = ssimLibrary(name = e$LibraryFilePath, session = GLOBAL_Session)
GLOBAL_Project = project(GLOBAL_Library, project = as.integer(e$ProjectId))
GLOBAL_Scenario = scenario(GLOBAL_Library, scenario = as.integer(e$ScenarioId))
GLOBAL_RunControl = GetDataSheetExpectData("STSim_RunControl", GLOBAL_Scenario)
GLOBAL_MaxIteration = GetSingleValueExpectData(GLOBAL_RunControl, "MaximumIteration")
GLOBAL_MinIteration = GetSingleValueExpectData(GLOBAL_RunControl, "MinimumIteration")
GLOBAL_MinTimestep = GetSingleValueExpectData(GLOBAL_RunControl, "MinimumTimestep")
GLOBAL_MaxTimestep = GetSingleValueExpectData(GLOBAL_RunControl, "MaximumTimestep")
GLOBAL_TotalIterations = (GLOBAL_MaxIteration - GLOBAL_MinIteration + 1)
GLOBAL_TotalTimesteps = (GLOBAL_MaxTimestep - GLOBAL_MinTimestep + 1)

#Simulation
envBeginSimulation(GLOBAL_TotalIterations * GLOBAL_TotalTimesteps)

for (iteration in GLOBAL_MinIteration:GLOBAL_MaxIteration) {
  rdRoad  = datasheet(GLOBAL_Scenario,"ROADS_Road")
  roadMethod = optionMapping[[rdRoad$RoadBuildingAlgorithm]]
 
  myStratum = datasheetRaster(GLOBAL_Scenario,datasheet="STSim_InitialConditionsSpatial",column="StratumFileName")
  initialRoads =  raster(rdRoad$InitialRoadRaster)
  initialCost = raster(rdRoad$CostRaster)
  
  rdTransitionRaster = datasheet(GLOBAL_Scenario,"ROADS_TransitionRaster")
  #rdTransitionRaster
  #QUESTION: how/when to use rdTransitionRaster?

  if(nrow(rdTransitionRaster)==0){
    newBlocks = datasheetRaster(GLOBAL_Scenario,datasheet="STSim_OutputSpatialTransition",iteration=iteration,timestep=max(1,GLOBAL_MinTimestep):GLOBAL_MaxTimestep,subset=expression(TransitionGroupID==rdRoad$RoadTransitionGroup))
  }else{stop("TO DO: Handle input transitions case.")}

  #NOTE: map order must be correct - would be nice if this happened automatically.
  tag = paste(c(strsplit(names(newBlocks)[[1]],".",fixed=T)[[1]][1:2]),collapse=".")
  newBlocks=subset(newBlocks, paste0(tag,".ts",max(1,GLOBAL_MinTimestep):GLOBAL_MaxTimestep))
  newBlocks[newBlocks>0] = 1
  #names(newBlocks)

  #TO DO - sort out cost surface. For now cost on current roads and newBlocks is 0, otherwise 1. 1000 for water bodies.
  cost = simpleCost(initialRoads,newBlocks[[1]],initialCost)
  
  #QUESTION: checking for validity of initialRoads and initialCost already done?
  sim=list()
  for (timestep in GLOBAL_MinTimestep:GLOBAL_MaxTimestep) {
    #iteration = 1;timestep=1
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

    plot(outRoads)    
    datasheet(GLOBAL_Scenario) 

    oDat = datasheet(GLOBAL_Scenario,"ROADS_OutputRaster")
    ?saveDatasheet
    ?save
    
    envStepSimulation()
  }
}

envEndSimulation()

