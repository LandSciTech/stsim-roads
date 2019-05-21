library(methods) #TO DO: figure out why this is here...
library(rsyncrosim)
library(roads)
library(raster)
Sys.setenv(TZ='EST')

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

#e=list(LibraryFilePath="D:/JHMisc/Churchill",ProjectId=1,ScenarioId=506)

#Environment
e = ssimEnvironment()
GLOBAL_Session = session("D:/SyncroSim 2-1-1-PRIVATE-BETA-3/syncrosim-windows-2-1-1")
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

if(nrow(rdRoad)==0){
  envEndSimulation()
  
}else{
  cSheet =  "ROADS_OutputRaster"
  
  
  oDat = datasheet(GLOBAL_Scenario,cSheet)
  
  roadDir =paste0(e$TempDirectory,"/",cSheet)
  if(!file.exists(roadDir)){
    dir.create(roadDir,recursive=T)
  }
  source(paste0(filepath(GLOBAL_Session),"/Packages/stsim-roads/transformerInner.R"))
  
  if(nrow(oDat)>0){
    saveDatasheet(GLOBAL_Scenario,data=oDat,name=cSheet,append=F)
    #datasheet(GLOBAL_Scenario,cSheet)
  }
  envEndSimulation()
}  
