optionMapping = list("Snapping"="snap","Minimum Spanning Tree"="mst","Sequential Least Cost Path"="lcp")

roadMethod = optionMapping[[rdRoad$RoadBuildingAlgorithm]]

myStratum = datasheetRaster(GLOBAL_Scenario,datasheet="STSim_InitialConditionsSpatial",column="StratumFileName")
initialRoads =  raster(rdRoad$InitialRoadRaster)
initialCost = raster(rdRoad$CostRaster)
#TO DO: check for alignment.

rdTransitionRaster = datasheet(GLOBAL_Scenario,"ROADS_TransitionRaster")
#TO DO: figure out how to use rdTransitionRaster inputs.


for (iteration in GLOBAL_MinIteration:GLOBAL_MaxIteration) {
  #iteration=1
  
  #QUESTION: Why are all results copied? Only need SpatialTransitions.
  if(nrow(rdTransitionRaster)==0){
    
    #newBlocks = datasheetRaster(GLOBAL_Scenario,datasheet="STSim_OutputSpatialTransition",iteration=iteration,timestep=max(1,GLOBAL_MinTimestep):GLOBAL_MaxTimestep)
    
    #selectBlocks = names(newBlocks)[grepl("974",names(newBlocks),fixed=T)]
    #newBlocks=subset(newBlocks,selectBlocks)
    
    #TO DO: fix this. Something goes wrong with subset, but only sometimes, usually deep in parallel processing. Sigh
    #names(newBlocks)
    #tGrps = datasheet(GLOBAL_Scenario,"STSim_TransitionGroup")
    #tGrpId = datasheet(GLOBAL_Scenario,"STSim_TransitionGroup",lookupsAsFactors=F)
    #names(newBlocks)        
    #newBlocks
    newBlocks = datasheetRaster(GLOBAL_Scenario,datasheet="STSim_OutputSpatialTransition",iteration=iteration,timestep=max(1,GLOBAL_MinTimestep):GLOBAL_MaxTimestep,subset=expression(TransitionGroupID==rdRoad$RoadTransitionGroup))
  }else{stop("TO DO: Handle input transitions case.")}

    
  if(class(newBlocks)=="RasterLayer"){
    newBlocks=brick(newBlocks)
    names(newBlocks)=paste0("tg.it",iteration,".ts",GLOBAL_MaxTimestep)
  }
  
  if(!is.element(class(newBlocks),c("RasterBrick","RasterStack"))){
    break
  }
  
  #TO DO: solve sorting properly in rsyncrosim. This is an awful hack to deal with wierd naming that sometimes happens in parallel processing.
  #names(newBlocks)=paste0(names(newBlocks),".it19")    
  lastElement = strsplit(names(newBlocks)[[1]],".",fixed=T)[[1]]
  
  lastElement=lastElement[length(lastElement)]
  if(grepl("it",lastElement,fixed=T)){
    outRoads = data.frame(Iteration=iteration,layerNames = names(newBlocks))
  }else{
    outRoads = data.frame(Iteration=iteration,layerNames="ok")
  }
  timestep=1
  outRoadName = paste0("roads.it",iteration,".ts",timestep) 
  outRoadPath = paste0(roadDir,"/",outRoadName,".csv")
  
  write.csv(outRoads,outRoadPath,row.names = F,append=T)

  #datasheet(GLOBAL_Scenario,cSheet)
  oDat = addRow(oDat,data.frame(Iteration=iteration,Timestep=timestep,Filename=outRoadPath))
  #names(newBlocks)=gsub(paste0(".",lastElement),"",names(newBlocks),fixed=T)
  
  envStepSimulation()
}
