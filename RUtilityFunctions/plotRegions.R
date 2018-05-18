plotRegions = function(regions=NA, states=T, ignoreDrawChange=F, clipRegions=NA){
  if (!exists("thk99buff") || !exists("HUC"))
  {
    stop("Must preload region data from model data setup: models.R")
  }
  
  if (!ignoreDrawChange)
  {
    op=par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  }
  
  if (!is.na(clipRegions))
  {
    plot(thk99buff[thk99buff$region %in% clipRegions,], border=NA, col=NA)
  } else {
    plot(thk99buff, border=NA, col=NA)
  }
  
  if (states)
  {
    if (!exists("stateMap"))
    {
      stateMap <<- readOGR("C:/DATA/General Maps/Gulf States/US_GulfStates.shp", "US_GulfStates")
    }
    plot(stateMap, add=T, lty=2)
  }
  
  plot(HUC[HUC$HUC4 %in% unique(thk99buff$HUC4),], add=T)
  
  if (!is.na(regions))
    ptsInRegions = thk99buff[thk99buff$region %in% regions,]
  else
    ptsInRegions = thk99buff
  
  regionHUCs = unique(ptsInRegions$HUC4)
  regionShapes = HUC[HUC$HUC4 %in% regionHUCs,]
  
  plot(regionShapes, add=T, col=NA)#"#00FF0044")
  regionKey = data.frame(HUC4=factor(ptsInRegions$HUC4), region=as.numeric(ptsInRegions$region))
  regionKey = unique(regionKey)
  
  text(coordinates(regionShapes), labels = merge(regionShapes, regionKey, by="HUC4")$region)
  
  if (!ignoreDrawChange)
  {
    par(op)
  }
}