source("../loadTHK99.R")

plotRegionsColor = function(regions=NA,
                       states=NA,
                       focus=NA,
                       allRegions=F,
                       allStates=T,
                       stateBoundaries=T,
                       drawPoints=F,
                       allPoints=F,
                       ignoreDrawChange=F,
                       labs=NA,
                       
                       FUN.before=function(){},
                       FUN.after=function(){})
{
  # Load data
  if (!exists("thk99buff"))
  {
    loadTHK99data(local=T, regions="ALL", QAplot=F, removal=T)
  }
  if (!exists("HUC"))
  {
    HUC = readOGR("C:/DATA/HUC/HUC_shapes/WBDHU4.shp", "WBDHU4")
    HUC = spTransform(HUC, proj4string(thk99buff))
  }
  if (!exists("stateMap") & (stateBoundaries | focus == "state" | !is.na(states)))
  {
    stateMap = readOGR("C:/DATA/General Maps/Gulf States/US_GulfStates.shp", "US_GulfStates")
    stateMap = spTransform(stateMap, proj4string(thk99buff))
    
    stateFP = list(
      AL = 1,
      FL = 12,
      LA = 22,
      MS = 28,
      TX = 48
    )
  }
  
  # Remove drawing margins
  if (!ignoreDrawChange)
  {
   op=par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  }

  # Clip points and HUC to regions/states if specified, otherwise full extent
  points = thk99buff
  if (!is.na(regions))
  {
    points = points[points$region %in% regions,]
  }
  HUCs = HUC[HUC$HUC4 %in% unique(points$HUC4),]
  
  
  # Set plot boundaries (focus)
  if (is.na(focus))
  {
    # Fall to default - points
    plot(points, border=NA, col=NA) #"#CCCCCC")
  } else if (focus == "region" | focus == "regions")
  {
    plot(HUCs, border=NA, col=NA) #"#CCCCCC")
  } else if (focus == "state" | focus == "states")
  {
    if (is.na(states))
    {
      includedStates = crop(stateMap, points)$STATEFP
    } else {
      includedStates = c()
      for(s in states)
      {
        if (is.null(stateFP[[s]]))
        {
          stop("Invalid state given (only AL, FL, MS, LA, TX)")
        }
        includedStates = c(includedStates, stateFP[[s]])
      }
    }
    plot(stateMap[stateMap$STATEFP %in% includedStates,], col=NA) #"#CCCCCC")
  } else {
    # Fall to default - points
    plot(points, border=NA, col=NA) #"#CCCCCC")
  }

  # Fill water
  ext = extent(par('usr'))
  rect(ext@xmin, ext@ymin, ext@xmax, ext@ymax, col="lightblue", border=NA)
  
  # Draw land
  plot(stateMap, add=T, col="beige", border=NA)
  
  FUN.before()
  
  # Draw state boundaries
  if (stateBoundaries)
  {
    plot(stateMap, add=T, lty=2)
  }

  # Plot regions
  if (allRegions)
  {
    plot(HUC[HUC$HUC4 %in% thk99buff$HUC4,], add=T)
  }
  plot(HUCs, add=T)

  # Add points
  if (drawPoints)
  {
    if (allPoints)
    {
      plot(thk99buff, add=T, col="orange", border=NA)
    } else {
      plot(points, add=T, col="orange", border=NA)
    }
  }
  
  # Add labels
  HUCinPlot = crop(HUCs, extent(par('usr')))
  
  regionKey = data.frame(HUC4=factor(points$HUC4), region=as.numeric(points$region))
  regionKey = unique(regionKey)
  HUClabs = merge(HUCinPlot, regionKey, by="HUC4")$region
  
  if (!is.na(labs) & length(labs) != length(HUClabs))
  {
    stop(sprintf("Wrong number of labs - expected %s for c(%s)", length(HUClabs), paste(HUClabs, collapse = ", ")))
  }
  if (is.na(labs))
  {
    labs = merge(HUCinPlot, regionKey, by="HUC4")$region
  }
  text(coordinates(HUCinPlot), labels = labs)
  
  FUN.after()
  
  if (ignoreDrawChange)
  {
    par(op)
  }
}

plotRegionsColor(regions = c(9,10), focus="region", drawPoints=T, ignoreDrawChange=T, FUN.before=function(){
  source("prepRestoreData.R")
  restore = restore[restore$YEAR <= 2005 & restore$YEAR > 1900,]
  removal = order(restore$ACRES, decreasing=TRUE)[1:4]
  restore = restore[-removal,]
  restore = spTransform(restore, proj4string(thk99buff))
  plot(restore, add=T, col="forestgreen", border=NA)
})
