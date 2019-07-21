loadTHK99data = function(local = F, ...) # ... is (regions, barrierIslands)
{
  # Local file loading from DATA folder
  if (local)
  {
    loadInfo = loadLocalTHK99(...)
  } else {
    loadInfo = loadPostgreSQLTHK99(...)
  }
  
  # loadInfo should be used to do processing. Processing is currently in the local because
  # HUC info isn't saved to a T1 THK99

  return()
}

loadPostgreSQLTHK99 = function(regions = NA, barrierIslands = F, toFiles = F, removal = T)
{
  return(NULL)
}

loadLocalTHK99 = function(QAplot = F, regions = NA, barrierIslands = F, toFiles = F, removal = T)
{
  # Check for missing required args (default NA)
  args = unlist(as.list(environment()))
  names = names(args)
  print(names)
  if (any(is.na(args)))
  {
    stop(sprintf("Missing arguments: %s", paste0(names(args[is.na(args)]), collapse=", ")))
  }
  if (!regions %in% c("2","3","ALL"))
  {
    stop("Incorrect number of regions specified: try 2, 3, or ALL")
  }
  
  library(rgdal)
  library(raster)
  thk99buff = readOGR("C:/DATA/EarthEngine/T1/thk99buff.shp", "thk99buff")
  
  # Load HUC data based on 'regions'
  if (regions == "2" | regions == "3")
  {
    HUClevel = "HUC2"
  } else if (regions == "ALL") {
    HUClevel = "HUC4"
  } else {
    stop("UNSUPPORTED NUMBER OF REGIONS: Use either ALL (HUC4), 2 or 3 (HUC2).")
  }
  HUCfilename = gsub("HUC(\\d*)", "WBDHU\\1", HUClevel) 
  HUC = readOGR(sprintf("C:/DATA/HUC/HUC_shapes/%s.shp", HUCfilename), HUCfilename)
  
  ###### Processing ######
  
  if (removal)
  {
    # Remove buffers without wetland change
    thk99buff = thk99buff[thk99buff$WET > 0,]
    
    # Remove barrier islands if chosen
    if (!barrierIslands)
    {
      shoreline = readOGR("C:/Users/GCRLWuHardy/Documents/General Maps/Coastlines/USCoast_h_L1.shp", "USCoast_h_L1")
      shoreline = spTransform(shoreline, proj4string(thk99buff))
      shoreline = crop(shoreline, thk99buff)
      thk99buff = thk99buff[!is.na(over(thk99buff, geometry(shoreline))),]
    }
  }
  
  # Extract HUC and region to each buffer
  HUC = spTransform(HUC, proj4string(thk99buff))
  hucZone = over(thk99buff,HUC[,HUClevel])
  thk99buff[[HUClevel]] = factor(hucZone[[HUClevel]])
  if (HUClevel == "HUC4") {
    thk99buff$region = as.numeric(thk99buff[[HUClevel]])
  } else if (HUClevel == "HUC2") {
    if (regions == 2)
    {
      thk99buff$region = sapply(thk99buff$HUC2, function(x){
        if (x == "03" | x == "12" | x == "13")
          return(1)
        else
          return(2)
      })
    } else if (regions == 3) {
      thk99buff$region = sapply(thk99buff$HUC2, function(x){
        if (x == "12" | x == "13") # West Gulf
          return(1)
        else if (x == "08") # LA
          return(2)
        else
          return(3) # East Gulf (03)
      })
    }
  }
  
  #Visualize regions
  if (QAplot) {
    colF = function(x){
      rainbow(length(unique(thk99buff[[HUClevel]])))[x]
    }
    plot(thk99buff, col=NA, border=NA, main="QA Plot for data loading")
    plot(HUC[HUC[[HUClevel]] %in% unique(thk99buff[[HUClevel]]),], add=T)
    plot(thk99buff, add=T, col=sapply(thk99buff$region, colF), border=NA)
    #plot(thk99buff[thk99buff@data$ORIG_FID == 1845,], add=T, col="white", border="black", lwd=3)
  }
  
  # Normalize Data ----------------------------------------------------------
  thk99buff_n = data.frame(sapply(thk99buff@data, function(x){
    if (is.numeric(x)) {
      return(scale(x))
    } else {
      return(x)
    }
  }))
  thk99buff_n$region = thk99buff$region
  thk99buff_n$logWET = thk99buff$logWET
  thk99buff_n$logPCT = thk99buff$logPCT
  thk99buff_n$WET = thk99buff$WET
  thk99buff_n$PCT = thk99buff$PCT
  thk99buff_n$ORIG_FID = thk99buff$ORIG_FID
  
  thk99buff <<- thk99buff
  thk99buff_n <<- thk99buff_n
  
  if (toFiles) {
    fileName = "thk99buff"
    outNames = c("%s.txt", "%s_n.txt", "%s.shp")
    fileNames = sapply(outNames, FUN=function(x){sprintf(x, fileName)})
    if (any(file.exists(fileNames)))
    {
      existingFiles = fileNames[file.exists(fileNames)]
      warning(sprintf("%s already exist%s", paste0(existingFiles, collapse=", "), ifelse(length(existingFiles) > 1, "", "s")))
    } else {
      write.table(thk99buff@data, fileNames[1], row.names = F, quote = F, sep = "\t")
      write.table(thk99buff_n, fileNames[2], row.names = F, quote = F, sep = "\t")
      writeOGR(thk99buff, fileNames[3], fileName, driver = "ESRI Shapefile")
    }
  }
  
  return(list(HUClevel = HUClevel))
}
