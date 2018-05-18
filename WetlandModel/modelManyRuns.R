setwd("WetlandModel/")

library(rjags)
library(sp)
library(magrittr)
library(raster)
library(rgdal)
library(rgeos)
library(RPostgreSQL)
library(postGIStools)
source("../RUtilityFunctions/createModels.R")
source("../RUtilityFunctions/codaSamplesDIC.R")


runModel = function(modParams, getData = F)
{
  # CONFIG ------------------------------------------------------------------
  vegIdx = modParams$vegIdx
  params = c("RSLR","WH","TR","CS", vegIdx)
  response = modParams$response
  regions = modParams$regions
  barrierIslands = F #Include barrier islands
  
  randomIntercept = modParams$intercept
  properRandomEffects = T
  
  phoneNotifications = !getData
  
  
  if (phoneNotifications)
  {
    library(RPushbullet)
    pbPost("note", "NAS_2016", sprintf("Starting data prep for a %s region model with parameters %s, response %s, and %srandom intercept.", regions, paste(params,collapse=","), response, if (!randomIntercept) "no" else ""))
  }
  
  # # Database Connection and Loading -----------------------------------------
  # source("../../config/postgresqlcfg.R")
  # if(exists("user") || exists("pw")) {  
  #   con <- dbConnect(PostgreSQL(), dbname = db, user = user,
  #                    host = host, port = port,
  #                    password = pw)
  #   rm(pw);rm(user)
  # }
  # 
  # huc2 = get_postgis_query(con, "SELECT * FROM huc2 WHERE huc2.HUC2 IN ('12','13','08','03')", geom_name = "geom")
  # 
  # dbDisconnect(con)
  
  
  # Load Local Data (TODO: Make DB) -----------------------------------------
  if (regions == "2" | regions == "3")
  {
    HUClevel = "HUC2"
  } else if (regions == "ALL") {
    HUClevel = "HUC4"
  } else {
    if (phoneNotifications){pbPost("note", "NAS_2016", "MODEL RUN ERROR: Unsupported number of regions.")}
    stop("UNSUPPORTED NUMBER OF REGIONS: Use either ALL (HUC4), 2 or 3 (HUC2).")
  }
  HUCfilename = gsub("HUC(\\d*)", "WBDHU\\1", HUClevel)
  HUC = readOGR(sprintf("C:/DATA/HUC/HUC_shapes/%s.shp", HUCfilename), HUCfilename)
  
  thk99buff = readOGR("C:/DATA/EarthEngine/T1/thk99buff.shp", "thk99buff")
  
  # # Visualize removing wetland changes of 0
  # plot(thk99buff, col=NA, border=NA)
  # plot(huc4, add=T)
  # plot(thk99buff, add=T, col="green", border=NA)
  # plot(thk99buff[thk99buff$WET > 0,], add=T, col="red", border=NA)
  
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
  colF = function(x){
    rainbow(length(unique(thk99buff[[HUClevel]])))[x]
  }
  plot(thk99buff, col=NA, border=NA)
  plot(HUC[HUC[[HUClevel]] %in% unique(thk99buff[[HUClevel]]),], add=T)
  plot(thk99buff, add=T, col=sapply(thk99buff$region, colF), border=NA)
  #plot(thk99buff[thk99buff@data$ORIG_FID == 1845,], add=T, col="white", border="black", lwd=3)
  
  # Normalize Data ----------------------------------------------------------
  thk99buff_n = data.frame(sapply(thk99buff@data[c(params)], function(x){scale(x)}))
  thk99buff_n = cbind(thk99buff_n, region=thk99buff$region)
  thk99buff_n = cbind(thk99buff_n, logWET=thk99buff$logWET)
  thk99buff_n = cbind(thk99buff_n, logPCT=thk99buff$logPCT)
  thk99buff_n = cbind(thk99buff_n, WET=thk99buff$WET)
  thk99buff_n = cbind(thk99buff_n, PCT=thk99buff$PCT)
  
  tryCatch({
    is.null(thk99buff_n[response])
  }, error= function(e){
    if (phoneNotifications){pbPost("note", "NAS_2016", "MODEL RUN ERROR: Response not included in data.")}
    stop("RESPONSE NOT INCLUDED IN DATA, SEE 'Normalize Data' SECTION IN CODE")
  })
  
  
  # Arrange Data for JAGS ---------------------------------------------------
  regions = length(unique(thk99buff_n$region))
  data = append(list(Nobs=nrow(thk99buff_n), Nregion=regions), thk99buff_n)
  
  if (getData)
  {
    return(data)
  }
  
  # Create Models -----------------------------------------------------------
  folderName = sprintf("%s-%sR-%s", response, regions, vegIdx)
  if (barrierIslands)
  {
    folderName = paste0(folderName, "-BaIs")
  }
  if (randomIntercept)
  {
    folderName = paste0(folderName, "-rB0")
  }
  if (!properRandomEffects)
  {
    folderName = paste0("FRE)", folderName)
  }
  models = createModels(response, params, randomIntercept, folderName, properRandomEffects = properRandomEffects)
  
  # Run Each Model in JAGS --------------------------------------------------
  if (!dir.exists("Results"))
  {
    dir.create("Results")
  }
  
  resultsDir = sprintf("Results/%s", folderName)
  if (!dir.exists(resultsDir))
  {
    dir.create(resultsDir)
  } 
  
  write.table("modelNo\tfixed\trandom\tDIC", sprintf("%s/DIC_%s.txt", resultsDir, folderName), row.names=F, quote=F, sep="\t")
  
  modelFiles = list.files(paste0("Models/", folderName), pattern="^\\d*.txt")
  
  if (phoneNotifications){pbPost("note", "NAS_2016", sprintf("Started running a %s region model with parameters %s, response %s, and %srandom intercept.", regions, paste(params,collapse=","), response, if (!randomIntercept) "no " else ""))}
  Sys.time()
  for(modelFile in modelFiles)
  {
    i = as.numeric(gsub("(\\d*)\\.txt", "\\1", modelFile))
    
    if (file.exists(sprintf("%s/%s.RData", resultsDir, i)))
    {
      print(sprintf("Skipping model %s; it already has been ran", i))
      next()
    }
    
    model = jags.model(sprintf("Models/%s/%s.txt", folderName, i),
                       data = data,
                       n.chains=3,
                       n.adapt=2000)
    
    output = coda.samples.dic(model = model,
                              variable.names=c("b0", paste0("b", params)),
                              n.iter=20000,
                              thin=1)
    
    fixed = paste(na.omit(models[i,1:length(params)]),collapse=",")
    random = paste(na.omit(models[i,(length(params)+1):(length(params)*2)]),collapse=",")
    
    write(sprintf("%s\t%s\t%s\t%s", i, fixed, random, output$dic$deviance + output$dic$penalty),
          file = sprintf("%s/DIC_%s.txt", resultsDir, folderName),
          append = T)
    save(output,file=sprintf("%s/%s.RData", resultsDir, i))
  }
  Sys.time()
  if (phoneNotifications){pbPost("note", "NAS_2016", "MODEL RUN COMPLETE!!!")}
  
}


#response, regions, vegidx, randomIntercept

responses = c("logWET", "logPCT")
vegIdxs = c("NDMI")
regionses = c("ALL", "3")
intercepts = c(T, F)

l = list(response=responses, regions=regionses, vegIdx=vegIdxs, intercept=intercepts)

combos = as.data.frame(do.call(expand.grid, l))
combos$response = as.character(combos$response)
combos$vegIdx = as.character(combos$vegIdx)
combos$region = as.character(combos$region)

for(i in 1:nrow(combos))
{
  runModel(c(combos[i,]))
}

