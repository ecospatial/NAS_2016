setwd("WetlandModel/TrueRandEffs")

library(rjags)
library(sp)
library(magrittr)
library(rgdal)
library(RPostgreSQL)
library(postGIStools)
source("../../createModels.R")
source("../../RUtilityFunctions/codaSamplesDIC.R")


# CONFIG ------------------------------------------------------------------
params = c("RSLR","WH","TR","CS","NDVI")
response = "logPCT"
randomIntercept = F
properRandomEffects = T


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
huc4 = readOGR("C:/DATA/HUC/HUC_shapes/WBDHU4.shp", "WBDHU4")
thk99buff = readOGR("C:/DATA/EarthEngine/T1/thk99buff.kml", "thk99buff")
thk99data = read.csv("C:/DATA/EarthEngine/T1/fullData.csv")

# Combine Spatial. Geo, and Wet Data --------------------------------------
huc4 = spTransform(huc4, proj4string(thk99buff))
hucZone = over(thk99buff,huc4[,"HUC4"])
thk99buff$HUC4 = factor(hucZone$HUC4)

thk99buff@data = cbind(thk99buff@data, thk99data)

# Convert wetland change pixels to hectares and compute log change
thk99buff$WET = thk99buff$WET*900/10000
thk99buff$logWET = log(thk99buff$WET)

# Calculate squared WH (for non-linear)
thk99buff$WHsq = thk99buff$WH^2

# # Visualize removing wetland changes of 0
# plot(thk99buff, col=NA, border=NA)
# plot(huc4, add=T)
# plot(thk99buff, add=T, col="green", border=NA)
# plot(thk99buff[thk99buff$WET > 0,], add=T, col="red", border=NA)

# Remove buffers without wetland change
thk99buff = thk99buff[thk99buff$WET > 0,]

# Normalize Data ----------------------------------------------------------
thk99buff_n = data.frame(sapply(thk99buff@data[c(params)], function(x){scale(x)}))
thk99buff_n = cbind(thk99buff_n, HUC4=thk99buff$HUC4)
thk99buff_n = cbind(thk99buff_n, logWET=thk99buff$logWET)
thk99buff_n = cbind(thk99buff_n, logPCT=thk99buff$logPCT)

tryCatch({
  is.null(thk99buff_n[response])
}, error= function(e){
  stop("RESPONSE NOT INCLUDED IN DATA, SEE 'Normalize Data' SECTION IN CODE")
})


# Arrange Data for JAGS ---------------------------------------------------
thk99buff_n$region = factor(thk99buff_n$HUC4)
thk99buff$region = thk99buff_n$region
levels(thk99buff_n$region) = 1:length(unique(thk99buff_n$region))
thk99buff_n$region = as.numeric(thk99buff_n$region)
regions = length(unique(thk99buff_n$region))

# Visualize
# colF = function(x){
#   rainbow(16)[x]
# }
# plot(thk99buff, col=NA, border=NA)
# plot(huc4[huc4$HUC4 %in% unique(thk99buff$HUC4),], add=T)
# plot(thk99buff, add=T, col=sapply(thk99buff$region, colF), border=NA)
# #plot(thk99buff[thk99buff@data$ORIG_FID == 1845,], add=T, col="white", border="black", lwd=3)

data = append(list(Nobs=nrow(thk99buff_n), Nregion=regions), thk99buff_n)

# Create Models -----------------------------------------------------------
folderName = sprintf("%s~%s", response, paste0(params, collapse="."))
if (regions > 2)
{
  folderName = paste0(folderName, sprintf("-%sRegions", regions))
}
if (randomIntercept)
{
  folderName = paste0(folderName, "-rB0")
}
if (properRandomEffects)
{
  folderName = paste0("TRE)", folderName)
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