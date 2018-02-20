setwd("WetlandModel")

library(rjags)
library(sp)
library(magrittr)
library(rgdal)
source("../RUtilityFunctions/codaSamplesDIC.R")

library(RPostgreSQL)
library(postGIStools)


# Database Connection and Loading -----------------------------------------
source("../config/postgresqlcfg.R")
if(exists("user") || exists("pw")) {  
  con <- dbConnect(PostgreSQL(), dbname = db, user = user,
                   host = host, port = port,
                   password = pw)
  rm(pw);rm(user)
}

wetloss = get_postgis_query(con, 'SELECT * FROM wetloss')
inlandbuff = get_postgis_query(con, "SELECT * FROM thkbuffers", geom_name = "geom")
inlandbuff@data = merge(inlandbuff@data, wetloss, by = "ORIG_FID")
#huc2 = get_postgis_query(con, "SELECT * FROM huc2", geom_name = "geom")

dbDisconnect(con)


# Combine Spatial. Geo, and Wet Data --------------------------------------
huc2AvgDis = readOGR("huc2AvgDis.shp", "huc2AvgDis")
huc2AvgDis = spTransform(huc2AvgDis, proj4string(inlandbuff))

hucZone=over(inlandbuff,huc2AvgDis[,"HUC2"]) #3=FL, 12=TX, 8=LA
hucZone$ORIG_FID=seq(0:(nrow(hucZone)-1))

dat=read.delim("../wetlandLossData.txt")
dat = merge(dat, hucZone, by="ORIG_FID")
dat$region = rep(2,nrow(dat))
dat[dat$HUC2 == "03" | dat$HUC2 == "12",]$region = 1

dat$WET = dat$WET*900/10000
dat$logWET = log(dat$WET)

data=list(Nobs=nrow(dat), Nregion=2)


# Create Models -----------------------------------------------------------
source("createModels.R")
params = c("RSLR","WH","TR","CS","NDMI")
response = "WET"
folderName = paste0(params, collapse=".")
createModels(params, folderName = folderName)

# Normalize Data ----------------------------------------------------------
dat_n = data.frame(sapply(dat[c(response, params)], function(x){scale(x)}))

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

write.table("modelNo\tfixed\trandom\tDIC", sprintf("%s/DIC.txt", resultsDir), row.names=F, quote=F, sep="\t")

modelFiles = list.files(paste0("Models/", folderName), pattern="^\\d*.txt")

for(modelFile in modelFiles)
{
  i = as.numeric(gsub("(\\d*)\\.txt", "\\1", modelFile))
  model = jags.model(sprintf("Models/%s/%s.txt", folderName, i),
                     data = append(data, dat),
                     n.chains=3,
                     n.adapt=2000)
  
  output = coda.samples.dic(model = model,
                          variable.names=c("b0", "bCS", "bWH", "bTR", "bRSLR", "bNDMI"),
                          n.iter=20000,
                          thin=1)
  
  fixed = paste(na.omit(models[i,1:length(params)]),collapse=",")
  random = paste(na.omit(models[i,(length(params)+1):(length(params)*2)]),collapse=",")
  
  write(sprintf("%s\t%s\t%s\t%s", i, fixed, random, output$dic$deviance + output$dic$penalty),
        file = sprintf("%s/DIC.txt", resultsDir),
        append = T)
  save(output,file=sprintf("%s/%s.RData", resultsDir, i))
}
