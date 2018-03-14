setwd("WetlandModel/New")

library(rjags)
library(sp)
library(magrittr)
library(rgdal)
library(RPostgreSQL)
library(postGIStools)


# Database Connection and Loading -----------------------------------------
source("../../config/postgresqlcfg.R")
if(exists("user") || exists("pw")) {  
  con <- dbConnect(PostgreSQL(), dbname = db, user = user,
                   host = host, port = port,
                   password = pw)
  rm(pw);rm(user)
}

huc2 = get_postgis_query(con, "SELECT * FROM huc2 WHERE huc2.HUC2 IN ('12','13','08','03')", geom_name = "geom")

dbDisconnect(con)


# Load Local Data (TODO: Make DB) -----------------------------------------
thk99buff = readOGR("C:/DATA/EarthEngine/T1/thk99buff.kml", "thk99buff")
thk99data = read.csv("C:/DATA/EarthEngine/T1/fullData.csv")

# Combine Spatial. Geo, and Wet Data --------------------------------------
huc2 = spTransform(huc2, proj4string(thk99buff))
hucZone = over(thk99buff,huc2[,"huc2"]) #3=FL, 12=TX, 13=west TX /// 8=LA
thk99buff$HUC2 = hucZone$huc2
thk99buff$region = sapply(thk99buff$HUC2, function(x){
  if (x == "03" | x == "12" | x == "13")
    return(1)
  else
    return(2)
})

thk99buff@data = cbind(thk99buff@data, thk99data)

# Visualize
colF = function(x){
  if (x =='03' | x == '13' | x == '12')
    return("red")
  else
    return("green")
}
plot(huc2)
plot(thk99buff, add=T, col=sapply(hucZone$huc2, colF), border=NA)
plot(thk99buff[thk99buff@data$ORIG_FID == 1845,], add=T, col="white", border="black", lwd=3)

# Convert wetland change pixels to hectares and compute log change
thk99buff$WET = thk99buff$WET*900/10000
thk99buff$logWET = log(thk99buff$WET)

# Calculate squared RSLR (for non-linear)
thk99buff$RSLRsq = thk99buff$RSLR^2

# Visualize removing wetland changes of 0
plot(huc2)
plot(thk99buff, add=T, col="green", border=NA)
plot(thk99buff[thk99buff$WET > 0,], add=T, col="red", border=NA)

# Remove buffers without wetland change
thk99buff = thk99buff[thk99buff$WET > 0,]


# Create Models -----------------------------------------------------------
setwd("..")
source("createModels.R")
setwd("./New")
params = c("RSLRsq","WH","TR","CS","NDVI")
folderName = paste0(params, collapse=".")
models = createModels(params, folderName = folderName)

# Normalize Data ----------------------------------------------------------
thk99buff_n = data.frame(sapply(thk99buff@data[c(params)], function(x){scale(x)}))
thk99buff_n = cbind(thk99buff_n, region=thk99buff$region)
thk99buff_n = cbind(thk99buff_n, logWET=thk99buff$logWET)

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

source("../../RUtilityFunctions/codaSamplesDIC.R")

write.table("modelNo\tfixed\trandom\tDIC", sprintf("%s/DIC.txt", resultsDir), row.names=F, quote=F, sep="\t")

modelFiles = list.files(paste0("Models/", folderName), pattern="^\\d*.txt")

data = append(list(Nobs=nrow(thk99buff_n), Nregion=2), thk99buff_n)

for(modelFile in modelFiles)
{
  i = as.numeric(gsub("(\\d*)\\.txt", "\\1", modelFile))
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
        file = sprintf("%s/DIC.txt", resultsDir),
        append = T)
  save(output,file=sprintf("%s/%s.RData", resultsDir, i))
}

Sys.time()
