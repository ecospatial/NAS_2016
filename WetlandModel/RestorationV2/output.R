library(rgdal)
library(rgeos)

setwd("WetlandModel/RestorationV2")
source("../../RUtilityFunctions/plotRegions.R")

thk99buff = readOGR("C:/DATA/EarthEngine/T1/thk99buff.shp", "thk99buff")
thk99buff = thk99buff[,-match(c("GAIN","GAINpx","CHGpx","CHG"),names(thk99buff))]

# Remove barrier islands
shoreline = readOGR("C:/Users/GCRLWuHardy/Documents/General Maps/Coastlines/USCoast_h_L1.shp", "USCoast_h_L1")
shoreline = spTransform(shoreline, proj4string(thk99buff))
shoreline = crop(shoreline, thk99buff)
thk99buff = thk99buff[!is.na(over(thk99buff, geometry(shoreline))),]

# Extract HUC and region to each buffer
HUC = readOGR("C:/DATA/HUC/HUC_shapes/WBDHU4.shp", "WBDHU4")
HUC = spTransform(HUC, proj4string(thk99buff))
hucZone = over(thk99buff, HUC[,"HUC4"])
thk99buff$HUC4 = factor(hucZone$HUC4)
thk99buff$region = as.numeric(thk99buff$HUC4)


# Load and Process Restoration Data
source("prepRestoreData.R")
restore = readOGR("C:/DATA/CPRA_Projects/T1/CPRA_PolysYears.shp", "CPRA_PolysYears")
restore = restore[restore$YEAR <= 2005 & restore$YEAR > 1900,]
removal = order(restore$ACRES, decreasing=TRUE)[1:4]
restore = restore[-removal,]
restore = spTransform(restore, proj4string(thk99buff))
restore$PROJ_TYPE = as.character(restore$PROJ_TYPE)

# Recode mis-coded/missing projects
miscodes = read.delim("recodesV2.txt", sep="\t", stringsAsFactors = F)
for (i in 1:nrow(miscodes))
{
  id = miscodes[i,]$PROJ_ID
  if (miscodes[i,]$RECODE != "")
    restore@data[restore@data$PROJ_ID == id,]$PROJ_TYPE = miscodes[i,]$RECODE
}

restoreRecode = list( #SP and BH should always be recoded because they describe the goal, not the methods.
  # Hydrologic alteration
  HA = c("HA", "FD", "HR", "SD"),
  FD = "FD",
  HR = "HR",
  SD = "SD",
  # Marsh creation
  MC = c("MC", "TE"),
  # Breakwaters
  BW = c("BW"),
  #Vegetative Planting
  VP = c("VP","PL")
)

for (type in names(restoreRecode))
{
  thk99buff[[type]] = rep(-1, nrow(thk99buff))
}
thk99buff$YEAR = rep(-1, nrow(thk99buff))
thk99buff$RESTORE = rep(-1, nrow(thk99buff))

restorePerBuffer=over(thk99buff, restore, returnList=T)
for (bufferNo in 1:length(restorePerBuffer))
{
  buffer = restorePerBuffer[[bufferNo]]
  if (nrow(buffer) > 0) #Buffer has restoration projects
  {
    for (i in 1:nrow(buffer))
    {
      projTypes = unlist(strsplit(buffer[i,]$PROJ_TYPE, '/'))
      if (is.null(projTypes))
        projTypes = buffer[i,]$PROJ_TYPE
    }
    for (type in names(restoreRecode))
    {
      if (any(restoreRecode[[type]] %in% projTypes))
      {
        thk99buff@data[bufferNo,][[type]] = 1
      }
      else
      {
        thk99buff@data[bufferNo,][[type]] = 0
      }
    }
    thk99buff@data[bufferNo,]$YEAR = 2006 - min(buffer$YEAR)
    thk99buff@data[bufferNo,]$RESTORE = 1
  } else {
    for (type in names(restoreRecode))
    {
      thk99buff@data[bufferNo,][[type]] = 0
    }
    thk99buff@data[bufferNo,]$YEAR = 0
    thk99buff@data[bufferNo,]$RESTORE = 0
  }
}
plotRegions(states=T, clipRegions=c(10,11))
points(coordinates(thk99buff),col="black",pch=19)
plot(restore,add=T,border=NA,col="#00FF0033")
points(coordinates(thk99buff[thk99buff$RESTORE == 1,]),col="red",pch=19)
points(coordinates(thk99buff[thk99buff$HA == 1,]),col="blue",pch=19)

thk99buff_la = thk99buff[thk99buff$region %in% c(10,11),]
thk99buff_la$region = thk99buff_la$region-9

write.table(thk99buff_la[thk99buff_la$WET > 0,], "C:/DATA/CPRA_Projects/T2/THK99larestore.txt")

writeOGR(thk99buff_la[thk99buff_la$WET > 0,], "C:/DATA/CPRA_Projects/T2/THK99larestore.shp", "THK99larestore", driver="ESRI Shapefile")
