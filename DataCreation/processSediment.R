library(dplyr)
library(rgdal)
library(raster)
rm(list=ls())

dataFolder = "C:/DATA/Sediment/T0"
outputFolder = "C:/DATA/Sediment/T1"
states = c("FL", "MS", "LA", "TX")

for(state in states) {
  folder = sprintf("%s/sediment%s9606", dataFolder, state)
  
  siteData = read.delim(paste(folder, "discrete_sites.tsv", sep="/"), skip = 57, header = T, stringsAsFactors = F)
  recordData = read.delim(paste(folder, "discrete_data.tsv", sep="/"), skip = 52, header = T, stringsAsFactors = F)
  
  if (!exists("allSiteData")){
    allSiteData = siteData
  } else {
    allSiteData = rbind(allSiteData, siteData)
  }
  if (!exists("allRecordData")){
    allRecordData = recordData
  } else {
    allRecordData = rbind(allRecordData, recordData)
  }
}
names(allRecordData) = toupper(names(allRecordData))

allRecordData = allRecordData %>%
  mutate(YEAR=substr(DATETIME, 1, 4)) %>%
  merge(., allSiteData, by="SITE_NO")

siteDataByYear = allRecordData %>%
  group_by(SITE_NO, YEAR) %>%
  summarise(MEAN_SSC=mean(as.numeric(SSC), na.rm=T)) %>%
  merge(., allSiteData, by="SITE_NO")

plot(allRecordData$LATITUDE~allRecordData$LONGITUDE)
extent = par("usr")
points(LATITUDE~LONGITUDE,
       data=siteDataByYear,
       xlim=extent[1:2], ylim=extent[3:4], pch=18, col='blue', cex=siteDataByYear$MEAN_SSC/1000)


thk99 = readOGR("C:/DATA/EarthEngine/T1/thk99buff.shp", "thk99buff")
thk99buffered = 

plot(thk99)