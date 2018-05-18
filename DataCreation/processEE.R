library(rgdal)
library(magrittr)
source("convertEEkmlToShp.R")

filePath = "C:/DATA/EarthEngine/T0/thk99buff.kml"
kmlOutPath = "C:/DATA/EarthEngine/T1/thk99buff.kml"
shpOutPath = "C:/DATA/EarthEngine/T1/thk99buff.shp"
layerName = "thk99buff"

if (file.exists(shpOutPath)) {
  print("loading thk99buff.shp")
  thk99buff = readOGR(shpOutPath, layerName)
  if (!is.null(thk99buff[["logPCT"]]))
  {
    stop("thk99buff.shp is already processed.")
  }
} else {
  print("thk99buff.shp not found... converting thk99buff.kml to shp")
  convertEEkmlToShp(filePath, kmlOutPath, shpOutPath, layerName)
  print("thk99buff.kml converted to thk99buff.shp!")
  thk99buff = readOGR(shpOutPath, layerName)
}

names(thk99buff@data) = gsub("systm_n", "system.index", names(thk99buff@data))

wetloss = read.csv("C:/DATA/EarthEngine/T0/thk99wetloss.csv")
names(wetloss) = gsub("count","WET",names(wetloss))

wet96 = read.csv("C:/DATA/EarthEngine/T0/thk99wet96.csv")
names(wet96) = gsub("count","WET96",names(wet96))

ndvi = read.csv("C:/DATA/EarthEngine/T0/thk99NDVI.csv")
names(ndvi) = gsub("mean","ndvi",names(ndvi))

ndmi = read.csv("C:/DATA/EarthEngine/T0/thk99NDMI.csv")
names(ndmi) = gsub("mean","ndmi",names(ndmi))

data = thk99buff@data %>%
  merge(wetloss, by="system.index", sort=F) %>%
  merge(wet96, by="system.index",sort=F) %>%
  merge(ndvi, by="system.index",sort=F) %>%
  merge(ndmi, by="system.index",sort=F)
names(data)
data = data[c("ORIG_FID.x", "SLOPE__","SL_MM_Y", "TIDE_M_","WAVES_M","WET","WET96", "ndvi","ndmi")]
names(data) = c("ORIG_FID", "CS", "RSLR", "TR", "WH", "WETpx", "WET96px", "NDVI","NDMI")
names(data)

if (all(head(data)$ORIG_FI != head(thk99buff@data)$ORIG_FI))
{
  stop("ERROR: The data is not merged properly!!!")
}

# Convert wetland change pixels to hectares and compute log change
data$WET = data$WETpx*900/10000
data$logWET = rep(NA, nrow(data))
data$logWET[data$WET > 0] = log(data$WET[data$WET > 0])

data$PCT = data$WETpx/data$WET96px
data$PCT[is.nan(data$PCT)] = NA
data$logPCT = rep(NA, nrow(data))
data$logPCT[data$PCT > 0 & !is.na(data$PCT)] = log(data$PCT[data$PCT > 0 & !is.na(data$PCT)])

thk99buff@data = data

op=par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(thk99buff, col=NA, border=NA)
points(coordinates(thk99buff), bg="GRAY", pch=21, cex=thk99buff$RSLR/10)
title("RSLR", line=-1)

plot(thk99buff, col=NA, border=NA)
points(coordinates(thk99buff), bg="YELLOW", pch=21, cex=thk99buff$WET/20)
title("Wetland Loss", line=-1)

# plot(thk99buff, col=NA, border=NA)
# points(coordinates(thk99buff), bg="YELLOW", pch=21, cex=(thk99buff$logWET+10)/10)

plot(thk99buff, col=NA, border=NA)
points(coordinates(thk99buff), bg="BLUE", pch=21, cex=thk99buff$PCT*2)
title("% Wetland Loss", line=-1)

plot(thk99buff, col=NA, border=NA)
points(coordinates(thk99buff), bg="GREEN", pch=21, cex=scale(thk99buff$NDVI)+2)
title("NDVI", line=-1)

# plot(thk99buff, col=NA, border=NA)
# points(coordinates(thk99buff), bg="BLUE", pch=21, cex=(thk99buff$logPCT+10)/10)
par(op)

writeOGR(thk99buff, "C:/DATA/EarthEngine/T1/thk99buff.shp","thk99buff", driver="ESRI Shapefile", overwrite_layer = T)

write.csv(data,"C:/DATA/EarthEngine/T1/fullData.csv")



# plot(WET~RSLR, data=data)
# plot(WET~NDVI, data=data, ylim=c(0,500),xlim=c(-1,1))
# plot(WET~NDMI, data=data, ylim=c(0,500),xlim=c(-1,1))
# plot(logWET~NDVI, data=data)
# 
# plot(data, pch=".")
# 
# plot(NDVI~NDMI, data=data,xlim=c(-1,1),ylim=c(-1,1))
# lm1 = lm(NDVI~NDMI, data=data)
# abline(lm1)
# 
# plot(WET~NDVI, data=data)
# plot(WET~NDMI, data=data)
# 
# plot(logWET~NDVI, data=data)
# plot(logWET~NDMI, data=data)
# 
# 
# plot(logWET~NDVI:RSLR, data=data)
# plot(logWET~NDMI:RSLR, data=data)
