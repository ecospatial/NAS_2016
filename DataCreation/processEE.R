library(rgdal)
library(magrittr)
library(RColorBrewer)
source("convertEEkmlToShp.R")

filePath = "C:/DATA/EarthEngine/T0/thk99buff.kml"
kmlOutPath = "C:/DATA/EarthEngine/T1/thk99buff.kml"
shpOutPath = "C:/DATA/EarthEngine/T1/thk99buff.shp"
layerName = "thk99buff"


# Load THK99buff from GEE; Stop if already processed ----------------------
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

# Merge Datasets ----------------------------------------------------------
names(thk99buff@data) = gsub("systm_n", "system.index", names(thk99buff@data))

wetloss = read.csv("C:/DATA/EarthEngine/T0/thk99wetloss.csv")
names(wetloss) = gsub("count","wetpx",names(wetloss))

wetgain = read.csv("C:/DATA/EarthEngine/T0/thk99wetgain.csv")
names(wetgain) = gsub("count","gainpx",names(wetgain))

wet96 = read.csv("C:/DATA/EarthEngine/T0/thk99wet96.csv")
names(wet96) = gsub("count","wet96",names(wet96))

ndvi = read.csv("C:/DATA/EarthEngine/T0/thk99NDVI.csv")
names(ndvi) = gsub("mean","ndvi",names(ndvi))

ndmi = read.csv("C:/DATA/EarthEngine/T0/thk99NDMI.csv")
names(ndmi) = gsub("mean","ndmi",names(ndmi))

data = thk99buff@data %>%
  merge(wetloss, by="system.index", sort=F) %>%
  merge(wetgain, by="system.index", sort=F) %>%
  merge(wet96, by="system.index",sort=F) %>%
  merge(ndvi, by="system.index",sort=F) %>%
  merge(ndmi, by="system.index",sort=F)
names(data)

beforeNames = c("ORIG_FID.x", "SLOPE__","SL_MM_Y", "TIDE_M_","WAVES_M","wetpx","gainpx","wet96","ndvi","ndmi")
afterNames = c("ORIG_FID", "CS", "RSLR", "TR", "WH","WETpx","GAINpx","WET96px","NDVI","NDMI")

if (length(beforeNames) != length(afterNames)) {
  stop("ERROR: RENAME LENGTHS DO NOT MATCH, FIX!!")
}

data = data[c("ORIG_FID.x", "SLOPE__","SL_MM_Y", "TIDE_M_","WAVES_M","wetpx","gainpx","wet96","ndvi","ndmi")]
names(data) = c("ORIG_FID", "CS", "RSLR", "TR", "WH","WETpx","GAINpx","WET96px","NDVI","NDMI")
names(data)

if (all(head(data)$ORIG_FI != head(thk99buff@data)$ORIG_FI))
{
  stop("ERROR: The data is not merged properly!!!")
}


# Process Calculated Variables TODO: Move This To Separate Script ---------

# Convert wetland change pixels to hectares
pxToHa = function(x) { return(x*900/10000) }
data$WET = pxToHa(data$WETpx)
data$WET96 = pxToHa(data$WET96px)
data$GAIN = pxToHa(data$GAINpx)

# Compute log wetland loss
data$logWET = rep(NA, nrow(data))
data$logWET[data$WET > 0] = log(data$WET[data$WET > 0])

# Compute percent of wetland in 96 lost by 06
data$PCT = data$WETpx/data$WET96px
data$PCT[is.nan(data$PCT)] = NA
data$logPCT = rep(NA, nrow(data))
data$logPCT[data$PCT > 0 & !is.na(data$PCT)] = log(data$PCT[data$PCT > 0 & !is.na(data$PCT)])

# Compute total wetland change
data$CHGpx = data$GAINpx - data$WETpx
data$CHG = data$GAIN - data$WET
if (all(data$CHG == pxToHa(data$CHGpx)))
{
  stop("ERROR: Wetland change calculations wrong!!!")
}

thk99buff@data = data


# QA Plots ----------------------------------------------------------------
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


# Save Processed THK99 ----------------------------------------------------
writeOGR(thk99buff, "C:/DATA/EarthEngine/T1/thk99buff.shp","thk99buff", driver="ESRI Shapefile", overwrite_layer = T)
write.csv(data,"C:/DATA/EarthEngine/T1/fullData.csv")
