library(rgdal)
library(magrittr)
library(dplyr)
library(raster)

# Load Data ---------------------------------------------------------------

# Read in shapes
thk99buff = readOGR("C:/DATA/EarthEngine/T1/thk99buff.shp", "thk99buff")

HUC4 = readOGR("C:/DATA/HUC/HUC_shapes/WBDHU4.shp", "WBDHU4")
HUC4 = spTransform(HUC4, proj4string(thk99buff))

# Crop HUC4 to study area
HUC4 = HUC4[HUC4$HUC4 %in% crop(HUC4, extent(thk99buff))$HUC4,]

# Select only HUC that contain THK
HUC4 = HUC4[!is.na(over(HUC4, geometry(thk99buff))),]

fixSiteId = function(x){
  l=sapply(x$site_no, FUN=function(v){return(nchar(v))}) # Fix site id
  x[l <= 8, ]$site_no = sprintf("%08d", x[l <= 8, ]$site_no) # Fix site id
  x$site_no = as.character(x$site_no) # Fix site id
  
  return(x)
}
# Read in discharge data
# d = read.delim("C:/DATA/Discharge/ngomDischarge96.txt", sep = "\t")
for (file in list.files("C:/DATA/Discharge/T0/", full.names = T))
{
  f = read.delim(file, sep="\t")
  f = fixSiteId(f)
  if (exists("d"))
  {
    d = bind_rows(d, f)
  } else {
    d = f
  }
}
# d = fixSiteId(d)

# Read in discharge site metadata
dischargeSites = read.delim("C:/DATA/Discharge/ngomSites.txt", sep = "\t")
dischargeSites = fixSiteId(dischargeSites)



z = d %>%
  group_by(site_no) %>%
  #summarize(mean=mean(X_00060_00000), sd=sd(X_00060_00000))%>%
  summarize(mean=mean(Flow_Inst), sd=sd(Flow_Inst))%>%
  as.data.frame()# %>%
  #fixSiteId()

siteDisch = merge(z, dischargeSites, by = "site_no")
siteDisch = na.omit(siteDisch)

coordinates(siteDisch) = ~dec_long_va+dec_lat_va
proj4string(siteDisch) = proj4string(thk99buff)
siteDisch$mean_n = scale(siteDisch$mean)
siteDisch$mean_n = siteDisch$mean_n + (-min(siteDisch$mean_n))

siteDisch$log_mean = log(siteDisch$mean)
siteDisch$log_mean_n = scale(siteDisch$mean)
siteDisch$log_mean_n = siteDisch$log_mean_n + (-min(siteDisch$log_mean_n))

plot(thk99buff)
points(siteDisch, pch=20, cex=siteDisch$log_mean/4, col="blue")
plot(HUC4, add=T, border="RED")


HUC4$avgdis = over(HUC4, siteDisch, fn = mean)$mean

library(RColorBrewer)
library(classInt)
library(sp)
pal = brewer.pal(9, "OrRd")
breaks = classIntervals(HUC4$avgdis, n = 9, style = "quantile")$brks
brknDis = cut(HUC4$avgdis, breaks)
cols = pal[brknDis]
cols[HUC4$avgdis == breaks[1]] = pal[1]
cols[is.na(cols)] = "gray"
plot(HUC4, col=cols)

HUC4[c("HUC4", "NAME", "avgdis")]@data

write.table(HUC4[c("HUC4", "NAME", "avgdis")]@data, "dischargePerHUC.txt", sep ="\t", row.names = F, quote = F)
