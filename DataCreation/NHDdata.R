library(rgdal)
library(raster)

# URL for NHD by Hydrological Unit, copied from the first available product at:
# http://prd-tnm.s3-website-us-west-2.amazonaws.com/?prefix=StagedProducts/Hydrography/NHD/HU4/HighResolution/
# %s is the 4-digit HUC4 code for the watershed
NHDurl = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU4/HighResolution/Shape/NHD_H_%s_HU4_Shape.zip"

destLoc = "C:/DATA/NHD/T0/NHD_H_%s_HU4_Shape.zip"

thk99buff = readOGR("C:/DATA/EarthEngine/T1/thk99buff.shp", "thk99buff")
HUC4 = readOGR("C:/DATA/HUC/HUC_shapes/WBDHU4.shp", "WBDHU4")
HUC4 = spTransform(HUC4, proj4string(thk99buff))
HUC4inPlot = crop(HUC4, extent(thk99buff))

for (HUCcode in HUC4inPlot$HUC4)
{
  download.file(sprintf(NHDurl, HUCcode), sprintf(destLoc, HUCcode))
}

