library(rgdal)
library(sp)
library(raster)

# Configuration -----------------------------------------------------------
ccapT0Dir = "C:/DATA/CCAP/T0/"
ccapT1Dir = "C:/DATA/CCAP/T1/"
extension = "_1996_ccap_land_cover.img" #"_1996_2006_ccap_change.img"
folder = "Cover96"

# Initialization ------------------------------------------------------------
ccap = list(
  la = raster(paste0(ccapT0Dir,folder,"/la",extension)),
  fl = raster(paste0(ccapT0Dir,folder,"/fl",extension)),
  ms = raster(paste0(ccapT0Dir,folder,"/ms",extension)),
  tx = raster(paste0(ccapT0Dir,folder,"/tx",extension)),
  al = raster(paste0(ccapT0Dir,folder,"/al",extension))
)

# Convert CCAP Data ----------------------------------------------------------
outDir = sprintf("%s/%s-tif", ccapT1Dir, folder)
if (!dir.exists(outDir))
{
  dir.create(outDir)
}

if (length(list.files(outDir)) < length(ccap))
{
  for(state in ccap)
  {
    writeRaster(state, sprintf("%s/%s.tif", outDir, names(state)), datatype="INT2U", format = "GTiff", options="COMPRESS=LZW")
  }
}
