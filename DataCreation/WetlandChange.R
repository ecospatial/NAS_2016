library(rgdal)
library(sp)
library(raster)

# Configuration -----------------------------------------------------------
outputName = "WetToAny"
test = F


# Initialization ----------------------------------------------------------
dir = sprintf("C:/DATA/CCAP/T1/%s", outputName)
paluEstuDir = paste0(dir, "/PaluEstu96")
estuDir = paste0(dir, "/Estu96")


# Load CCAP Data ----------------------------------------------------------
ccapDir = "C:/DATA/CCAP/T0/Change9606"
extension = "_1996_2006_ccap_change.img"
ccap = list(
  la = raster(paste0(ccapDir,"/la",extension)),
  fl = raster(paste0(ccapDir,"/fl",extension)),
  ms = raster(paste0(ccapDir,"/ms",extension)),
  tx = raster(paste0(ccapDir,"/tx",extension)),
  al = raster(paste0(ccapDir,"/al",extension))
)
wetlandCodes = read.delim(paste0(ccapDir, "/wetlandCodes.txt")) #all changes to and from wetlands


# Test --------------------------------------------------------------------
if (test)
  {
  ccap = list(
      la = raster(nrow=5,ncol=5),
      fl = raster(nrow=5,ncol=5),
      al = raster(nrow=5,ncol=5),
      ms = raster(nrow=5,ncol=5),
      tx = raster(nrow=5,ncol=5)
    )
  ccap = lapply(ccap, function(x){
    r=raster(nrow=5,ncol=5)
    r[] = ceiling(runif(ncell(r))*500)
    x=r
  })
}


# Extract Areas with Palustrine or Estuarine Wetlands in 1996 -------------
paluEstuCodes96 = wetlandCodes[grep("Wetland", wetlandCodes$Type96),] #land types that were wetland in 96
paluEstu96 = lapply(ccap, function(x){
  return(x * x %in% paluEstuCodes96$ID)
})

# Extract Areas with only Estuarine Wetlands in 1996 ----------------------
estuCodes96 = wetlandCodes[grep("Estuarine", wetlandCodes$Type96),]
estu96 = lapply(ccap, function(x){
  return(x * x %in% estuCodes96$ID)
})


# Save Rasters ------------------------------------------------------------
if(!dir.exists(dir))
{
  dir.create(dir)
}

if(!dir.exists(paluEstuDir) || length(list.files(paluEstuDir)) == 0)
{
  dir.create(paluEstuDir)
  for(i in 1:length(paluEstu96)){
    stateName = names(paluEstu96)[i]
    writeRaster(paluEstu96[[i]], sprintf("%s/%s_paluEstu96.img", paluEstuDir, stateName), datatype="INT2U", options="COMPRESS=RLE")
  }
}else{warning("PaluEstu96 directory already exists and is not empty; files not written.")}

if(!dir.exists(estuDir) || length(list.files(estuDir)) == 0)
{
  dir.create(estuDir)
  for(i in 1:length(estu96)){
    stateName = names(estu96)[i]
    writeRaster(estu96[[i]], sprintf("%s/%s_estu96.img", estuDir, stateName), datatype="INT2U", options="COMPRESS=RLE")
  }
}else{warning("Estu96 directory already exists and is not empty; files not written.")}



# THK99 -------------------------------------------------------------------
library(RPostgreSQL)
library(postGIStools)

source("config/postgresqlcfg.R")
if(exists("user") || exists("pw")) {  
  con <- dbConnect(PostgreSQL(), dbname = db, user = user,
                   host = host, port = port,
                   password = pw)
  rm(pw);rm(user)
}

inlandbuff = get_postgis_query(con, "SELECT * FROM thkbuffers", geom_name = "geom")

dbDisconnect(con)

if(!exists("paluEstu96") || !exists("estu96"))
{
  extension = "_estu96.img"
  estu96 = list(
    la = raster(paste0(estuDir,"/la",extension)),
    fl = raster(paste0(estuDir,"/fl",extension)),
    ms = raster(paste0(estuDir,"/ms",extension)),
    tx = raster(paste0(estuDir,"/tx",extension)),
    al = raster(paste0(estuDir,"/al",extension))
  )
  
  extension = "_paluEstu96.img"
  paluEstu96 = list(
    la = raster(paste0(paluEstuDir,"/la",extension)),
    fl = raster(paste0(paluEstuDir,"/fl",extension)),
    ms = raster(paste0(paluEstuDir,"/ms",extension)),
    tx = raster(paste0(paluEstuDir,"/tx",extension)),
    al = raster(paste0(paluEstuDir,"/al",extension))
  )
}

paluEstuLoss9606 = data.frame(FID = numeric(0), wetLoss = numeric(0))
estuLoss9606 = data.frame(FID = numeric(0), wetLoss = numeric(0))

inlandbuff = spTransform(inlandbuff, proj4string(ccap$la))

source("RUtilityFunctions/getMinExtent.R")
for (state in names(ccap))
{
  e = getMinExtent(inlandbuff, ccap[[state]])
  
  cropLandPts = crop(inlandbuff, e)
  cropEstu = crop(estu96[[state]], e)
  cropPaluEstu = crop(paluEstu96[[state]], e)
  
  estuVals = extract(cropEstu, cropLandPts, fun=function(x, na.rm = na.rm){return(sum(x > 0, na.rm = na.rm))}, na.rm=TRUE)
  paluEstuVals = extract(cropPaluEstu, cropLandPts, fun=function(x, na.rm = na.rm){return(sum(x > 0, na.rm = na.rm))}, na.rm=TRUE)
  
  eVals = data.frame(FID = cropLandPts$ORIG_FID, wetLoss = estuVals)
  estuLoss9606 = rbind(estuLoss9606, eVals)
  peVals = data.frame(FID = cropLandPts$ORIG_FID, wetLoss = paluEstuVals)
  paluEstuLoss9606 = rbind(paluEstuLoss9606, peVals)
}

write.table(estuLoss9606, sprintf("%s/%s_estu.txt", dir, outputName), row.names = FALSE, quote = FALSE)
write.table(paluEstuLoss9606, sprintf("%s/%s_paluEstu.txt", dir, outputName), row.names = FALSE, quote = FALSE)

estuLoss9606 = estuLoss9606[order(estuLoss9606$FID),]
cols = colorRampPalette(c("green", "yellow", "red"))(20)
colBreaks = cut(estuLoss9606$wetLoss, c(-Inf, seq(0, max(estuLoss9606$wetLoss), length.out = 20)))
plot(inlandbuff)
plot(inlandbuff, add=T, col=cols[colBreaks], border=NA)

# r = ccap$ms
# inlandbuff = spTransform(inlandbuff, proj4string(ccap$ms))
# cropLandPts = crop(inlandbuff, r)
# val = extract(r, cropLandPts, fun=function(x, na.rm = na.rm){return(sum(x > 0, na.rm = na.rm))}, na.rm=TRUE)
