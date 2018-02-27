library(rgdal)
library(sp)
library(raster)

# Configuration -----------------------------------------------------------
outputName = "EstuToWaterAquatic"

fromRegex = "^.*(Estuarine) (?!Aquatic)"
toRegex = "^.*(Water|Aquatic)"
fromLaymen = "Land types in 1996 that are estuarine, excluding estu aquatic beds."
toLaymen = "Land types in 2006 that are open water or palu/estu aquatic beds."

test = F


# Initialization ----------------------------------------------------------
dir = sprintf("C:/DATA/CCAP/T1/%s", outputName)


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
  rowColSize = 500
  cellRange = 500
  ccap = list(
      la = raster(nrow=rowColSize,ncol=rowColSize),
      fl = raster(nrow=rowColSize,ncol=rowColSize),
      al = raster(nrow=rowColSize,ncol=rowColSize),
      ms = raster(nrow=rowColSize,ncol=rowColSize),
      tx = raster(nrow=rowColSize,ncol=rowColSize)
    )
  ccap = lapply(ccap, function(x){
    r=raster(nrow=rowColSize,ncol=rowColSize)
    r[] = ceiling(runif(ncell(r))*cellRange)
    x=r
  })
}


# Filter land change codes to types specified in config above -------------
fromCodes = wetlandCodes[grep(fromRegex, wetlandCodes$Type96, perl=T),]
toCodes = wetlandCodes[grep(toRegex, wetlandCodes$Type06, perl=T),]
changeCodes = fromCodes[fromCodes$ID %in% toCodes$ID,]

change9606 = lapply(ccap, function(x){
  return(x * x %in% changeCodes$ID)
})


# Save Rasters ------------------------------------------------------------
if(dir.exists(dir) && length(list.files(dir)) != 0)
{
  warning(paste0(outputName, " directory already exists and is not empty; files not written."))
} else {
  dir.create(dir, showWarnings = FALSE)
  for (i in 1:length(change9606))
  {
    stateName = names(change9606)[i]
    writeRaster(change9606[[i]], sprintf("%s/%s_%s.img", dir, stateName, outputName), datatype="INT2U", options="COMPRESS=RLE")
  }
}


# THK99 -------------------------------------------------------------------
library(RPostgreSQL)
library(postGIStools)

# Load THK99 from db
source("config/postgresqlcfg.R")
if(exists("user") || exists("pw")) {  
  con <- dbConnect(PostgreSQL(), dbname = db, user = user,
                   host = host, port = port,
                   password = pw)
  rm(pw);rm(user)
}
inlandbuff = get_postgis_query(con, "SELECT * FROM thkbuffers", geom_name = "geom")
dbDisconnect(con)

# Load change if not loaded from above
if(!exists("change9606"))
{
  extension = sprintf("_%s.img", outputName)
  change9606 = list(
    la = raster(paste0(dir, "/la", extension)),
    fl = raster(paste0(dir, "/fl", extension)),
    ms = raster(paste0(dir, "/ms", extension)),
    tx = raster(paste0(dir, "/tx", extension)),
    al = raster(paste0(dir, "/al", extension))
  )
}


# Extract wetland change for each polygon in THK99 buffers
inlandbuff = spTransform(inlandbuff, proj4string(ccap$al))

changeVals9606 = data.frame(FID = numeric(0), wetLoss = numeric(0))
source("RUtilityFunctions/getMinExtent.R")
for (state in names(ccap))
{
  e = getMinExtent(inlandbuff, ccap[[state]])
  
  cropLandPts = crop(inlandbuff, e)
  cropChange = crop(change9606[[state]], e)
  
  changeVals = extract(cropChange, cropLandPts, fun=function(x, na.rm = na.rm){return(sum(x > 0, na.rm = na.rm))}, na.rm=TRUE)
  
  cVals = data.frame(FID = cropLandPts$ORIG_FID, wetLoss = changeVals)
  changeVals9606 = rbind(changeVals9606, cVals)
}

# Remove duplicates from states that have overlapping extents
# Largest value is true value, as it is contained fully by one state
changeVals9606 = changeVals9606[order(changeVals9606$FID, -changeVals9606$wetLoss),] #order by FID then hi-to-lo wetLoss
changeVals9606 = changeVals9606[!duplicated(changeVals9606$FID),] #remove duplicates after first value (which is highest)

#Write
write.table(changeVals9606, sprintf("%s/%s_output.txt", dir, outputName), row.names = FALSE, quote = FALSE, sep = "\t")


# Write Metadata ----------------------------------------------------------
write(sprintf("%s
-------------------------------
Time: %s

Change Types:
-------------------------------
From:
\t%s
\t%s
\t\t%s

To Regex:
\t%s
\t%s
\t\t%s

Codes:
-------------------------------",
    outputName,
    Sys.time(),
    fromRegex,
    fromLaymen,
    paste(unique(changeCodes$Type96), collapse="\n\t\t"),
    toRegex,
    toLaymen,
    paste(unique(changeCodes$Type06), collapse="\n\t\t")),
  
  sprintf("%s/%s_metadata.txt", dir, outputName))

write.table(changeCodes, sprintf("%s/%s_metadata.txt", dir, outputName), quote =FALSE, row.names = FALSE, append = TRUE, sep = "\t")

