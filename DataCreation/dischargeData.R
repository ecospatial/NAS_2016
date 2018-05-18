library(dataRetrieval)
library(rgdal)
library(magrittr)
library(dplyr)
library(raster)


# CONFIG ------------------------------------------------------------------
siteOutput = "C:/DATA/Discharge/ngomSites.txt"
colDataOutput = "C:/DATA/Discharge/colData.txt"
dataOutput = "C:/DATA/Discharge/ngomDischarge2.txt"
outputDir = "C:/DATA/Discharge/T0/"

batchAmount = 100
pCode = "00060"

years = 1996:2003
startMonth = 6
endMonth = 8
endDays = 31

# Load Data ---------------------------------------------------------------

# Read in shapes
THK99 = readOGR("C:/DATA/EarthEngine/T1/thk99buff.shp", "thk99buff")

HUC4 = readOGR("C:/DATA/HUC/HUC_shapes/WBDHU4.shp", "WBDHU4")
HUC4 = spTransform(HUC4, proj4string(THK99))

# Crop HUC4 to study area
HUC4 = HUC4[HUC4$HUC4 %in% crop(HUC4, extent(THK99))$HUC4,]

# Select only HUC that contain THK
HUC4 = HUC4[!is.na(over(HUC4, geometry(THK99))),]


# Grab Sites --------------------------------------------------------------

# Extract site metadata in each HUC

# if (file.exists(siteOutput))
# {
#   colData = scan(colDataOutput, character(), sep = "\t")
#   dischargeSites = read.delim(siteOutput, sep="\t", colClasses = colData)
#   coordinates(dischargeSites) = ~dec_long_va+dec_lat_va
#   proj4string(dischargeSites) = proj4string(THK99)
# } else {
  sites = list()
  for (i in 1:nrow(HUC4))
  {
    huc = HUC4[i,]
    hucId = as.character(huc$HUC4)
    ex = extent(huc)
    box = sprintf("%.6f,%.6f,%.6f,%.6f", ex@xmin, ex@ymin, ex@xmax, ex@ymax)
  
    tryCatch({
      siteData = whatNWISsites(bBox = box,
          agencyCd = "USGS",
          parameterCd="00060",
          hasDataTypeCd="dv"
      )
      sites[[hucId]] = siteData
    },
    error = function(err){
      sites[[hucId]] = "ERROR"
    })
  }
  
  # Combine site data for all HUCs
  dischargeSites = as.data.frame(sites[[1]])
  for (i in 2:length(sites))
  {
    dischargeSites = rbind(dischargeSites, sites[[i]])
  }
  
  
  # Crop Sites to only Contained by HUC
  coordinates(dischargeSites) = ~dec_long_va+dec_lat_va
  proj4string(dischargeSites) = proj4string(THK99)
  
  dischargeSites = dischargeSites[!is.na(over(dischargeSites, geometry(HUC4))),]
  
  # Write Sites to file
  write.table(dischargeSites, siteOutput, sep = "\t", row.names = F, quote = F)
  
  # Save column type data
  # write(paste0(unlist(sapply(dischargeSites@data, class)), collapse = "\t"), colDataOutput)
# }

# Sites QA Plot
plot(HUC4, axes=T)
plot(THK99, add=T, col="BLUE")
points(dischargeSites)
plot(HUC4, add=T, border="RED")


# Grab Water Data ---------------------------------------------------------
# dischargeSites$site_no = sprintf("%08d", dischargeSites$site_no) # Ensure that site ids are 8 in length (add leading)

batches = ceiling(nrow(dischargeSites)/batchAmount)
print(sprintf("Extracting water data (%s batches)", batches))
for (i in 1:batches)
{
  print(sprintf("Starting batch %s/%s (%.0f%%)...", i, batches, (i-1)/batches*100))
  nStart = 1 + (i - 1) * batchAmount
  nEnd = nStart + (batchAmount - 1)
  if (nEnd > nrow(dischargeSites))
  {
    nEnd = nrow(dischargeSites)
  }
  for (y in years)
  {
    startDate = sprintf("%s-%s-%s", y, startMonth, 01)
    endDate = sprintf("%s-%s-%s", y, endMonth, endDays)
    
    d = readNWISuv(siteNumbers = dischargeSites[nStart:nEnd,]$site_no,
         parameterCd = pCode,
         startDate = startDate,
         endDate = endDate) %>%
      renameNWISColumns() %>%
      dplyr::select(site_no, dateTime, Flow_Inst)
    
    fileName = sprintf("%s/discharge%s.txt", outputDir, y)
    write.table(d, fileName, row.names = F, quote = F, sep = "\t", col.names = !file.exists(fileName), append = file.exists(fileName))
  }
  print(sprintf("Finished batch %s/%s (%.0f%%)", i, batches, i/batches*100))
}
