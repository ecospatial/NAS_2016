
library(rgdal)
library(sp)
library(magrittr)
library(dplyr)


# CONFIG ------------------------------------------------------------------
restoreT0file = "C:/DATA/CPRA_Projects/T0/shapefile/CPRA_Polys.shp"
restoreT1outputName = "CPRA_PolysYears"
restoreT1updated = sprintf("C:/DATA/CPRA_Projects/T1/%s.shp", restoreT1outputName)
restoreT1undated = "C:/DATA/CPRA_Projects/T1/undatedRestores.txt"
restoreT1manualDated = "C:/DATA/CPRA_Projects/T1/manualDatedRestores.txt"

# Restore Types -----------------------------------------------------------
typeCodes = c("BH","FD","HP","HR","IN","MC","OT","OR","RR","SD","SP","VP","TE")
typeDescriptions = c("Barrier Island/Headland Restoration",
                     "Freshwater Diversion",
                     "Hurricane Protection",
                     "Hydrologic Restoration",
                     "Infrastructure",
                     "Marsh Creation",
                     "Other",
                     "Oyster Barrier Reef",
                     "Ridge Restoration",
                     "Sediment Diversion ",
                     "Shoreline Protection",
                     "Vegetative Planting",
                     "Terracing")
restoreTypes = data.frame(CODE=typeCodes, NAME=typeDescriptions)

# Load data ---------------------------------------------------------------

# Load restore data
restore = readOGR(restoreT0file, "CPRA_Polys", stringsAsFactors = F)
restore$YEAR = as.numeric(restore$YEAR) # Convert year to numeric

# Pull data from master report
tabData = read.delim("C:/DATA/CPRA_Projects/T0/tabula-A1_FINAL_03.30.2017.txt", sep="\t", stringsAsFactors = F)
tabData = tabData[tabData$Project.ID != "Project ID",]
tabData$Last.Year.of.Construction[tabData$Last.Year.of.Construction == "Pending"] = -999 # Code pending as -999
tabData$Last.Year.of.Construction[tabData$Last.Year.of.Construction == "Unknown"] = -888 # Code unknown as -888
tabData$Last.Year.of.Construction = as.numeric(tabData$Last.Year.of.Construction) # Convert year to numeric


# Fill data for missing project years -------------------------------------
restore9999 = restore[restore$YEAR == 9999,] # Limit to projects missing years (coded as 9999)

for (i in 1:nrow(restore9999))
{
  projId = restore9999[i,]$PROJ_ID
  if (projId %in% tabData$Project.ID)
  {
    y = tabData[tabData$Project.ID == projId,]$Last.Year.of.Construction
    restore9999@data[i,]$YEAR = y
    restore@data[restore$PROJ_ID == projId,]$YEAR = y
  }
}

# Create spreadsheet for manual lookups -----------------------------------
restore9999 = restore9999[restore9999$YEAR == 9999,] # Remove all newly added years to keep only missing year projects
restore9999$URL = paste0("https://www.google.com/search?q=", gsub(" ", "+", restore9999$PROJ_NAME), "+site%3Alacoast.gov")

if (!file.exists(restoreT1undated))
{
  write.table(restore9999, restoreT1undated, row.names = F, quote = F, sep = "\t")
}


# Manual dating must take place between the lines above and code below. Edit file directly and use links to determine dates.


# Fill Data from Manual Year Data -----------------------------------------
if (!file.exists(restoreT1manualDated))
{
  stop(sprintf("Manually dated restoration projects missing; manually date projects found in %s", restoreT1undated))
}

restoreManual = read.delim(restoreT1manualDated)
restoreManual$YEAR = as.numeric(as.character(restoreManual$YEAR)) # Convert year to numeric
restoreManual$PROJ_ID = as.character(restoreManual$PROJ_ID) # Convert id to character

for (i in 1:nrow(restore9999))
{
  projId = restore9999[i,]$PROJ_ID
  acres = restore9999[i,]$ACRES # some ids are duplicate (multi-polygon entry; think Hawaii), so match acreage which is calculated from shape
  if (projId %in% restoreManual$PROJ_ID)
  {
    y = restoreManual[restoreManual$PROJ_ID == projId & floor(restoreManual$ACRES*100) == floor(acres*100),]$YEAR
    restore9999@data[i,]$YEAR = y
    restore@data[restore$PROJ_ID == projId,]$YEAR = y
  }
}

# Plot Restorations Per Year ----------------------------------------------
# plot(as.numeric(table(restore$YEAR[restore$YEAR > 0]))~c(1986:2017),type="l",
#      xlab="Year",
#      ylab="Restoration")
# lines(as.numeric(table(restore$YEAR[restore$YEAR > 0 & restore$PROJ_TYPE != "OT"]))~c(1987:2017),col="red")


# Save New Shapefile with Dates Filled ------------------------------------
restore = restore[1:10]
writeOGR(restore, restoreT1updated, restoreT1outputName, driver="ESRI Shapefile", overwrite_layer = T)
