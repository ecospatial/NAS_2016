setwd("WetlandModel/RestorationV2")

library(rjags)
library(sp)
library(magrittr)
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(RPostgreSQL)
library(postGIStools)
library(MCMCvis)
#source("../../RUtilityFunctions/createModels.R")
source("../../RUtilityFunctions/codaSamplesDIC.R")
source("../../RUtilityFunctions/plotRegions.R")

# CONFIG ------------------------------------------------------------------
params = c("RSLR","WH","TR","CS", "NDVI")
restoreParams = c("HA", "MC", "BW", "VP")
barrierIslands = F #Include barrier islands

# Load Local Data (TODO: Make DB) -----------------------------------------
# Hydrological regimes
HUClevel = "HUC4"
HUCfilename = gsub("HUC(\\d*)", "WBDHU\\1", HUClevel)
HUC = readOGR(sprintf("C:/DATA/HUC/HUC_shapes/%s.shp", HUCfilename), HUCfilename)

# Buffers
thk99buff = readOGR("C:/DATA/EarthEngine/T1/thk99buff.shp", "thk99buff")

# Remove barrier islands if chosen
if (!barrierIslands)
{
  shoreline = readOGR("C:/Users/GCRLWuHardy/Documents/General Maps/Coastlines/USCoast_h_L1.shp", "USCoast_h_L1")
  shoreline = spTransform(shoreline, proj4string(thk99buff))
  shoreline = crop(shoreline, thk99buff)
  thk99buff = thk99buff[!is.na(over(thk99buff, geometry(shoreline))),]
}

# Extract HUC and region to each buffer
HUC = spTransform(HUC, proj4string(thk99buff))
hucZone = over(thk99buff,HUC[,HUClevel])
thk99buff[[HUClevel]] = factor(hucZone[[HUClevel]])
if (HUClevel == "HUC4") {
  thk99buff$region = as.numeric(thk99buff[[HUClevel]])
} else if (HUClevel == "HUC2") {
  if (regions == 2)
  {
    thk99buff$region = sapply(thk99buff$HUC2, function(x){
      if (x == "03" | x == "12" | x == "13")
        return(1)
      else
        return(2)
    })
  } else if (regions == 3) {
    thk99buff$region = sapply(thk99buff$HUC2, function(x){
      if (x == "12" | x == "13") # West Gulf
        return(1)
      else if (x == "08") # LA
        return(2)
      else
        return(3) # East Gulf (03)
    })
  }
}

#Visualize regions
colF = function(x){
  if (x == 10)
    return("red")
  else if (x == 11)
    return("cyan")
  else
    return("yellow")
}
plotRegions(states=T, clipRegions=c(10,11))
plot(thk99buff, add=T, col=sapply(thk99buff$region, colF))


# Load and Process Restoration Data ---------------------------------------
source("prepRestoreData.R")
restore = readOGR("C:/DATA/CPRA_Projects/T1/CPRA_PolysYears.shp", "CPRA_PolysYears")
restore = restore[restore$YEAR <= 2005 & restore$YEAR > 1900,]
#View(restore[order(restore$ACRES, decreasing=TRUE)[1:15],]@data)
removal = order(restore$ACRES, decreasing=TRUE)[1:4]
restore = restore[-removal,]
restore = spTransform(restore, proj4string(thk99buff))
restore$PROJ_TYPE = as.character(restore$PROJ_TYPE)
plot(restore)
barplot(table(restore$PROJ_TYPE))
cbind(CODE=typeCodes, NAME=typeDescriptions)

# Save which restorations have buffers within
bufferPerRestore = restore[!is.na(over(restore,thk99buff)$ORIG_FID),]
bufferPerRestore$URL = paste0("=HYPERLINK(\"https://cims.coastal.louisiana.gov/outreach/ProjectView.aspx?projID=", bufferPerRestore$PROJ_ID, "\")")
bufferPerRestore$GOOGLE = paste0("=HYPERLINK(\"https://www.google.com/search?q=", bufferPerRestore$PROJ_NAME, " site%3Alacoast.gov\")")
write.table(bufferPerRestore, "restoresV2.txt", row.names=F, quote=F, sep="\t")

# Recode mis-coded projects
miscodes = read.delim("recodesV2.txt", sep="\t", stringsAsFactors = F)
for (i in 1:nrow(miscodes))
{
  id = miscodes[i,]$PROJ_ID
  if (miscodes[i,]$RECODE != "")
    restore@data[restore@data$PROJ_ID == id,]$PROJ_TYPE = miscodes[i,]$RECODE
}

x=barplot(table(restore$PROJ_TYPE), xaxt="n")
text(cex=1, x=x-.25, y=-4, names(table(restore$PROJ_TYPE)), xpd=TRUE, srt=45)

# Extract restoration type to buffers (recoded to less restore types)
restoreRecode = list( #SP and BH should always be recoded because they describe the goal, not the methods.
  # Hydrologic alteration
  HA = c("HA", "FD", "HR", "SD"),
  FD = "FD",
  HR = "HR",
  SD = "SD",
  # Marsh creation
  MC = c("MC", "TE"),
  # Breakwaters
  BW = c("BW"),
  #Vegetative Planting
  VP = c("VP","PL")
)

for (type in names(restoreRecode))
{
  thk99buff[[type]] = rep(-1, nrow(thk99buff))
}
thk99buff$YEAR = rep(-1, nrow(thk99buff))
thk99buff$RESTORE = rep(-1, nrow(thk99buff))

restorePerBuffer=over(thk99buff, restore, returnList=T)
for (bufferNo in 1:length(restorePerBuffer))
{
  buffer = restorePerBuffer[[bufferNo]]
  if (nrow(buffer) > 0) #Buffer has restoration projects
  {
    for (i in 1:nrow(buffer))
    {
      projTypes = unlist(strsplit(buffer[i,]$PROJ_TYPE, "/"))
      if (is.null(projTypes))
        projTypes = buffer[i,]$PROJ_TYPE
    }
    for (type in names(restoreRecode))
    {
      if (any(restoreRecode[[type]] %in% projTypes))
      {
        thk99buff@data[bufferNo,][[type]] = 1
      }
      else
      {
        thk99buff@data[bufferNo,][[type]] = 0
      }
    }
    thk99buff@data[bufferNo,]$YEAR = 2006 - min(buffer$YEAR)
    thk99buff@data[bufferNo,]$RESTORE = 1
  } else {
    for (type in names(restoreRecode))
    {
      thk99buff@data[bufferNo,][[type]] = 0
    }
    thk99buff@data[bufferNo,]$YEAR = 0
    thk99buff@data[bufferNo,]$RESTORE = 0
  }
}
plotRegions(states=T, clipRegions=c(10,11))
points(coordinates(thk99buff),col="black",pch=19)
points(coordinates(thk99buff[thk99buff$RESTORE == 1,]),col="red",pch=19)
plot(restore,add=T,border=NA,col="#00FF0033")

# Code Restoration Type ---------------------------------------------------
thk99buff$RESTORETYPE = rep(5, nrow(thk99buff))
dblCode = 0
types = c("BW","HA","MC","VP")
for (ty in 1:length(types))
{
  for (i in 1:nrow(thk99buff))
  {
    if (thk99buff@data[i,][[types[ty]]] == 1)
    {
      if (thk99buff@data[i,]$RESTORETYPE == 5)
      {
        thk99buff@data[i,]$RESTORETYPE = ty
      } else {
        dblCode = dblCode + 1
      }
    }
  }
}
print(sprintf("Double codes: %s", dblCode))

# IMPORTANT: Reduce Data to Louisiana -------------------------------------
thk99buff_la = thk99buff[thk99buff$region %in% c(10,11),]
thk99buff_la$region = thk99buff_la$region-9


# Add additional region + restoration variables ---------------------------
thk99buff_la$RESTORENREGION = rep(-1, nrow(thk99buff_la))
thk99buff_la$TYPENREGION = rep(-1, nrow(thk99buff_la))
for (i in 1:nrow(thk99buff_la))
{
  row = thk99buff_la[i,]
  # BINARY RESTORE AND REGION (2x2)
  thk99buff_la$RESTORENREGION[i] = ifelse(row$region == 1, row$RESTORE+1, thk99buff_la$RESTORE[i]+1 + length(unique(thk99buff_la$RESTORE)))

  # RESTORE TYPE AND REGION (5x2; 5 = types + none)
  thk99buff_la$TYPENREGION[i] = ifelse(row$region == 1, row$RESTORETYPE, thk99buff_la$RESTORETYPE[i] + length(unique(thk99buff_la$RESTORETYPE)))
}


# Normalize Data ----------------------------------------------------------
thk99buff_la_n = data.frame(sapply(thk99buff_la@data[c(params)], function(x){scale(x)}))
thk99buff_la_n = cbind(thk99buff_la_n, region=thk99buff_la$region)
thk99buff_la_n = cbind(thk99buff_la_n, logWET=thk99buff_la$logWET)
thk99buff_la_n = cbind(thk99buff_la_n, logPCT=thk99buff_la$logPCT)
thk99buff_la_n = cbind(thk99buff_la_n, WET=thk99buff_la$WET)
thk99buff_la_n = cbind(thk99buff_la_n, PCT=thk99buff_la$PCT)
thk99buff_la_n = cbind(thk99buff_la_n, CHG=thk99buff_la$CHG)

thk99buff_la_n$HA = thk99buff_la$HA
thk99buff_la_n$HR = thk99buff_la$HR
thk99buff_la_n$FD = thk99buff_la$FD
thk99buff_la_n$SD = thk99buff_la$SD
thk99buff_la_n$MC = thk99buff_la$MC
thk99buff_la_n$BW = thk99buff_la$BW
thk99buff_la_n$VP = thk99buff_la$VP
thk99buff_la_n$YEAR = thk99buff_la$YEAR
thk99buff_la_n$RESTORE = thk99buff_la$RESTORE
thk99buff_la_n$RESTORETYPE = thk99buff_la$RESTORETYPE
thk99buff_la_n$RESTORENREGION = thk99buff_la$RESTORENREGION
thk99buff_la_n$TYPENREGION = thk99buff_la$TYPENREGION


# Reduce Data -------------------------------------------------------------
# years = 5
# HA only (removes buffers w restore that isn't HA), within X years
# HA split does not matter because all HA split are HA == 1
# thk99buff_n_reduce = thk99buff_n[((thk99buff_n$YEAR <= years & thk99buff_n$HA == 1 ) | thk99buff_n$RESTORE == 0),]

# Any restore, within X years
# thk99buff_la_n_reduce = thk99buff_la_n[((thk99buff_la_n$YEAR <= years) | thk99buff_la_n$RESTORE == 0),]

# Null NDVIs
# thk99buff_la_n_reduce = thk99buff_la_n_reduce[!is.na(thk99buff_la_n_reduce$NDVI),]


# QA Plots ----------------------------------------------------------------
plot(thk99buff_la, col=NA, border=NA, axes=T)
plot(restore,add=T,col="#99999933")
points(coordinates(thk99buff_la), bg="black", pch=21, col=NA)
points(coordinates(thk99buff_la[thk99buff_la$CHG > 0,]), bg="green", pch=21, col=NA)
points(coordinates(thk99buff_la[thk99buff_la$CHG < 0,]), bg="red", pch=21, col=NA)
points(coordinates(thk99buff_la[thk99buff_la$RESTORE == 1 & thk99buff_la$CHG > 0,]), bg="blue", pch=21, col=NA)
points(coordinates(thk99buff_la[thk99buff_la$RESTORE == 1 & thk99buff_la$CHG < 0,]), bg="yellow", pch=21, col=NA)
legend(-93, 29.25,
       legend = c("No Change", "Gain", "Loss", "Gain w Restore", "Loss w Restore"),
       col = c("black", "green", "red", "blue", "yellow"),
       pch=19)

plot(CHG~RSLR, data=thk99buff_la, pch=19)
points(CHG~RSLR, data=thk99buff_la[thk99buff_la$CHG > 0,], pch=19, col="green")
points(CHG~RSLR, data=thk99buff_la[thk99buff_la$CHG < 0,], pch=19, col="red")
points(CHG~RSLR, data=thk99buff_la[thk99buff_la$RESTORE == 1 & thk99buff_la$CHG > 0,], pch=19, col="blue")
points(CHG~RSLR, data=thk99buff_la[thk99buff_la$RESTORE == 1 & thk99buff_la$CHG < 0,], pch=19, col="yellow")



# Run Each Model in JAGS --------------------------------------------------
if (!dir.exists("Results"))
{
  dir.create("Results")
}

modelFiles=c(
  #"fullModel",
  "noRestoRegDiff-model58"
)

runModel = function(modelFile, normalized = F, Ntypes = NA, track = NULL) {
  extraTrack = track
  if (is.na(Ntypes))
  {
    Ntypes = length(types)+1
  }
  
  outputName = sprintf("Results/%s%s.RData", modelFile, if(normalized) "_n" else "")
  if (file.exists(outputName)) {
    print(sprintf("%s already ran, skipping", modelFile))
    return()
  }
  
  # Arrange Data for JAGS 
  regions = length(unique(thk99buff_la$region))
  
  if (normalized)
  {
    data = append(list(Nobs=nrow(thk99buff_la_n), Nregion=regions, Ntypes=Ntypes), thk99buff_la_n)
  } else {
    data = append(list(Nobs=nrow(thk99buff_la), Nregion=regions, Ntypes=Ntypes), thk99buff_la@data)
  }
  
  model = jags.model(sprintf("%s.txt", modelFile),
                     data = data,
                     n.chains=3,
                     n.adapt=2000)
  
  output = coda.samples.dic(model = model,
                            variable.names=c("b0", paste0("b", params),
                                             "bMC", "bVP", "bBW",
                                             "bYEAR",
                                             # "bRESTORE",
                                             "bHA",
                                             #"bHR","bSD","bFD"
                                             extraTrack
                            ),
                            n.iter=20000,
                            thin=1)
  
  save(output, file=outputName)
}

runAllModels = function() {
  for (modelFile in modelFiles) {
    runModel(modelFile)
  }
}

runAllModels()

loadModelResults = function(modelFile) {
  load(sprintf("Results/%s.RData", modelFile))
  return(output)
}


o1 = loadModelResults("58restoRandomYear")
o2 = loadModelResults("58restoRandomYear_n")


allResultFiles = list.files("Results")
allResultDic = list()
for (resultFile in allResultFiles)
{
  load(paste0("Results/",resultFile))
  allResultDic$model = c(allResultDic$model, gsub(".RData","",resultFile))
  allResultDic$dic = c(allResultDic$dic, output$dic$deviance + output$dic$penalty)
}
allResultDic = data.frame(allResultDic)
allResultDic = allResultDic[order(allResultDic$dic),]
allResultDic

combos = expand.grid(c(types, "NA"), c("W","E"))
labels = paste0(combos[,1], ",", combos[,2])

runModel("dblMultiLvl")
MCMCplot(loadModelResults("dblMultiLvl")$samples, params="bWH", ref_ovl=T, labels = sprintf("bWH[%s]", labels), xlab="Wetland Change (hectares)")
MCMCplot(loadModelResults("dblMultiLvl")$samples, params="bRSLR", ref_ovl=T, labels = sprintf("bRSLR[%s]", labels), xlab="Wetland Change (hectares)")

rslrPooledRestore = loadModelResults("RSLR-model58")
MCMCplot(rslrPooledRestore$samples, params=c("bRSLR", paste0("b", types)), ref_ovl=T)
plot(CHG~RSLR, data=rslrPooledRestore$data)
lm1=lm(CHG~RSLR, data=rslrPooledRestore$data)
abline(lm1,col="red")

rslrWhRandomRestore = loadModelResults("58restoRandomYear")
MCMCplot(rslrWhRandomRestore$samples, params=c("bWH"), ref_ovl=T)
MCMCplot(rslrWhRandomRestore$samples, params=c("bRSLR"), ref_ovl=T)

runModel("58restoRandomYearWBase", track=c("zWH","zRSLR"))
rslrWhRandomRestoreWbase = loadModelResults("58restoRandomYearWbase")
MCMCplot(rslrWhRandomRestoreWbase$samples, params=c("bWH"), ref_ovl=T)
MCMCplot(rslrWhRandomRestoreWbase$samples, params=c("bRSLR"), ref_ovl=T)
MCMCplot(rslrWhRandomRestoreWbase$samples, params=c("zWH"), ref_ovl=T)
MCMCplot(rslrWhRandomRestoreWbase$samples, params=c("zRSLR"), ref_ovl=T)

runModel(as.character(allResultDic[1,]$model))
rslrRandomVarStruct = loadModelResults(as.character(allResultDic[1,]$model))
MCMCplot(rslrRandomVarStruct$samples, ref_ovl=T)
