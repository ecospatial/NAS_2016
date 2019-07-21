setwd("WetlandModel/SLR")

library(rjags)
library(sp)
library(magrittr)
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(MCMCvis)
# Load in custom utility functions
source("../loadTHK99.R")
source("../../RUtilityFunctions/createModels.R")
source("../../RUtilityFunctions/codaSamplesDIC.R")

loadTHK99data(local=T, regions="ALL")
params = c("WH","CS","RSLR","TR","NDVI")

# Load and Process Restoration Data ---------------------------------------
source("prepRestoreData.R")
restore = restore[restore$YEAR <= 2005 & restore$YEAR > 1900,]
#View(restore[order(restore$ACRES, decreasing=TRUE)[1:15],]@data)
removal = order(restore$ACRES, decreasing=TRUE)[1:4]
restore = restore[-removal,]
restore = spTransform(restore, proj4string(thk99buff))
restoreParams = c("HA","BW","MC","VP","YEAR")

plot(restore)
barplot(table(restore$PROJ_TYPE))
cbind(CODE=typeCodes, NAME=typeDescriptions)

# Save which restorations have buffers within
bufferPerRestore = restore[!is.na(over(restore,thk99buff)$ORIG_FID),]
bufferPerRestore$URL = paste0("=HYPERLINK(\"https://cims.coastal.louisiana.gov/outreach/ProjectView.aspx?projID=", bufferPerRestore$PROJ_ID, "\")")
bufferPerRestore$GOOGLE = paste0("=HYPERLINK(\"https://www.google.com/search?q=", bufferPerRestore$PROJ_NAME, " site%3Alacoast.gov\")")

# Recode mis-coded projects
miscodes = read.delim("miscodes.txt", sep="\t", stringsAsFactors = F)
for (i in 1:nrow(miscodes))
{
  id = miscodes[i,]$PROJ_ID
  if (miscodes[i,]$RECODE != "")
    restore@data[restore@data$PROJ_ID == id,]$PROJ_TYPE = miscodes[i,]$RECODE
}
miscodesBarrier = read.delim("miscodes-barrier.txt", sep="\t", stringsAsFactors = F)
for (i in 1:nrow(miscodesBarrier))
{
  id = miscodesBarrier[i,]$PROJ_ID
  if (miscodesBarrier[i,]$RECODE != "")
    restore@data[restore@data$PROJ_ID == id,]$PROJ_TYPE = miscodesBarrier[i,]$RECODE
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
source("../../RUtilityFunctions/plotRegions.R")
plotRegions(states="LA", focus="state", stateBoundaries=T, regions=c(9,10), labs=c("W LA", "E LA"))
plot(restore,add=T,border=NA,col="#00FF0033")
plot(thk99buff,add=T,border=NA,col="black")
plot(thk99buff[thk99buff$RESTORE == 1,], add=T,border=NA,col="red")

# Normalize Data ----------------------------------------------------------
thk99buff_n = data.frame(sapply(thk99buff@data[params], function(x){scale(x)}))
thk99buff_n = cbind(thk99buff_n, region=thk99buff$region)
thk99buff_n = cbind(thk99buff_n, logWET=thk99buff$logWET)
thk99buff_n = cbind(thk99buff_n, logPCT=thk99buff$logPCT)
thk99buff_n = cbind(thk99buff_n, WET=thk99buff$WET)
thk99buff_n = cbind(thk99buff_n, PCT=thk99buff$PCT)

thk99buff_n$HA = thk99buff$HA
thk99buff_n$HR = thk99buff$HR
thk99buff_n$FD = thk99buff$FD
thk99buff_n$SD = thk99buff$SD
thk99buff_n$MC = thk99buff$MC
thk99buff_n$BW = thk99buff$BW
thk99buff_n$VP = thk99buff$VP
thk99buff_n$YEAR = thk99buff$YEAR
thk99buff_n$RESTORE = thk99buff$RESTORE

# IMPORTANT: Reduce Data to Louisiana -------------------------------------
thk99buff_n = thk99buff_n[thk99buff$region %in% c(9,10),]
thk99buff_n$region = thk99buff_n$region-8


# Any restore, within X years
years = 10
data_reduce = thk99buff_n[((thk99buff_n$YEAR <= years) | thk99buff_n$RESTORE == 0),]

# Arrange Data for JAGS ---------------------------------------------------
regions = length(unique(data_reduce$region))

data = append(list(Nobs=nrow(data_reduce), Nregion=regions), data_reduce)

# Run Model in JAGS -------------------------------------------------------
if (!dir.exists("Results"))
{
  dir.create("Results")
}

modelNo = 1
while(file.exists(sprintf("Results/%s.RData", modelNo)))
{
  modelNo = modelNo + 1  
}

model = jags.model("model.txt",
                   data = data,
                   n.chains=3,
                   n.adapt=50000)

output = coda.samples.dic(model = model,
                          variable.names=c("b0", paste0("b", params), paste0("b", restoreParams), "logWET.p", "logWET.HA","logWET.noR"),
                          n.iter=250000,
                          thin=4)
output$data = data
save(output, file=sprintf("Results/%s.RData", modelNo))

a = MCMCsummary(output$samples)# %>% as.data.frame()
b = a %>% cbind(param=row.names(.), .)
c = b %>% mutate(param=as.character(param))
d = c %>% dplyr::filter(grepl("logWET.p", param))
predPost = MCMCsummary(output$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
predPost_noR = MCMCsummary(output$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.noR", param))
predPost_HA= MCMCsummary(output$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.HA", param))
