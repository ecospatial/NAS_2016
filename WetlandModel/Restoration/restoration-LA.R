setwd("WetlandModel/Restoration")

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
source("../../RUtilityFunctions/createModels.R")
source("../../RUtilityFunctions/codaSamplesDIC.R")
source("../../RUtilityFunctions/plotRegions.R")

# CONFIG ------------------------------------------------------------------
params = c("RSLR","WH","TR","CS", "NDVI")
restoreParams = c("HA", "MC", "BW", "VP")
barrierIslands = F #Include barrier islands

phoneNotifications = F
if (phoneNotifications)
{
  library(RPushbullet)
  pbPost("note", "NAS_2016", "Starting data prep for model run...")
}
# Database Connection and Loading -----------------------------------------
# source("../../config/postgresqlcfg.R")
# if(exists("user") || exists("pw")) {  
#   con <- dbConnect(PostgreSQL(), dbname = db, user = user,
#                    host = host, port = port,
#                    password = pw)
#   rm(pw);rm(user)
# }
# 
# huc2 = get_postgis_query(con, "SELECT * FROM huc2 WHERE huc2.HUC2 IN ('12','13','08','03')", geom_name = "geom")
# 
# dbDisconnect(con)


# Load Local Data (TODO: Make DB) -----------------------------------------
# Hydrological regimes
HUClevel = "HUC4"
HUCfilename = gsub("HUC(\\d*)", "WBDHU\\1", HUClevel)
HUC = readOGR(sprintf("C:/DATA/HUC/HUC_shapes/%s.shp", HUCfilename), HUCfilename)

# Buffers
thk99buff = readOGR("C:/DATA/EarthEngine/T1/thk99buff.shp", "thk99buff")
thk99buff = thk99buff[thk99buff$WET > 0,] # Remove buffers without wetland change

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
  if (x == 9)
    return("red")
  else if (x == 10)
    return("cyan")
  else
    return("yellow")
}
plotRegions(states=T, clipRegions=c(9,10))
plot(thk99buff, add=T, col=sapply(thk99buff$region, colF))


# Load and Process Restoration Data ---------------------------------------
source("prepRestoreData.R")
restore = restore[restore$YEAR <= 2005 & restore$YEAR > 1900,]
#View(restore[order(restore$ACRES, decreasing=TRUE)[1:15],]@data)
removal = order(restore$ACRES, decreasing=TRUE)[1:4]
restore = restore[-removal,]
restore = spTransform(restore, proj4string(thk99buff))
plot(restore)
barplot(table(restore$PROJ_TYPE))
cbind(CODE=typeCodes, NAME=typeDescriptions)

# Save which restorations have buffers within
bufferPerRestore = restore[!is.na(over(restore,thk99buff)$ORIG_FID),]
bufferPerRestore$URL = paste0("=HYPERLINK(\"https://cims.coastal.louisiana.gov/outreach/ProjectView.aspx?projID=", bufferPerRestore$PROJ_ID, "\")")
bufferPerRestore$GOOGLE = paste0("=HYPERLINK(\"https://www.google.com/search?q=", bufferPerRestore$PROJ_NAME, " site%3Alacoast.gov\")")
write.table(bufferPerRestore, "restores.txt", row.names=F, quote=F, sep="\t")

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
plotRegions(states=T, clipRegions=c(9,10))
plot(restore,add=T,border=NA,col="#00FF0033")
plot(thk99buff,add=T,border=NA,col="black")
plot(thk99buff[thk99buff$RESTORE == 1,], add=T,border=NA,col="red")

# Normalize Data ----------------------------------------------------------
thk99buff_n = data.frame(sapply(thk99buff@data[c(params)], function(x){scale(x)}))
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


# Reduce Data -------------------------------------------------------------
years = 5
# HA only (removes buffers w restore that isn't HA), within X years
# HA split does not matter because all HA split are HA == 1
# thk99buff_n_reduce = thk99buff_n[((thk99buff_n$YEAR <= years & thk99buff_n$HA == 1 ) | thk99buff_n$RESTORE == 0),]

# Any restore, within X years
thk99buff_n_reduce = thk99buff_n[((thk99buff_n$YEAR <= years) | thk99buff_n$RESTORE == 0),]


# Arrange Data for JAGS ---------------------------------------------------
regions = length(unique(thk99buff_n_reduce$region))
data = append(list(Nobs=nrow(thk99buff_n_reduce), Nregion=regions), thk99buff_n_reduce)

# Run Each Model in JAGS --------------------------------------------------
if (!dir.exists("Results"))
{
  dir.create("Results")
}

if (phoneNotifications){pbPost("note", "NAS_2016", "Started running the restoration model for LA regions.")}

model58Text = {"
model {
    for (i in 1:Nobs) {
      logWET.mu[i] <- b0[region[i]] + bRSLR[region[i]] * RSLR[i] + bWH[region[i]] * WH[i] + bTR[region[i]] * TR[i] #Linear Model
                       + bHA[region[i]] * HA[i] + bMC * MC[i] + bBW * BW[i] + bVP * VP[i] + bYEAR * YEAR[i]

      logWET[i] ~ dnorm(logWET.mu[i], logWET.tau) #Response distribution
    }

    #Random Effect Priors
    for(j in 1:Nregion) {
      b0[j] ~ dnorm(b0.mu,b0.tau)
      bRSLR[j] ~ dnorm(RSLR.mu,RSLR.tau)
      bWH[j] ~ dnorm(WH.mu,WH.tau)
      bTR[j] ~ dnorm(TR.mu,TR.tau)
      bHA[j] ~ dnorm(HA.mu,HA.tau)
    }

    #Hyper Priors
    b0.mu ~ dnorm(0,0.00001)
    b0.tau ~ dgamma(1,1)
    RSLR.mu ~ dnorm(0,0.00001)
    RSLR.tau ~ dgamma(1,1)
    WH.mu ~ dnorm(0,0.00001)
    WH.tau ~ dgamma(1,1)
    TR.mu ~ dnorm(0,0.00001)
    TR.tau ~ dgamma(1,1)

    HA.mu ~ dnorm(0,0.00001)
    HA.tau ~ dgamma(1,1)
  
    #Fixed Effect Priors
    bMC ~ dnorm(0,0.00001)
    bBW ~ dnorm(0,0.00001)
    bVP ~ dnorm(0,0.00001)
    bYEAR ~ dnorm(0,0.00001)
    # bRESTORE ~ dnorm(0,0.00001)

    logWET.tau ~ dgamma(1,1)
}"
}
model241Text = {"
model {
    for (i in 1:Nobs) {
        logWET.mu[i] <- b0[region[i]] + bRSLR[region[i]] * RSLR[i] + bWH[region[i]] * WH[i] + bTR[region[i]] * TR[i] + bCS[region[i]] * CS[i] + bNDVI * NDVI[i] #Linear Model
                       # + bRESTORE * RESTORE[i]
                        + bHA * HA[i] + bMC * MC[i] + bBW * BW[i] + bVP * VP[i]
        logWET[i] ~ dnorm(logWET.mu[i], logWET.tau) #Response distribution
    }

    #Random Effect Priors
    for(j in 1:Nregion) {
        b0[j] ~ dnorm(b0.mu,b0.tau)
        bRSLR[j] ~ dnorm(RSLR.mu,RSLR.tau)
        bWH[j] ~ dnorm(WH.mu,WH.tau)
        bTR[j] ~ dnorm(TR.mu,TR.tau)
        bCS[j] ~ dnorm(CS.mu,CS.tau)
    }

    #Hyper Priors
    b0.mu ~ dnorm(0,0.00001)
    b0.tau ~ dgamma(1,1)
    RSLR.mu ~ dnorm(0,0.00001)
    RSLR.tau ~ dgamma(1,1)
    WH.mu ~ dnorm(0,0.00001)
    WH.tau ~ dgamma(1,1)
    TR.mu ~ dnorm(0,0.00001)
    TR.tau ~ dgamma(1,1)
    CS.mu ~ dnorm(0,0.00001)
    CS.tau ~ dgamma(1,1) 
    
    #Fixed Effect Priors
    bNDVI ~ dnorm(0,0.00001)
    bHA ~ dnorm(0,0.00001)
    bMC ~ dnorm(0,0.00001)
    bBW ~ dnorm(0,0.00001)
    bVP ~ dnorm(0,0.00001)
  # bRESTORE ~ dnorm(0,0.00001)
    
    logWET.tau ~ dgamma(1,1)
}"
}
model210PCTText = {"
  model {
    for (i in 1:Nobs) {
        logPCT.mu[i] <- b0[region[i]] + bWH[region[i]] * WH[i] + bTR[region[i]] * TR[i] + bCS[region[i]] * CS[i] + bNDVI[region[i]] * NDVI[i] #Linear Model
                      + bHA * HA[i] + bYEAR * YEAR[i] #+ bMC * MC[i] + bBW * BW[i] + bVP * VP[i]
        logPCT[i] ~ dnorm(logPCT.mu[i], logPCT.tau) #Response distribution
    }

    #Random Effect Priors
    for(j in 1:Nregion) {
        b0[j] ~ dnorm(b0.mu,b0.tau)
        bWH[j] ~ dnorm(WH.mu,WH.tau)
        bTR[j] ~ dnorm(TR.mu,TR.tau)
        bCS[j] ~ dnorm(CS.mu,CS.tau)
        bNDVI[j] ~ dnorm(NDVI.mu,NDVI.tau)
    }

    #Hyper Priors
    b0.mu ~ dnorm(0,0.00001)
    b0.tau ~ dgamma(1,1)
    WH.mu ~ dnorm(0,0.00001)
    WH.tau ~ dgamma(1,1)
    TR.mu ~ dnorm(0,0.00001)
    TR.tau ~ dgamma(1,1)
    CS.mu ~ dnorm(0,0.00001)
    CS.tau ~ dgamma(1,1)
    NDVI.mu ~ dnorm(0,0.00001)
    NDVI.tau ~ dgamma(1,1) 
    
    #Fixed Effect Priors
    
    
    logPCT.tau ~ dgamma(1,1)
  }"
}

modelVariableName = sprintf("model%sText", modelNo)
if (!exists(modelVariableName))
{
  stop(sprintf("Model %s does not have a variable with model text in restoration-LA.R", modelNo))
}

model = jags.model(textConnection(model58Text),
                   data = data,
                   n.chains=3,
                   n.adapt=2000)
  
output = coda.samples.dic(model = model,
                        variable.names=c("b0", paste0("b", params),
                                         "bMC", "bVP", "bBW",
                                         "bYEAR",
                                         # "bRESTORE",
                                         "bHA"
                                         #"bHR","bSD","bFD"
                                         ),
                        n.iter=20000,
                        thin=1)

MCMCplot(output$samples, ref_ovl = T, main=output$dic$deviance+output$dic$penalty)
#MCMCplot(output$samples, ref_ovl = T, params=paste0("b", restoreParams))

# 
# for(r in 1:length(restorePerBuffer))
# {
#   buffer=restorePerBuffer[[bufferNo]]
#   if(gsub("[Bb]reakwater", buffer$PROJ_NAME))
#   {
#     print(buffer)
#   }else{
#     print("nope")
#   }
# }

# save(output,file="Results/output.RData")
# 
# if (phoneNotifications){pbPost("note", "NAS_2016", "MODEL RUN COMPLETE!!!")}




# restorePerBuffer=over(thk99buff, restore, returnList=T)
# nPtsWRestore=0
# projTypes = c()
# for(buffer in restorePerBuffer)
# {
#   if (nrow(buffer) > 0)
#   {
#     for (i in 1:nrow(buffer))
#     {
#       row = buffer[i,]
#       projTypes = c(projTypes, as.character(row$PROJ_TYPE))
#     }
#     nPtsWRestore = nPtsWRestore + 1
#   }
# }
# nPtsWRestore
# 
# pTypeNames = restoreTypes[restoreTypes$CODE %in% unique(projTypes),]
# maxType = max(table(projTypes))
# x=barplot(table(projTypes), ylim=c(0,1.10*maxType))
# text(x, table(projTypes), table(projTypes), pos=3)#as.numeric(unlist(combos)))
# textX = 3
# textY = 0.90*maxType
# lineHeight = floor(maxType/20)
# for (i in 1:nrow(pTypeNames))
# {
#   text(textX, textY-(i*lineHeight), pTypeNames[i,]$CODE, pos=4)
#   text(textX+1, textY-(i*lineHeight), pTypeNames[i,]$NAME, pos=4)
# }



# plot(logWET~RSLR, data=thk99buff_n)
# points(logWET~RSLR, data=thk99buff_n[thk99buff_n$RESTORE > 0,],col="green",pch=20)
# points(logWET~RSLR, data=thk99buff_n[thk99buff_n$RESTORE > 0 & thk99buff_n$YEAR <= 10,],col="cyan",pch=20)
# points(logWET~RSLR, data=thk99buff_n[thk99buff_n$RESTORE > 0 & thk99buff_n$YEAR <= 5,],col="red",pch=20)
# title(sprintf("ALL Restores (n = %s)", nrow(thk99buff_n[thk99buff_n$RESTORE > 0,])))
# 
# op=par(mfrow=c(2,2))
# for (type in names(restoreRecode))
# {
#   plot(logWET~RSLR, data=thk99buff_n)
#   points(logWET~RSLR, data=thk99buff_n[thk99buff_n[[type]] > 0,],col="green",pch=20)
#   points(logWET~RSLR, data=thk99buff_n[thk99buff_n[[type]] > 0 & thk99buff_n$YEAR <= 10,],col="cyan",pch=20)
#   points(logWET~RSLR, data=thk99buff_n[thk99buff_n[[type]] > 0 & thk99buff_n$YEAR <= 5,],col="red",pch=20)
#   title(sprintf("%s (n = %s)", type, nrow(thk99buff_n[thk99buff_n[[type]] > 0,])))
# }
# par(op)
# 
# for (type in names(restoreRecode))
# {
#   plotRegions(states=T, clipRegions=c(9,10))
#   plot(thk99buff,border=NA,col="black",add=T)
#   plot(thk99buff[thk99buff_n[[type]] > 0,],border=NA,col="green",add=T)
#   plot(thk99buff[thk99buff_n[[type]] > 0 & thk99buff_n[[type]] <= 10,],border=NA,col="cyan",add=T)
#   plot(thk99buff[thk99buff_n[[type]] > 0 & thk99buff_n[[type]] <= 5,],border=NA,col="red",add=T)
#   title(type)
# }
# 