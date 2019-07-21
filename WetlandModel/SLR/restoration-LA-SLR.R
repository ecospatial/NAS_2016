setwd("WetlandModel/SLR")

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
# Load in modified MCMCchains referenced here https://github.com/caseyyoungflesh/MCMCvis/pull/4
# Should be on CRAN by September 2018
source("~/00000 2017/MCMCvis/R/MCMCchains.R")
source("~/00000 2017/MCMCvis/R/MCMCplot.R")
source("~/00000 2017/MCMCvis/R/MCMCtrace.R")
source("~/00000 2017/MCMCvis/R/MCMCsummary.R")
# Load in custom utility functions
source("../../RUtilityFunctions/createModels.R")
source("../../RUtilityFunctions/codaSamplesDIC.R")
source("../../RUtilityFunctions/plotRegions.R")

# CONFIG ------------------------------------------------------------------
modelName = "58-Any-10y-Y"

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


# Function for Running Restoration Model ----------------------------------
RunRestorationModel = function(HAonly, HAsplit=NULL, years=NULL, modelNo=NULL, includeYear=NULL, includeRestoreDummy=NULL, HArandom=NULL,
                               data = thk99buff_n, iters = 20000, thin = 1,
                               RSLRscen=NULL, nameMod="")
{
  if (is.null(HAsplit))
  {
    modParams = HAonly
    if (length(modParams) != 7)
    {
      stop("Wrong number of parameters in vector. Either give vector with 7 parameters, or 7 individual parameters.")
    }
    HAonly=modParams$HAonly
    HAsplit=modParams$HAsplit
    years=modParams$years
    modelNo=modParams$modelNo
    includeYear=modParams$includeYear
    includeRestoreDummy=modParams$includeRestoreDummy
    HArandom=modParams$HArandom
  }
  
  model58Text = {"
    model {
    for (i in 1:Nobs) {
      logWET.mu[i] <- b0[region[i]] + bRSLR[region[i]] * RSLR[i] + bWH[region[i]] * WH[i] + bTR[region[i]] * TR[i] #Linear Model
      %s #HA or HA split dummies
      %s #Other restore type dummies
      %s #Year of restore
      %s #Restore dummy
      
      logWET[i] ~ dnorm(logWET.mu[i], logWET.tau) #Response distribution

      logWET.mu.p[i] <- b0[region[i]] + bRSLR[region[i]] * (RSLR[i] + RSLRscenario) + bWH[region[i]] * WH[i] + bTR[region[i]] * TR[i] #Linear Model
      %s #HA or HA split dummies
      %s #Other restore type dummies
      %s #Year of restore
      %s #Restore dummy

      logWET.p[i] ~ dnorm(logWET.mu.p[i], logWET.tau) #Predictions   
    }
    
    #Random Effect Priors
    for(j in 1:Nregion) {
      b0[j] ~ dnorm(b0.mu,b0.tau)
      bRSLR[j] ~ dnorm(RSLR.mu,RSLR.tau)
      bWH[j] ~ dnorm(WH.mu,WH.tau)
      bTR[j] ~ dnorm(TR.mu,TR.tau)
      %s
    }
    
    #Hyper Priors
    b0.mu ~ dnorm(0,0.00001)
    b0.tau ~ dgamma(1,1)
    RSLR.mu ~ dnorm(0,0.00001)
    RSLR.sigma ~ dunif(0,10)
    RSLR.tau <- sqrt(1/RSLR.sigma)
    WH.mu ~ dnorm(0,0.00001)
    WH.sigma ~ dunif(0,10)
    WH.tau <- sqrt(1/WH.sigma)
    TR.mu ~ dnorm(0,0.00001)
    TR.sigma ~ dunif(0,10)
    TR.tau <- sqrt(1/TR.sigma)
    %s
    
    #Fixed Effect Priors
    #HA or HA split
    %s
    #Other restore
    %s
    #Year
    %s
    #Restore dummy
    %s

    RSLRscenario %s
    
    logWET.tau ~ dgamma(1,1)
    }"
  }
  model241Text = {"
    model {
    for (i in 1:Nobs) {
    logWET.mu[i] <- b0[region[i]] + bRSLR[region[i]] * RSLR[i] + bWH[region[i]] * WH[i] + bTR[region[i]] * TR[i] + bCS[region[i]] * CS[i] + bNDVI * NDVI[i] #Linear Model
    %s #HA or HA split dummies
    %s #Other restore type dummies
    %s #Year of restore
    %s #Restore dummy
    
    logWET[i] ~ dnorm(logWET.mu[i], logWET.tau) #Response distribution
    }
    
    #Random Effect Priors
    for(j in 1:Nregion) {
    b0[j] ~ dnorm(b0.mu,b0.tau)
    bRSLR[j] ~ dnorm(RSLR.mu,RSLR.tau)
    bWH[j] ~ dnorm(WH.mu,WH.tau)
    bTR[j] ~ dnorm(TR.mu,TR.tau)
    bCS[j] ~ dnorm(CS.mu,CS.tau)
    %s
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
    %s
    
    #Fixed Effect Priors
    bNDVI ~ dnorm(0,0.00001)
    bHA ~ dnorm(0,0.00001)
    bMC ~ dnorm(0,0.00001)
    bBW ~ dnorm(0,0.00001)
    bVP ~ dnorm(0,0.00001)
    #HA or HA split
    %s
    #Other restore
    %s
    #Year
    %s
    #Restore dummy
    %s
    
    logWET.tau ~ dgamma(1,1)
    }"
  }
  model210PCTText = {"
    model {
    for (i in 1:Nobs) {
    logPCT.mu[i] <- b0[region[i]] + bWH[region[i]] * WH[i] + bTR[region[i]] * TR[i] + bCS[region[i]] * CS[i] + bNDVI[region[i]] * NDVI[i] #Linear Model
    %s #HA or HA split dummies
    %s #Other restore type dummies
    %s #Year of restore
    %s #Restore dummy
    
    logPCT[i] ~ dnorm(logPCT.mu[i], logPCT.tau) #Response distribution
    }
    
    #Random Effect Priors
    for(j in 1:Nregion) {
    b0[j] ~ dnorm(b0.mu,b0.tau)
    bWH[j] ~ dnorm(WH.mu,WH.tau)
    bTR[j] ~ dnorm(TR.mu,TR.tau)
    bCS[j] ~ dnorm(CS.mu,CS.tau)
    bNDVI[j] ~ dnorm(NDVI.mu,NDVI.tau)
    %s
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
    %s
    
    #Fixed Effect Priors
    %s
    
    logPCT.tau ~ dgamma(1,1)
    }"
  }
  modelName = sprintf("%s-%s%s-%sy%s%s%s%s%s",
                      modelNo,
                      if(HAonly) "HA" else "Any",
                      if(HAsplit) "-split" else "",
                      years,
                      if(includeYear | includeRestoreDummy) "-" else "",
                      if(includeYear) "Y" else "",
                      if(includeRestoreDummy) "Y" else "",
                      if(HArandom) "-rHA" else "",
                      nameMod
  )
  # Reduce Data -------------------------------------------------------------
  if (HAonly)
  {
    # HA only (removes buffers w restore that isn't HA), within X years
    # HA split does not matter because all HA split are HA == 1
    data_reduce = data[((data$YEAR <= years & data$HA == 1 ) | data$RESTORE == 0),]
  } else {
    # Any restore, within X years
    data_reduce = data[((data$YEAR <= years) | data$RESTORE == 0),]
  }
  
  # Arrange Data for JAGS ---------------------------------------------------
  regions = length(unique(data_reduce$region))
  data = append(list(Nobs=nrow(data_reduce), Nregion=regions), data_reduce)
  
  # Run Each Model in JAGS --------------------------------------------------
  if (!dir.exists("Models"))
  {
    dir.create("Models")
  }
  
  if (!dir.exists("Results"))
  {
    dir.create("Results")
  }
  
  if (!dir.exists("Results/Plots"))
  {
    dir.create("Results/Plots")
  }
  
  if (!file.exists("Results/DIC.txt"))
  {
    write.table("modelNo\tDIC", "Results/DIC.txt", row.names=F, quote=F, sep="\t")
  }
  
  if (phoneNotifications){pbPost("note", "NAS_2016", "Started running the restoration model for LA regions.")}
  
  
  modelVariableName = sprintf("model%sText", modelNo)
  if (!exists(modelVariableName))
  {
    stop(sprintf("Model %s does not have a variable with model text in restoration-LA.R", modelNo))
  }
  
  if (HAsplit)
  {
    if (HArandom)
    {
      HAlikelihood = "+ bHR[region[i]] * HR[i] + bSD[region[i]] * SD[i] + bFD[region[i]] * FD[i]"
      HArandomEffectPriors = "bHR[j] ~ dnorm(HR.mu, HR.tau)\nbSD[j] ~ dnorm(SD.mu, SD.tau)\nbFD[j] ~ dnorm(FD.mu, FD.tau)"
      HAhyperPriors = "HR.mu ~ dnorm(0,0.00001)\nHR.tau ~ dgamma(1,1)\nSD.mu ~ dnorm(0,0.00001)\nSD.tau ~ dgamma(1,1)\nFD.mu ~ dnorm(0,0.00001)\nFD.tau ~ dgamma(1,1)"
      HApriors = ""
    } else {
      HAlikelihood = "+ bHR * HR[i] + bSD * SD[i] + bFD * FD[i]"
      HArandomEffectPriors = "" 
      HAhyperPriors = ""
      HApriors = "bHR ~ dnorm(0,0.00001)\nbSD ~ dnorm(0,0.00001)\nbFD ~ dnorm(0,0.00001)"
    }
  } else {
    if (HArandom)
    {
      HAlikelihood = "+ bHA[region[i]] * HA[i]" 
      HArandomEffectPriors = "bHA[j] ~ dnorm(HA.mu, HA.tau)"
      HAhyperPriors = "HA.mu ~ dnorm(0,0.00001)\nHA.tau ~ dgamma(1,1)"
      HApriors = ""
    } else {
      HAlikelihood = "+ bHA * HA[i]"
      HArandomEffectPriors = ""
      HAhyperPriors = ""
      HApriors = "bHA ~ dnorm(0,0.00001)"
    }
  }
  modelText = sprintf(get(modelVariableName),
                      # Likelihood
                      HAlikelihood,
                      if (HAonly) "" else "+ bMC * MC[i] + bBW * BW[i] + bVP * VP[i] ",
                      if (includeYear) "+ bYEAR * YEAR[i]" else "",
                      if (includeRestoreDummy) "+ bRESTORE * RESTORE[i]" else "",
                      
                      # Prediction Likelihood
                      HAlikelihood,
                      if (HAonly) "" else "+ bMC * MC[i] + bBW * BW[i] + bVP * VP[i] ",
                      if (includeYear) "+ bYEAR * YEAR[i]" else "",
                      if (includeRestoreDummy) "+ bRESTORE * RESTORE[i]" else "",
                      
                      # Random Effect Priors
                      HArandomEffectPriors,
                      
                      # Hyperpriors
                      HAhyperPriors,
                      
                      # Priors
                      HApriors,
                      if (HAonly) "" else "bMC ~ dnorm(0,0.00001)\nbBW ~ dnorm(0,0.00001)\nbVP ~ dnorm(0,0.00001)",
                      if (includeYear) "bYEAR ~ dnorm(0,0.00001)" else "",
                      if (includeRestoreDummy) "bRESTORE ~ dnorm(0,0.00001)" else "",
                      
                      # Restoration Present
                      if (!is.null(RSLRscen)) sprintf("~ dnorm(%s)", paste0(RSLRscen, collapse=", ")) else "<- 0"
  )
  
  write(modelText, file=sprintf("Models/%s.txt", modelName))
  
  model = jags.model(textConnection(modelText),
                     data = data,
                     n.chains=3,
                     n.adapt=iters/10)
  
  # Construct which parameters to track
  HAparams = if (HAsplit) c("HR", "SD", "FD") else c("HA")
  otherparams = if (HAonly) c() else c("MC", "VP", "BW")
  if (includeRestoreDummy) otherparams = c(otherparams, "RESTORE")
  if (includeYear) otherparams = c(otherparams, "YEAR")
  trackRestoreParams = c(HAparams, otherparams)
  
  # Construct which parameters to plot
  plotRestoreParams = c()
  for (param in trackRestoreParams)
  {
    if (sum(data_reduce[param]) > 0)
    {
      plotRestoreParams = c(plotRestoreParams, param)
    }
  }
  
  trackRestoreParams = paste0("b", trackRestoreParams)
  plotRestoreParams = paste0("b", plotRestoreParams)
  
  output = coda.samples.dic(model = model,
                            variable.names=c("b0", paste0("b", params), trackRestoreParams, "logWET.p"),
                            n.iter=iters,
                            thin=thin)
  output$data = data
  
  save(output, file=sprintf("Results/%s.RData", modelName))
  write(sprintf("%s\t%s", modelName, output$dic$deviance + output$dic$penalty),
        file = "Results/DIC.txt",
        append = T)
  
  png(sprintf("Results/Plots/%s.png", modelName))
  MCMCplot(output$samples, ref_ovl = T, params=c("b0", paste0("b", params), plotRestoreParams))
  dev.off()
  
  return(output)
}




# Run SLR Scenarios -------------------------------------------------------
RCPscens = read.delim("out.txt")



predPost = RunRestorationModel(F, F, 10, 58, T, F, F)
rcp3_2100 = RunRestorationModel(F, F, 10, 58, T, F, F, RSLRscen = c(RCPscens$RCP3_2100.mu, RCPscens$RCP3_2100.sd), nameMod = "RCP3_2100")
rcp3_2300 = RunRestorationModel(F, F, 10, 58, T, F, F, RSLRscen = c(RCPscens$RCP3_2300.mu, RCPscens$RCP3_2300.sd), nameMod = "RCP3_2300")
rcp85_2100 = RunRestorationModel(F, F, 10, 58, T, F, F, RSLRscen = c(RCPscens$RCP85_2100.mu, RCPscens$RCP3_2100.sd), nameMod = "RCP85_2100")
rcp85_2300 = RunRestorationModel(F, F, 10, 58, T, F, F, RSLRscen = c(RCPscens$RCP85_2300.mu, RCPscens$RCP3_2100.sd), nameMod = "RCP85_2300")

predPost_p = MCMCsummary(predPost$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param))
rcp3_2100_p = MCMCsummary(rcp3_2100$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param))
rcp3_2300_p = MCMCsummary(rcp3_2300$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param))
rcp85_2100_p = MCMCsummary(rcp85_2100$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param))
rcp85_2300_p = MCMCsummary(rcp85_2300$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param))

predPost_pred = predPost_p %>% dplyr::filter(grepl("logWET.p", param))
rcp3_2100_pred = rcp3_2100_p %>% dplyr::filter(grepl("logWET.p", param))
rcp3_2300_pred = rcp3_2100_p %>% dplyr::filter(grepl("logWET.p", param))
rcp85_2100_pred = rcp85_2100_p %>% dplyr::filter(grepl("logWET.p", param))
rcp85_2300_pred = rcp85_2300_p %>% dplyr::filter(grepl("logWET.p", param))

op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP3")
lines(density(predPost_pred$mean), col="black",lty=3)
lines(density(rcp3_2100_pred$mean), col="blue")
lines(density(rcp3_2300_pred$mean), col="red", lty=2)
legend(3, 0.8, c("Data", "PP", "2100", "2300"), col=c("black","black","blue","red"), lty=c(1,3,1,2), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5",lty=0)
lines(density(predPost_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_pred$mean), col="blue")
lines(density(rcp85_2300_pred$mean), col="red")
legend(3, 0.8, c("Data", "PP", "2100", "2300"), col=c("black","black","blue","red"), lty=c(1,3,1,1), cex=0.75)
par(op)





op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
lines(density(predPost_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_pred[rcp85_2100$data$region == 1,]$mean), col="blue")
lines(density(rcp85_2100_pred[rcp85_2100$data$region == 2,]$mean), col="red")
#lines(density(rcp85_2100_pred$mean), col="green")
legend(2, 0.8, c("PP", "W LA", "E LA"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_pred[rcp85_2300$data$region == 1,]$mean), col="blue")
lines(density(rcp85_2300_pred[rcp85_2300$data$region == 2,]$mean), col="red")
#lines(density(rcp85_2300_pred$mean), col="green")
legend(2, 0.8, c("PP", "W LA", "E LA"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
par(op)





op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
lines(density(predPost_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_pred[rcp85_2100$data$RESTORE == 1,]$mean), col="blue")
lines(density(rcp85_2100_pred[rcp85_2100$data$RESTORE == 0,]$mean), col="red")
#lines(density(rcp85_2100_pred$mean), col="green")
legend(2, 0.8, c("PP", "RESTORE", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_pred[rcp85_2300$data$RESTORE == 1,]$mean), col="blue")
lines(density(rcp85_2300_pred[rcp85_2300$data$RESTORE == 0,]$mean), col="red")
#lines(density(rcp85_2300_pred$mean), col="green")
legend(2, 0.8, c("PP", "RESTORE", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
par(op)





lines(density(rcp85_2100_pred[rcp85_2100$data$HA == 1,]$mean-predPost_pred[rcp85_2100$data$HA == 1,]$mean), col="blue")
plot(density(rcp85_2100_pred[rcp85_2100$data$HA == 0,]$mean-predPost_pred[rcp85_2100$data$HA == 0,]$mean), col="red")
#lines(density(rcp85_2100_pred$mean), col="green")
legend(0.3, 20, c("RESTORE", "NONE"), col=c("blue","red"), lty=1)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_pred[rcp85_2300$data$RESTORE == 1,]$mean), col="blue")
lines(density(rcp85_2300_pred[rcp85_2300$data$RESTORE == 0,]$mean), col="red")
#lines(density(rcp85_2300_pred$mean), col="green")
legend(3, 0.8, c("PP", "RESTORE", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1))



# Run Restoration Scenarios -----------------------------------------------

# Check to see which restoration methods are most influencing
MCMCplot(predPost$samples, params = c("bHA", "bMC", "bBW", "bVP"), ref_ovl=T)
# HA and BW are roughly equal significant negative; VP is roughly 80% CI significant negative

# Function to generate x% 1s

getPct1s = function(n, percent) {
  x = rep(0, n)
  x[sample(1:n, (percent/100) * n, FALSE)] = 1
  
  return(x)
}

# Increase HA Sites
{
  HA0count = thk99buff_n %>% filter(HA == 0) %>% nrow() # Grab # of HA == 0s
  
  # HA for 50 percent of unaltered sites
  thk99_HA50 = thk99buff_n
  thk99_HA50[thk99_HA50$HA == 0,]$HA = getPct1s(HA0count, 50)
  
  HA50pct = RunRestorationModel(F, F, 10, 58, T, F, F, data=thk99_HA50, nameMod = "HA50pct")
  HA50pct_p = MCMCsummary(HA50pct$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param))
  HA50pct_pred = HA50pct_p %>% dplyr::filter(grepl("logWET.p", param))
  
  # HA for 100 percent of unaltered sites
  thk99_HA100 = thk99buff_n
  thk99_HA100[thk99_HA100$HA == 0,]$HA = getPct1s(HA0count, 100)
  
  HA100pct = RunRestorationModel(F, F, 10, 58, T, F, F, data=thk99_HA100, nameMod = "HA100pct")
  HA100pct_p = MCMCsummary(HA100pct$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param))
  HA100pct_pred = HA100pct_p %>% dplyr::filter(grepl("logWET.p", param))
  
  plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "HA Increases")
  lines(density(predPost_pred$mean), col="black",lty=3)
  lines(density(HA50pct_pred$mean), col="blue")
  lines(density(HA100pct_pred$mean), col="blue", lty=2)
  legend(3, 0.8, c("Data", "PP", "HA 50%+", "HA100%"), col=c("black","black","blue","blue"), lty=c(1,3,1,2))
}


# Increase BW Sites
{
  BW0count = thk99buff_n %>% filter(BW == 0) %>% nrow() # Grab # of BW == 0s
  
  # HA for 50 percent of unaltered sites
  thk99_BW50 = thk99buff_n
  thk99_BW50[thk99_BW50$BW == 0,]$BW = getPct1s(BW0count, 50)
  
  BW50pct = RunRestorationModel(F, F, 10, 58, T, F, F, data=thk99_BW50, nameMod = "BW50pct")
  BW50pct_p = MCMCsummary(BW50pct$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param))
  BW50pct_pred = BW50pct_p %>% dplyr::filter(grepl("logWET.p", param))
  
  # BW for 100 percent of unaltered sites
  thk99_BW100 = thk99buff_n
  thk99_BW100[thk99_BW100$BW == 0,]$BW = getPct1s(BW0count, 100)
  
  BW100pct = RunRestorationModel(F, F, 10, 58, T, F, F, data=thk99_BW100, nameMod = "BW100pct")
  BW100pct_p = MCMCsummary(BW100pct$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param))
  BW100pct_pred = BW100pct_p %>% dplyr::filter(grepl("logWET.p", param))
  
  plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "BW Increases")
  lines(density(predPost_pred$mean), col="black",lty=3)
  lines(density(BW50pct_pred$mean), col="red")
  lines(density(BW100pct_pred$mean), col="red", lty=2)
  legend(3, 0.8, c("Data", "PP", "BW 50%+", "BW100%"), col=c("black","black","red","red"), lty=c(1,3,1,2))
}



# Run SLR Scenarios with Original Data ------------------------------------

thk99buff_la = thk99buff@data[thk99buff$region %in% c(9,10),]
thk99buff_la$region = thk99buff_la$region-8

# predPost_orig = RunRestorationModel(F, F, 10, 58, T, F, F,
#                                     data=thk99buff_la, iters = 600000, thin = 20, nameMod = "-od")
# rcp3_2100_orig = RunRestorationModel(F, F, 10, 58, T, F, F,
#                                     RSLRscen = c(RCPscens$RCP3_2100.mu, RCPscens$RCP3_2100.sd), nameMod = "-RCP3_2100-od",
#                                     data=thk99buff_la)
# rcp3_2300_orig = RunRestorationModel(F, F, 10, 58, T, F, F,
#                                     RSLRscen = c(RCPscens$RCP3_2300.mu, RCPscens$RCP3_2300.sd), nameMod = "-RCP3_2300-od",
#                                     data=thk99buff_la)
# rcp85_2100_orig = RunRestorationModel(F, F, 10, 58, T, F, F,
#                                     RSLRscen = c(RCPscens$RCP85_2100.mu, RCPscens$RCP3_2100.sd), nameMod = "-RCP85_2100-od",
#                                     data=thk99buff_la)
# rcp85_2300_orig = RunRestorationModel(F, F, 10, 58, T, F, F,
#                                     RSLRscen = c(RCPscens$RCP85_2300.mu, RCPscens$RCP3_2100.sd), nameMod = "-RCP85_2300-od",
#                                     data=thk99buff_la)
source("../../RUtilityFunctions/bayesianTools.R")
predPost_orig = getCI("58-Any-10y-Y-od")
rcp3_2100_orig = getCI("58-Any-10y-Y-RCP3_2100-od")
rcp3_2300_orig = getCI("58-Any-10y-Y-RCP3_2300-od")
rcp85_2100_orig = getCI("58-Any-10y-Y-RCP85_2100-od")
rcp85_2300_orig = getCI("58-Any-10y-Y-RCP85_2300-od")

predPost_orig_pred = MCMCsummary(predPost_orig$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp3_2100_orig_pred = MCMCsummary(rcp3_2100_orig$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp3_2300_orig_pred = MCMCsummary(rcp3_2300_orig$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp85_2100_orig_pred = MCMCsummary(rcp85_2100_orig$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp85_2300_orig_pred = MCMCsummary(rcp85_2300_orig$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))

op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP3",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp3_2100_orig_pred$mean), col="blue")
lines(density(rcp3_2300_orig_pred$mean), col="red", lty=2)
legend(3, 0.8, c("Base", "2100", "2300"), col=c("black","blue","red"), lty=c(3,1,2), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_orig_pred$mean), col="blue")
lines(density(rcp85_2300_orig_pred$mean), col="red",lty=2)
legend(3, 0.8, c("Base", "2100", "2300"), col=c("black","blue","red"), lty=c(3,1,2), cex=0.75)
par(op)

op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
lines(density(rcp85_2100_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$region == 1,]$mean), col="blue")
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$region == 2,]$mean), col="red")
legend(2, 0.8, c("Combined", "W LA", "E LA"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$region == 1,]$mean), col="blue")
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$region == 2,]$mean), col="red")
legend(2, 0.8, c("Combined", "W LA", "E LA"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
par(op)





op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$RESTORE == 1,]$mean), col="blue")
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$RESTORE == 0,]$mean), col="red")
#lines(density(rcp85_2100_pred$mean), col="green")
legend(2, 0.8, c("Combined", "RESTORE", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$RESTORE == 1,]$mean), col="blue")
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$RESTORE == 0,]$mean), col="red")
#lines(density(rcp85_2300_pred$mean), col="green")
legend(2, 0.8, c("Combined", "RESTORE", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
par(op)

op=par(mfrow=c(2,2))
# op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$HA == 1,]$mean), col="blue")
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$HA == 0,]$mean), col="red")
#lines(density(rcp85_2100_pred$mean), col="green")
legend(2, 0.8, c("Combined", "HA", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$HA == 1,]$mean), col="blue")
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$HA == 0,]$mean), col="red")
#lines(density(rcp85_2300_pred$mean), col="green")
legend(2, 0.8, c("Combined", "HA", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# par(op)
# 
# 
# op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$BW == 1,]$mean), col="blue")
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$BW == 0,]$mean), col="red")
#lines(density(rcp85_2100_pred$mean), col="green")
legend(2, 0.8, c("Combined", "BW", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$BW == 1,]$mean), col="blue")
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$BW == 0,]$mean), col="red")
#lines(density(rcp85_2300_pred$mean), col="green")
legend(2, 0.8, c("Combined", "BW", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
par(op)

MCMCplot(predPost_orig$samples, params=c(paste0("b", c(params, restoreParams)), "bYEAR"), ref_ovl=T)
MCMCtrace(predPost_orig$samples, params=c(paste0("b", c(params, restoreParams)), "bYEAR"))
