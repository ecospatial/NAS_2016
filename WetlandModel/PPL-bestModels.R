source("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/RUtilityFunctions/bayesianTools.R")
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel")
library(MCMCvis)


# CONFIG ------------------------------------------------------------------
areaModelName = "logWET-14R-NDVI"
pctModelName = "logPCT-14R-NDVI"
predPostArea = "        logWET.p[i] ~ dnorm(logWET.mu[i], logWET.tau)"
predPostPct = "        logPCT.p[i] ~ dnorm(logPCT.mu[i], logPCT.tau)"

areaPPmodelDir = sprintf("Results_03_2020/Models/PP-%s", areaModelName)
pctPPmodelDir = sprintf("Results_03_2020/Models/PP-%s", pctModelName)

areaPPresultDir = sprintf("Results_03_2020/Results/PP-%s", areaModelName)
areaSummaryFilePath = sprintf("%s/DICPPL_PP-%s.txt", areaPPresultDir, areaModelName)
pctPPresultDir = sprintf("Results_03_2020/Results/PP-%s", pctModelName)
pctSummaryFilePath = sprintf("%s/DICPPL_PP-%s.txt", pctPPresultDir, pctModelName)


# Create model files for predictive posterior -----------------------------

# Area Models
areaModels=combineDIC(areaModelName,top=NA)
areaModels=areaModels[c("modelNo","fixed","random","DIC","sig","type")]

if (!dir.exists(areaPPmodelDir))
{
  dir.create(areaPPmodelDir)
}

for (i in areaModels$modelNo) # For the models listed in the top 10; but you can specify any model numbers here
{
  intercept = "-rB0"
  modelNo = i
  if (i > 242)
  {
    intercept = ""
    modelNo = i - 242
  }
  
  filePath = sprintf("Models/%s%s/%s.txt", areaModelName, intercept, modelNo)
  
  if (!file.exists(filePath))
  {
    stop(sprintf("Error: %s model %s file does not exist", areaModelName, modelNo))
  }
  
  modelText = readLines(filePath)
  modelText = append(modelText, predPostArea, after=4) # Add in predictive posterior after line 4 which is the response distribution
  
  newFilePath = sprintf("%s/%s.txt", areaPPmodelDir, i)
  write(modelText, newFilePath)
}

# Percent Models
pctModels=combineDIC(pctModelName,top=NA)
pctModels=pctModels[c("modelNo","fixed","random","DIC","sig","type")]

if (!dir.exists(pctPPmodelDir))
{
  dir.create(pctPPmodelDir)
}

for (i in pctModels$modelNo) # For the models listed in the top 10; but you can specify any model numbers here
{
  intercept = "rB0"
  modelNo = i
  if (i > 242)
  {
    intercept = ""
    modelNo = i - 242
  }
  
  filePath = sprintf("Models/%s/%s.txt", pctModelName, modelNo)
  
  if (!file.exists(filePath))
  {
    stop(sprintf("Error: %s model %s file does not exist", pctModelName, modelNo))
  }
  
  modelText = readLines(filePath)
  modelText = append(modelText, predPostPct, after=4) # Add in predictive posterior after line 4 which is the response distribution
  
  newFilePath = sprintf("%s/%s.txt", pctPPmodelDir, i)
  write(modelText, newFilePath)
}


# Run models to obtain predictive posterior -------------------------------

### SOURCE LINES 16-210 FROM modelManyRuns.R for function 'runModel' to obtain data.###

params = c("RSLR","WH","TR","CS", "NDVI")

# Area Models
if (!dir.exists(areaPPresultDir))
{
  dir.create(areaPPresultDir)
}

write(paste0(c("modelNo", "DIC", "PPL"), collapse="\t"), file = areaSummaryFilePath)

data = runModel(list(response="logWET", regions="ALL", vegIdx="NDVI", intercept=NULL), getData=T) # Intercept doesn't matter because
                                                                                                  # data is returned before it's used
for (modelNo in areaModels$modelNo)
{
  model = jags.model(sprintf("%s/%s.txt", areaPPmodelDir, modelNo),
                     data = data,
                     n.chains=3,
                     n.adapt=2000)
  
  output = coda.samples.dic(model = model,
                            variable.names=c("b0", paste0("b", params), "logWET.p"),
                            n.iter=20000,
                            thin=1)
  
  save(output,file=sprintf("%s/%s.RData", areaPPresultDir, modelNo))
  
  PPL = sum((data$logWET - as.data.frame(MCMCsummary(output$samples, params="logWET.p"))$mean)^2)
  
  write(paste0(c(modelNo, (output$dic$deviance + output$dic$penalty), PPL), collapse = "\t"),
        file = areaSummaryFilePath,
        append = T)
}


rm(ls="data")

# Percent Models
if (!dir.exists(pctPPresultDir))
{
  dir.create(pctPPresultDir)
}

write(paste0(c("modelNo", "DIC", "PPL"), collapse="\t"), file = pctSummaryFilePath)

data = runModel(list(response="logPCT", regions="ALL", vegIdx="NDVI", intercept=NULL), getData=T) # Intercept doesn't matter because
# data is returned before it's used
for (modelNo in pctModels$modelNo)
{
  model = jags.model(sprintf("%s/%s.txt", pctPPmodelDir, modelNo),
                     data = data,
                     n.chains=3,
                     n.adapt=2000)
  
  output = coda.samples.dic(model = model,
                            variable.names=c("b0", paste0("b", params), "logPCT.p"),
                            n.iter=20000,
                            thin=1)
  
  save(output,file=sprintf("%s/%s.RData", pctPPresultDir, modelNo))
  
  PPL = sum((data$logPCT - as.data.frame(MCMCsummary(output$samples, params="logPCT.p"))$mean)^2)
  
  write(paste0(c(modelNo, (output$dic$deviance + output$dic$penalty), PPL), collapse = "\t"),
        file = pctSummaryFilePath,
        append = T)
}




# DIC vs PPL --------------------------------------------------------------
areaDICppl = read.delim(areaSummaryFilePath, sep = "\t")
pctDICppl = read.delim(pctSummaryFilePath, sep = "\t")

areaDICppl
pctDICppl

op = par(mfrow=c(1,2))
plot(DIC~PPL, areaDICppl)
plot(DIC~PPL, pctDICppl)
par(op)
