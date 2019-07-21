source("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/RUtilityFunctions/bayesianTools.R")
source("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/RUtilityFunctions/codaSamplesDIC.R")
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel")
library(MCMCvis)
library(rjags)

# source("loadTHK99.R")
# loadTHK99data(local=T,regions="ALL")

###### BETTER METHOD??? ######
# https://www4.stat.ncsu.edu/~reich/ABA/code/checks_guns

# CONFIG ------------------------------------------------------------------
areaModelName = "logWET-14R-NDVI"
pctModelName = "logPCT-14R-NDVI"
predPostArea = c("        logWET.p[i] ~ dnorm(logWET.mu[i], logWET.tau)",
                 "        sq[i] <- (logWET[i]-logWET.mu[i])^2",
                 "        sq.p[i] <- (logWET.p[i]-logWET.mu[i])^2")
predPostPct = gsub("WET", "PCT", predPostArea)

pvalTrackArea = paste0("\t", c("p.min <- step(min(logWET.p[]) - min(logWET[]))",
                  "p.max <- step(max(logWET.p[]) - max(logWET[]))",
                  "p.range <- step((max(logWET.p[])-min(logWET.p[]))-(max(logWET[])-min(logWET[])))",
                  "p.mean <- step(mean(logWET.p[]) - mean(logWET[]))",
                  "p.sd <- step(sd(logWET.p[]) - sd(logWET[]))",
                  "p.fit <- step(sum(sq.p[]) - sum(sq[]))"))
# p.fit is discrepancy from https://github.com/CCheCastaldo/SESYNCBayes/blob/master/Lecture/ModelChecking.pdf
pvalTrackPct = gsub("WET", "PCT", pvalTrackArea)

pvalNames = c("p.min", "p.max", "p.range", "p.mean", "p.sd", "p.fit")
pvalLabels = c("Min", "Max", "Range", "Mean", "SD", "Discrepancy")

areaPvalModelDir = sprintf("Models/Pval-%s", areaModelName)
pctPvalModelDir = sprintf("Models/Pval-%s", pctModelName)

areaPvalResultDir = sprintf("Results/Pval-%s", areaModelName)
pctPvalResultDir = sprintf("Results/Pval-%s", pctModelName)


# Create model files for predictive posterior -----------------------------

# Area Models

if (!dir.exists(areaPvalModelDir))
{
  dir.create(areaPvalModelDir)
}

# areaModels=combineDIC(areaModelName)
# areaModels=areaModels[c("modelNo","fixed","random","DIC","sig","type")]
# modelNums = areaModels$modelNo
modelNums = c(58)#,146,145,241,161)

for (i in modelNums) # For the models listed in the top 10; but you can specify any model numbers here
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
  modelText = append(modelText, pvalTrackArea, after=length(modelText)-1)
  
  newFilePath = sprintf("%s/%s.txt", areaPvalModelDir, i)
  write(modelText, newFilePath)
}

# Percent Models

if (!dir.exists(pctPvalModelDir))
{
  dir.create(pctPvalModelDir)
}

# pctModels=combineDIC(pctModelName)
# pctModels=pctModels[c("modelNo","fixed","random","DIC","sig","type")]
# modelNums2 = pctModels$modelNo
modelNums2 = c(452)#,450,210,468)

for (i in modelNums2) # For the models listed in the top 10; but you can specify any model numbers here
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
  
  newFilePath = sprintf("%s/%s.txt", pctPvalModelDir, i)
  write(modelText, newFilePath)
}

# Run models to obtain pvalues --------------------------------------------
params = c("RSLR","WH","TR","CS", "NDVI")

# Area Models
if (!dir.exists(areaPvalResultDir))
{
  dir.create(areaPvalResultDir)
}

data = append(thk99buff@data, list(Nregion=14, Nobs = nrow(thk99buff@data)))
for (modelNo in modelNums)
{
  model = jags.model(sprintf("%s/%s.txt", areaPvalModelDir, modelNo),
                     data = data,
                     n.chains=3,
                     n.adapt=2000)
  
  output = coda.samples.dic(model = model,
                            variable.names=c(pvalNames, paste0("b", params)),
                            n.iter=200000,
                            thin=1)
  
  save(output,file=sprintf("%s/%s.RData", areaPvalResultDir, modelNo))
}

# Percent Models
if (!dir.exists(pctPvalResultDir))
{
  dir.create(pctPvalResultDir)
}

# data is returned before it's used
for (modelNo in modelNums2)
{
  model = jags.model(sprintf("%s/%s.txt", pctPvalModelDir, modelNo),
                     data = data,
                     n.chains=3,
                     n.adapt=2000)
  
  output = coda.samples.dic(model = model,
                            variable.names=c(pvalNames, paste0("b", params)),
                            n.iter=20000,
                            thin=1)
  
  save(output,file=sprintf("%s/%s.RData", pctPvalResultDir, modelNo))
}

# P-Values ----------------------------------------------------------------
Dnames <- c("Min Y", "Max Y", "Range Y", "Min rate", "Max rate", "Range rate")

load(sprintf("%s/%s.RData", areaPvalResultDir, 58))
MCMCsummary(output$samples, params=pvalNames)

pMeans = c()
pSds = c()
for(m in modelNums) {
  pMeans = c(pMeans, MCMCsummary(output$samples, params="p.mean")[1])
  pSds = c(pSds, MCMCsummary(output$samples, params="p.sd")[1])
}
pMeans
pSds

plot(density(c(MCMCchains(output$samples, params="p.fit"))))




load(sprintf("%s/%s.RData", pctPvalResultDir, 452))
MCMCsummary(output$samples, params=pvalNames)

pMeans = c()
pSds = c()
for(m in modelNums2) {
  pMeans = c(pMeans, MCMCsummary(output$samples, params="p.mean")[1])
  pSds = c(pMeans, MCMCsummary(output$samples, params="p.sd")[1])
}
pMeans
pSds



