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
predPostArea = c(
                  "        logWET.p[i] ~ dnorm(logWET.mu[i], logWET.tau)"
                 #, "        sq[i] <- (logWET[i]-logWET.mu[i])^2",
                 #, "        sq.p[i] <- (logWET.p[i]-logWET.mu[i])^2")
                )

pvalTrackArea = paste0("\t", c(
                    #  "p.min <- step(min(logWET.p[]) - min(logWET[]))",
                    #, "p.max <- step(max(logWET.p[]) - max(logWET[]))",
                    #, "p.range <- step((max(logWET.p[])-min(logWET.p[]))-(max(logWET[])-min(logWET[])))",
                    #, "p.fit <- step(sum(sq.p[]) - sum(sq[]))"
                    "p.mean <- step(mean(logWET.p[]) - mean(logWET[]))",
                    "p.sd <- step(sd(logWET.p[]) - sd(logWET[]))",
                    "p.cv <- step(sd(logWET.p[])/(mean(logWET.p[])+smallNum) - sd(logWET[])/(mean(logWET[])+smallNum))"
                  ))
# p.fit is discrepancy from https://github.com/CCheCastaldo/SESYNCBayes/blob/master/Lecture/ModelChecking.pdf

pvalTracked = c("p.mean", "p.sd", "p.cv")
pvalLabels = c("PMean", "PSD", "PCV")

areaPvalModelDir = sprintf("Results_03_2020/Models/Pval-%s", areaModelName)
areaPvalResultDir = sprintf("Results_03_2020/Results/Pval-%s", areaModelName)

# Create model files for predictive posterior -----------------------------

# Area Models

if (!dir.exists(areaPvalModelDir))
{
  dir.create(areaPvalModelDir)
}

# areaModels=combineDIC(areaModelName)
# areaModels=areaModels[c("modelNo","fixed","random","DIC","sig","type")]
# modelNums = areaModels$modelNo
modelNums = 1:484 #c(58,146,145,241,161,388,483,300,387,403)

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

# Run models to obtain pvalues --------------------------------------------
params = c("RSLR","WH","TR","CS", "NDVI")

# Area Models
if (!dir.exists(areaPvalResultDir))
{
  dir.create(areaPvalResultDir)
}

bugs.step = function(x) {
  if (x >= 0) {
    return(1)
  } else {
    return(0)
  }
}

data = append(thk99buff@data, list(Nregion=14, Nobs = nrow(thk99buff@data), smallNum = 0.000001))
for (modelNo in modelNums)
{
  model = jags.model(sprintf("%s/%s.txt", areaPvalModelDir, modelNo),
                     data = data,
                     n.chains=3,
                     n.adapt=2000)
  
  output = coda.samples.dic(model = model,
                            variable.names=c(pvalTracked),#, paste0("b", params)),
                            n.iter=200000,
                            thin=1)
  
  save(output,file=sprintf("%s/%s.RData", areaPvalResultDir, modelNo))
}

# P-Values ----------------------------------------------------------------
pMeans = rep(NA, length(modelNums))
pSds = rep(NA, length(modelNums))
pCvs = rep(NA, length(modelNums))

for (modelNo in modelNums) {
  load(sprintf("%s/%s.RData", areaPvalResultDir, modelNo))
  summ = MCMCsummary(output$samples, params=pvalTracked)
  rowNames = row.names(summ)
  pMeans[modelNo] = summ[which(rowNames=="p.mean"),1]
  pSds[modelNo] = summ[which(rowNames=="p.sd"),1]
  pCvs[modelNo] = summ[which(rowNames=="p.cv"),1]
}

modelOrder = read.delim("Results_03_2020/area-models_DIC_PPL_03292020_full.txt")$modelNo
out = data.frame(modelNo=modelOrder,p.mean=pMeans[modelOrder], p.sd=pSds[modelOrder], p.cv=pCvs[modelOrder])
write.table(out, file="Results_03_2020/pvals.txt", row.names=F, quote=F, sep="\t")