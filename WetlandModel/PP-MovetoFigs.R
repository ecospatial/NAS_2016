library(MCMCvis)
library(magrittr)
source("loadTHK99.R")


# Load Data ---------------------------------------------------------------
areaModelName = "logWET-14R-NDVI"
pctModelName = "logPCT-14R-NDVI"

areaPPresultDir = sprintf("Results/PP-%s", areaModelName)
pctPPresultDir = sprintf("Results/PP-%s", pctModelName)

areaModelNo = 58
pctModelNo = 452

areaPPresultFile = sprintf("%s/%s.RData", areaPPresultDir, areaModelNo)
pctPPresultFile = sprintf("%s/%s.RData", pctPPresultDir, pctModelNo)

load(areaPPresultFile)
areaPP = output
load(pctPPresultFile)
pctPP = output
rm(ls="output")

areaPPsummary = MCMCsummary(areaPP$samples, params=c("logWET.p")) %>% data.frame()
names(areaPPsummary) = c("mean", "sd", "q2.5", "median", "q97.5", "Rhat")
pctPPsummary = MCMCsummary(pctPP$samples, params=c("logPCT.p")) %>% data.frame()
names(pctPPsummary) = c("mean", "sd", "q2.5", "median", "q97.5", "Rhat")
loadTHK99data(local=T, regions="ALL")

# Area Pred vs Obs Plot ---------------------------------------------------
rnge = c(-max(range(thk99buff$logWET)),max(range(thk99buff$logWET)))

plot(1~1, type="n",
     xlab = "Observed",
     ylab = "Predicted",
     xlim = range(thk99buff$logWET),
     ylim = rnge)
abline(0,1)

arrows(thk99buff$logWET, areaPPsummary$q2.5, thk99buff$logWET, areaPPsummary$q97.5, length=0.05, angle=90, code=3)
points(areaPPsummary$median~thk99buff$logWET, pch=21, col="black", bg="yellow")


# Percent Pred vs Obs Plot ------------------------------------------------
rnge = c(-max(range(thk99buff$logPCT)),max(range(thk99buff$logPCT)))

plot(1~1, type="n",
     xlab = "Observed",
     ylab = "Predicted",
     xlim = c(-9, 0),
     ylim = c(-9, 0))
abline(0,1)

arrows(thk99buff$logPCT, pctPPsummary$q2.5, thk99buff$logPCT, pctPPsummary$q97.5, length=0.05, angle=90, code=3)
points(pctPPsummary$median~thk99buff$logPCT, pch=21, col="black", bg="yellow")