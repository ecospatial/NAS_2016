library(MCMCvis)
source("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/RUtilityFunctions/bayesianTools.R")
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel")

# Write Signifs to File ---------------------------------------------------
a=combineDIC("logWET-14R-NDVI")
a=a[c("modelNo","fixed","random","DIC","sig","type")]
a
MCMCplot(getCI(58, "logWET-14R-NDVI")$samples, params = "bRSLR", ref_ovl=T)

b=combineDIC("logPCT-14R-NDVI")
b=b[c("modelNo","fixed","random","DIC","sig","type")]
b
op=par(mfrow=c(2,2))
MCMCplot(getCI(210, "logPCT-14R-NDVI")$samples, ref_ovl=T)
MCMCplot(getCI(210, "logPCT-14R-NDVI")$samples, params = "bNDVI", ref_ovl=T)
par(op)

b2=combineDIC("logPCT-14R-NDVI", omit="CS")
b2=b2[c("modelNo","fixed","random","DIC","sig","type")]
MCMCplot(getCI(154, "logPCT-14R-NDVI")$samples, ref_ovl=T, params= "bRSLR")



#NDMI
c=combineDIC("logWET-14R-NDMI")
c=c[c("modelNo","fixed","random","DIC","sig","type")]
c
MCMCplot(getCI(146, "logWET-14R-NDMI")$samples, params = "bRSLR", ref_ovl=T)

d=combineDIC("logPCT-14R-NDMI")
d=d[c("modelNo","fixed","random","DIC","sig","type")]
d
op=par(mfrow=c(2,2))
MCMCplot(getCI(210, "logPCT-14R-NDVI")$samples, ref_ovl=T)
