source("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/RUtilityFunctions/bayesianTools.R")

a=combineDIC("logWET-14R-NDVI", top=NA)
a=a[c("modelNo","fixed","random","DIC","sig","type")]

b=combineDIC("logPCT-14R-NDVI", top=NA)
b=b[c("modelNo","fixed","random","DIC","sig","type")]

write.table(a, "Figures/AppendixS1.txt", row.names=F, quote=F, sep="\t")
write.table(b, "Figures/AppendixS2.txt", row.names=F, quote=F, sep="\t")
