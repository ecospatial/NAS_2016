wd=getwd()
source("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/RUtilityFunctions/bayesianTools.R")


# Compare True Rand Effs to Fixed Effects ("False Random") ----------------
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel/TrueRandEffs")
dic1=DICexamine("TRE)logPCT~RSLR.WH.TR.CS.NDVI-3Regions-rB0", top=10)
getCIs(195, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-3Regions-rB0")

setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel/New")
dic2=DICexamine("logPCT~RSLR.WH.TR.CS.NDVI-3Regions-rB0", top=10)
getCIs(195, "logPCT~RSLR.WH.TR.CS.NDVI-3Regions-rB0")

dicm = merge(dic2,dic1,by="modelNo")
dicm = dicm[order(dicm$DIC.y),]

plot(DIC.y~DIC.x, data=dicm, xlab="Fixed Effect (\"False Random\")", ylab="True Mixed Effect", xlim=c(3192,3199), ylim=c(3192,3199))
abline(0,1)


# 15 Regions --------------------------------------------------------------
plotRegions = function(regions){
  plot(thk99buff)
  plot(huc4[huc4$HUC4 %in% unique(thk99buff$HUC4),], add=T)
  
  ptsInRegions = thk99buff[thk99buff$region %in% regions,]
  regionHUCs = unique(ptsInRegions$HUC4)
  regionShapes = huc4[huc4$HUC4 %in% regionHUCs,]
  
  plot(regionShapes, add=T, col="green")
  regionKey = data.frame(HUC4=factor(ptsInRegions$HUC4), region=as.numeric(ptsInRegions$region))
  regionKey = unique(regionKey)
  
  #text(coordinates(regionShapes), labels = regionShapes$HUC4)
  text(coordinates(regionShapes), labels = merge(regionShapes, regionKey, by="HUC4")$region)
}

setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel/TrueRandEffs")
dic3=DICexamine("TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", top=10)

getCIs(195, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0")
getCIs(195, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", pars = "b0\\[.*\\]")

getCIs(203, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", regex_pars = "bWH.*")

# regionNums = (1:15)[-11]
regionNums = c(1,3,5,6,10,13)
pars = paste0("bWH[", regionNums, "]")
getCIs(203, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", pars=pars)
plotRegions(regionNums)

# Region 11 (low sample size n=1)
plot(huc4[as.character(huc4$HUC4) == "1203",])
plot(huc4[huc4$HUC4 %in% unique(thk99buff$HUC4),], add=T)
plot(thk99buff, add=T,col="green")

# Region counts
table(thk99buff$region)
cols = rep("gray", length(unique(thk99buff$region)))
cols[regionNums] = "green"
xx=barplot(table(thk99buff$region), col=cols)
text(x=xx, y=as.numeric(table(thk99buff$region)), labels=ns, pos=4)

# Model compare
dic3$modelNo %in% dicm$modelNo
dic3[dic3$modelNo %in% dicm$modelNo,]
dic3[!dic3$modelNo %in% dicm$modelNo,]

# Visualize new regional models
regionNums = c(1,3,5,6,10,13)
pars = paste0("bWH[", regionNums, "]")
getCIs(219, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", pars=pars)
plotRegions(regionNums)
