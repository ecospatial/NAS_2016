wd=getwd()
source("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/RUtilityFunctions/bayesianTools.R")

# Compare True Rand Effs to Fixed Effects ("False Random") ----------------
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel/TrueRandEffs")
dic1=DICexamine("TRE)logPCT~RSLR.WH.TR.CS.NDVI-3Regions-rB0", top=10)
getCIs(195, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-3Regions-rB0")

setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel/New")
dic2=DICexamine("logPCT~RSLR.WH.TR.CS.NDVI-3Regions-rB0", top=10)
getCIs(195, "logPCT~RSLR.WH.TR.CS.NDVI-3Regions-rB0")
getCIs(197, "logPCT~RSLR.WH.TR.CS.NDVI-3Regions-rB0")

dicm = merge(dic2,dic1,by="modelNo")
dicm = dicm[order(dicm$DIC.y),]

plot(DIC.y~DIC.x, data=dicm, xlab="Fixed Effect (\"False Random\")", ylab="True Mixed Effect")#, xlim=c(3192,3199), ylim=c(3192,3199))
abline(0,1)


# 15 Regions --------------------------------------------------------------
plotRegions = function(regions=NA, states=F){
  op=par(oma=c(0,0,0,0), mar=c(0,0,0,0))
  
  plot(thk99buff, border=NA, col=NA)
  
  if (states)
  {
    if (!exists("stateMap"))
    {
      stateMap <<- readOGR("C:/DATA/General Maps/Gulf States/US_GulfStates.shp", "US_GulfStates")
    }
    plot(stateMap, add=T, lty=2)
  }
  
  plot(HUC[HUC$HUC4 %in% unique(thk99buff$HUC4),], add=T)
  
  if (!is.na(regions))
    ptsInRegions = thk99buff[thk99buff$region %in% regions,]
  else
    ptsInRegions = thk99buff
  
  regionHUCs = unique(ptsInRegions$HUC4)
  regionShapes = HUC[HUC$HUC4 %in% regionHUCs,]
  
  plot(regionShapes, add=T, col=NA)#"#00FF0044")
  regionKey = data.frame(HUC4=factor(ptsInRegions$HUC4), region=as.numeric(ptsInRegions$region))
  regionKey = unique(regionKey)
  
  text(coordinates(regionShapes), labels = merge(regionShapes, regionKey, by="HUC4")$region)

  par(op)
}

setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel/TrueRandEffs")
dic3=DICexamine("TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", top=10)

getCIs(195, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0")
getCIs(195, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", regex_pars = "b0.*")

dic3[dic3$modelNo==199,][c("sig","non")]
getCIs(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", regex_pars = "bTR.*")
plotRegions(c(3,6,9))

getCIs(203, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", regex_pars = "bWH.*") # NOT IN TOP 2 DIC

# Exploration of model 203, NOT IN TOP 2 DIC
# # regionNums = (1:15)[-11]
# regionNums = c(1,3,5,6,10,13)
# pars = paste0("bWH[", regionNums, "]")
# getCIs(203, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", pars=pars)
# plotRegions(regionNums)

# Region 11 (low sample size n=1)
op=par(bg="lightblue",oma=rep(0,4),mar=rep(0,4))
probPt = which(thk99buff$region == 11)
pts = (probPt-30):(probPt+30)
plot(thk99buff[pts,], border=NA)
plot(HUC, add=T, col="lightgray")
plot(HUC[HUC$HUC4 %in% unique(thk99buff$HUC4),], add=T, col="gray")
plot(HUC[as.character(HUC$HUC4) == as.character(thk99buff[probPt,]$HUC4),], add=T, col="yellow")
plot(thk99buff, add=T,col="blue", border=NA)
plot(thk99buff[probPt,], col="red", border=NA, add=T)
par(op)

# Region counts
table(thk99buff$region)
cols = rep("gray", length(unique(thk99buff$region)))
cols[regionNums] = "green"
xx=barplot(table(thk99buff$region), col=cols, xlab="# of Points in Region (n)", ylab="Region", ylim=c(0, max(as.numeric(table(thk99buff$region)))+40))
text(x=xx, y=as.numeric(table(thk99buff$region)), labels=as.numeric(table(thk99buff$region)), pos=3)

# Model compare
dic3$modelNo %in% dicm$modelNo
dic3[dic3$modelNo %in% dicm$modelNo,]
dic3[!dic3$modelNo %in% dicm$modelNo,]

# 15Regions rB0 -----------------------------------------------------------
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel/TrueRandEffs")
dic4=DICexamine("TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", top=10)

getCIs(195, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0")

getCIs(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", regex_pars="bTR.*")
regionNums = c(3,6,9)
# pars = paste0("bTR[", regionNums, "]")
# getCIs(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", pars=pars)
plotRegions(regionNums)


# 15Regions no rB0 --------------------------------------------------------
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel/TrueRandEffs")
dic5=DICexamine("TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions", top=10)

getCIs(195, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0")

getCIs(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions", regex_pars="bTR.*")
regionNums = c(3,6,9)
pars = paste0("bTR[", regionNums, "]")
getCIs(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions", pars=pars)
plotRegions(regionNums)

dicm2 = merge(dic4, dic5, by="modelNo")
plot(DIC.y~DIC.x, data=dicm2, xlab="Random Intercept", ylab="No Random Intercept", xlim=c(3192,3199), ylim=c(3192,3199))
abline(0,1)



# 15Region Compare rB0 vs Non ---------------------------------------------
getCIs(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", xlim=c(-4,4))
getCIs(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions", xlim=c(-4,4))

getCI(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", "bNDVI")
getCI(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions", "bNDVI")
getCI(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", "bCS")
getCI(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions", "bCS")
getCI(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions-rB0", "bWH")
getCI(199, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-15Regions", "bWH")


# 15Region logWET ---------------------------------------------------------
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel/TrueRandEffs")
dic6=DICexamine("TRE)logWET~RSLR.WH.TR.CS.NDVI-15Regions-rB0", top=10)
getCIs(60, "TRE)logWET~RSLR.WH.TR.CS.NDVI-15Regions-rB0")
getCIs(60, "TRE)logWET~RSLR.WH.TR.CS.NDVI-15Regions-rB0", regex_pars="b0.*")
plotRegions(c(3,8))
plotRegions()
getCIs(60, "TRE)logWET~RSLR.WH.TR.CS.NDVI-15Regions-rB0", regex_pars="bCS.*")
plotRegions(c(1,3,5,6,8,10))

getCIs(165, "TRE)logWET~RSLR.WH.TR.CS.NDVI-15Regions-rB0", omit="b0")

dic7=DICexamine("TRE)logWET~RSLR.WH.TR.CS.NDVI-15Regions", top=10)
getCIs(60, "TRE)logWET~RSLR.WH.TR.CS.NDVI-15Regions-rB0")

dicm3 = merge(dic6, dic7, by="modelNo")
plot(DIC.y~DIC.x, data=dicm3, xlab="Random Intercept", ylab="No Random Intercept", xlim=c(2819,2826), ylim=c(2819,2826))
abline(0,1)


# Old 2/3 Region Models ---------------------------------------------------
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel/New")
dicold1 = DICexamine("logPCT~RSLR.WH.TR.CS.NDMI", noSig = T)
dicold2 = DICexamine("logPCT~RSLR.WH.TR.CS.NDVI", noSig = T)
dicold3 = DICexamine("logPCT~RSLR.WH.TR.CS.NDVI-3Regions", noSig = T)
dicold4 = DICexamine("logWET~RSLR.WHsq.TR.CS.NDVI", noSig = T)
dicold5 = DICexamine("logWET~RSLRsq.WH.TR.CS.NDVI", noSig = T)


dicold3 = DICexamine("logPCT~RSLR.WH.TR.CS.NDVI-3Regions")


# Write Signifs to File ---------------------------------------------------
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel")
s1=outputSignifs("TRE)logWET~RSLR.WH.TR.CS.NDVI-14Regions-rB0")
s2=outputSignifs("TRE)logWET~RSLR.WH.TR.CS.NDVI-14Regions")
s3=outputSignifs("TRE)logPCT~RSLR.WH.TR.CS.NDVI-14Regions-rB0")
s4=outputSignifs("TRE)logPCT~RSLR.WH.TR.CS.NDVI-14Regions")

write.table(s1,"WETrb0.txt", quote=F, sep="\t")
write.table(s2,"WET.txt", quote=F, sep="\t")
write.table(s3,"PCTrb0.txt", quote=F, sep="\t")
write.table(s4,"PCT.txt", quote=F, sep="\t")


getCIs(240, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-14Regions", regex_pars="bNDVI.*")
getCIs(240, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-14Regions", regex_pars="bRSLR.*")
getCIs(240, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-14Regions", regex_pars="bTR.*")
getCIs(240, "TRE)logPCT~RSLR.WH.TR.CS.NDVI-14Regions", regex_pars="bWH.*")


