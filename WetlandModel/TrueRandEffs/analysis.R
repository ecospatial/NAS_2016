wd=getwd()
setwd("WetlandModel/TrueRandEffs")

source(paste0(wd,"/RUtilityFunctions/bayesianTools.R"))

modelName="TRE)logPCT~RSLR.WH.TR.CS.NDVI-3Regions-rB0"
dic=DICexamine(modelName,top=10)
dic

getCIs(195, modelName)
