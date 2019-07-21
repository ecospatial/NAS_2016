library(dplyr)
library(MCMCvis)
source("../../RUtilityFunctions/bayesianTools.R")

source("../loadTHK99.R")

toFile = T

# Load Data and Scenarios -------------------------------------------------
loadTHK99data(local=T, regions="ALL")
params = c("bWH","bRSLR","bTR")
restoreParams = c("bBW", "bVP", "bHA", "bMC")

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


# Figure 1 - DAG ----------------------------------------------------------
# In PPTX file

# Figure 2 - Restoration Posteriors ---------------------------------------
if (toFile){  png("Figures/Figure2.png", width = 4, height = 4, units = "in", res = 300)  }
labs = c(paste0(rep(params, each=2), c("[W]", "[E]")), restoreParams, "bYEAR")
MCMCplot(predPost_orig$samples, params = c(params, restoreParams, "bYEAR"), ref_ovl = T, labels=labs)
if (toFile){  dev.off() }


# Figure X - Model Prediction Comparison ----------------------------------
if (toFile){  png("Figures/FigureX.png", width = 4, height = 4, units = "in", res = 300)  }
plot(predPost_orig_pred$`50%`~predPost_orig$data$logWET,
     xlim=range(predPost_orig$data$logWET)+c(-1,1), ylim=range(predPost_orig$data$logWET)+c(-1,1),
     xlab="Observed WL Loss (log hectare)", ylab="Predicted WL Loss (log hectare)")

ciRange = data.frame(x=numeric(0), y2.5=numeric(0), y97.5=numeric(0))
j=1
for (i in 1:length(predPost_orig$data$logWET))
{
  x = predPost_orig$data$logWET[i]
  y2.5 = predPost_orig_pred$`2.5%`[i]
  y97.5 = predPost_orig_pred$`97.5%`[i]
  
  row = ciRange[ciRange$x == x,]
  if (nrow(row) > 0) {
    if (y2.5 > row$y2.5) {
      ciRange[ciRange$x == x,]$y2.5 = y2.5
    }
    if (y97.5 > row$y97.5) {
      ciRange[ciRange$x == x,]$y97.5 = y97.5
    }
  } else {
    ciRange[j,] = c(x, y2.5, y97.5)
    j = j + 1
  }
}

xs = unique(predPost_orig$data$logWET)[order(ciRange$x)]
pred1=ciRange$y2.5[order(ciRange$x)]
pred2=ciRange$y97.5[order(ciRange$x)]

polygon(c(rev(xs), xs), c(rev(pred1), pred2), col = 'grey80', border = NA)

lines(ciRange$y2.5[order(ciRange$x)]~unique(predPost_orig$data$logWET)[order(ciRange$x)], lty = 2)
lines(ciRange$y97.5[order(ciRange$x)]~unique(predPost_orig$data$logWET)[order(ciRange$x)], lty = 2)

abline(0,1)

#arrows(predPost_orig$data$logWET, predPost_orig_pred$`2.5%`, predPost_orig$data$logWET, predPost_orig_pred$`97.5%`, length=0.05, angle=90, code=3)

points(predPost_orig_pred$`50%`~predPost_orig$data$logWET, pch=21, col="black", bg="white")

if (toFile){  dev.off() }
# #Residuals
# plot((predPost_orig$data$logWET-predPost_orig_pred$`50%`)~predPost_orig$data$logWET,
#      xlim=range(predPost_orig$data$logWET)+c(-1,1), ylim=range(predPost_orig$data$logWET)+c(-1,1),
#      xlab="Wetland Loss (log hectare)", ylab="Residuals")
# abline(0,0)


# Figure 3 - RSLR Restoration Correlation ---------------------------------
  # plot(rcp85_2300_orig_pred$mean~rcp85_2300_orig$data$RSLR, xlab="RSLR", ylab="Predicted Loss")
  # points(rcp85_2300_orig_pred$mean[rcp85_2300_orig$data$RESTORE == 1]~rcp85_2300_orig$data$RSLR[rcp85_2300_orig$data$RESTORE == 1], xlab="RSLR", ylab="Predicted Loss", pch=19, col="blue")
  # points(rcp85_2300_orig_pred$mean[rcp85_2300_orig$data$HA == 1]~rcp85_2300_orig$data$RSLR[rcp85_2300_orig$data$HA == 1], xlab="RSLR", ylab="Predicted Loss", pch=19, col="green")
  # points(rcp85_2300_orig_pred$mean[rcp85_2300_orig$data$RESTORE == 0]~rcp85_2300_orig$data$RSLR[rcp85_2300_orig$data$RESTORE == 0], xlab="RSLR", ylab="Predicted Loss", pch=19, col="red")

if (toFile){  png("Figures/Figure3.png", width = 4, height = 4, units = "in", res = 300)  }
library(sm)
sm.density.compare(predPost_orig$data$logWET, predPost_orig$data$RESTORE, col=c("black","black"), lty=c(1,2), xlab = "Wetland Loss (log hectares)")
legend(2.5,0.27,c("Restoration", "No Restoration"),lty=c(1,2), cex=if(toFile) 0.6 else 1)
if (toFile){  dev.off()  }

# Figure 4 - RCP3 vs RCP8.5 -----------------------------------------------
if (toFile){  png("Figures/Figure4.png", width = 8, height = 4, units = "in", res = 300)  }
op=par(mfrow=c(1,2))
plot(density(predPost_orig_pred$mean), ylim=c(0,1), xlim=c(min(predPost_orig_pred$mean), 5),
     main = "RCP3",lty=0, xlab = "Wetland Loss (log hectares)")
lines(density(predPost_orig_pred$mean), col="black", lty=3)
lines(density(rcp3_2100_orig_pred$mean), col="blue", lty=1)
lines(density(rcp3_2300_orig_pred$mean), col="red", lty=1)
legend(2.2, 0.8, c("Base", "2100", "2300"), col=c("black","blue","red"), lty=c(3,1,1), cex=0.75)

plot(density(predPost_orig_pred$mean), ylim=c(0,1), xlim=c(min(predPost_orig_pred$mean), 5),
     main = "RCP8.5",lty=0, xlab = "Wetland Loss (log hectares)")
lines(density(predPost_orig_pred$mean), col="black", lty=3)
lines(density(rcp85_2100_orig_pred$mean), col="blue", lty=1)
lines(density(rcp85_2300_orig_pred$mean), col="red", lty=1)
legend(2.2, 0.8, c("Base", "2100", "2300"), col=c("black","blue","red"), lty=c(3,1,1), cex=0.75)
par(op)
if (toFile){  dev.off()  }

# Figure 4 - (Restore v No Restore) v RSLR --------------------------------
predPost_NR = getCI("58-Any-10y-Y-NR")
rcp3_2100_NR = getCI("58-Any-10y-Y-RCP3_2100-NR")
rcp3_2300_NR = getCI("58-Any-10y-Y-RCP3_2300-NR")
rcp85_2100_NR = getCI("58-Any-10y-Y-RCP85_2100-NR")
rcp85_2300_NR = getCI("58-Any-10y-Y-RCP85_2300-NR")

predPost_NR_pred = MCMCsummary(predPost_NR$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp3_2100_NR_pred = MCMCsummary(rcp3_2100_NR$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp3_2300_NR_pred = MCMCsummary(rcp3_2300_NR$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp85_2100_NR_pred = MCMCsummary(rcp85_2100_NR$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp85_2300_NR_pred = MCMCsummary(rcp85_2300_NR$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))

if (toFile){  png("Figures/Figure5.png", width = 8, height = 4, units = "in", res = 300)  }
op=par(mfrow=c(1,2))
plot(density(predPost_orig_pred$`50%`), lwd=1,
     xlim=c(-1,7), ylim=c(0,1.05), xlab="Wetland Loss (log hectares)", main="")
lines(density(predPost_NR_pred$`50%`), lwd=2)
lines(density(rcp3_2300_orig_pred$`50%`), lwd=1, col="blue")
lines(density(rcp3_2300_NR_pred$`50%`), lwd=2, col="blue")
legend(3.8,0.9,c("Current (Base)", "RCP3 2300", "Restoration", "No Restoration"),
       col=c("black","blue","black","black"), lwd=c(1,1,1,2), cex=if(toFile) 0.6 else 1)
title("a)", adj=0, line=0.25, cex.main=0.85)

plot(density(predPost_orig_pred$`50%`), lwd=1,
     xlim=c(-1,7), ylim=c(0,1.05), xlab="Wetland Loss (log hectares)", main="")
lines(density(predPost_NR_pred$`50%`), lwd=2)
lines(density(rcp85_2300_orig_pred$`50%`), lwd=1, col="red")
lines(density(rcp85_2300_NR_pred$`50%`), lwd=2, col="red")
legend(3.8,0.9,c("Current (Base)", "RCP8.5 2300", "Restoration", "No Restoration"),
       col=c("black","red","black","black"), lwd=c(1,1,1,2), cex=if(toFile) 0.6 else 1)
title("b)", adj=0, line=0.25, cex.main=0.85)
par(op)
if (toFile){  dev.off() }


# Table 1 -----------------------------------------------------------------
compareNullRestores = function(base,null){
  c(baseSum=sum(exp(base$`50%`)), nullSum=sum(exp(null$`50%`)), 
    diff=sum(exp(null$`50%`))-sum(exp(base$`50%`)), ratio=sum(exp(null$`50%`)) / sum(exp(base$`50%`)))
}
# Increase in wetloss when removing restoration from current
compPredPost = compareNullRestores(predPost_orig_pred, predPost_NR_pred)

# Increase in wetland loss when removing restoration under RCP3 2100+2300
compRCP3_2100 = compareNullRestores(rcp3_2100_orig_pred, rcp3_2100_NR_pred)
compRCP3_2300 = compareNullRestores(rcp3_2300_orig_pred, rcp3_2300_NR_pred)

# Increase in wetland loss when removing restoration under RCP8.5 2100+2300
compRPC85_2100 = compareNullRestores(rcp85_2100_orig_pred, rcp85_2100_NR_pred)
compRPC85_2300 = compareNullRestores(rcp85_2300_orig_pred, rcp85_2300_NR_pred)

nullRestoTable = rbind(compPredPost, compRCP3_2100, compRCP3_2300, compRPC85_2100, compRPC85_2300)
write.table(nullRestoTable, "Figures/Table1.txt", sep="\t", quote=F)


# Density Difference from Baseline ----------------------------------------
plot(density(rcp85_2300_orig_pred$mean-predPost_orig_pred$mean), xlim=c(0,1.2))
lines(density(rcp85_2100_orig_pred$mean-predPost_orig_pred$mean), col="red")
lines(density(rcp3_2100_orig_pred$mean-predPost_orig_pred$mean), col="green")
lines(density(rcp3_2300_orig_pred$mean-predPost_orig_pred$mean), col="blue")

c(1:nrow(rcp85_2300_orig_pred))[rcp85_2300_orig_pred$mean-predPost_orig_pred$mean > 1]

# RSLR and HA Correlation -------------------------------------------------
plot(logWET~RSLR, data=thk99buff)
points(logWET~RSLR, data=thk99buff[rcp85_2300_orig_pred$mean-predPost_orig_pred$mean > 1,], col="red", pch=19)

plot(logWET~RSLR, data=thk99buff)
points(logWET~RSLR, data=thk99buff[rcp85_2300_orig$data$RESTORE == 1,], pch=19, col="blue")
points(logWET~RSLR, data=thk99buff[rcp85_2300_orig$data$HA == 1,], pch=19, col="green")


plot(rcp85_2300_orig_pred$mean~rcp85_2300_orig$data$RSLR, xlab="RSLR", ylab="Predicted Loss")
points(rcp85_2300_orig_pred$mean[rcp85_2300_orig$data$RESTORE == 1]~rcp85_2300_orig$data$RSLR[rcp85_2300_orig$data$RESTORE == 1], xlab="RSLR", ylab="Predicted Loss", pch=19, col="blue")
points(rcp85_2300_orig_pred$mean[rcp85_2300_orig$data$HA == 1]~rcp85_2300_orig$data$RSLR[rcp85_2300_orig$data$HA == 1], xlab="RSLR", ylab="Predicted Loss", pch=19, col="green")
points(rcp85_2300_orig_pred$mean[rcp85_2300_orig$data$RESTORE == 0]~rcp85_2300_orig$data$RSLR[rcp85_2300_orig$data$RESTORE == 0], xlab="RSLR", ylab="Predicted Loss", pch=19, col="red")

plot(rcp85_2300_orig_pred$mean~rcp85_2300_orig$data$RSLR, xlab="RSLR", ylab="Predicted Loss")
points(rcp85_2300_orig_pred$mean[rcp85_2300_orig$data$region == 1]~rcp85_2300_orig$data$RSLR[rcp85_2300_orig$data$region == 1], xlab="RSLR", ylab="Predicted Loss", pch=19, col="blue")
points(rcp85_2300_orig_pred$mean[rcp85_2300_orig$data$region == 2]~rcp85_2300_orig$data$RSLR[rcp85_2300_orig$data$region == 2], xlab="RSLR", ylab="Predicted Loss", pch=19, col="red")

plot(thk99buff[thk99buff$ORIG_FID %in% rcp85_2100_orig$data$ORIG_FID,])
plot(thk99buff[thk99buff$ORIG_FID %in% rcp85_2100_orig$data$ORIG_FID[rcp85_2100_orig$data$region == 1],], add=T, col="green", border="green")
plot(thk99buff[thk99buff$ORIG_FID %in% data.frame(rcp85_2100_orig$data)[rcp85_2300_orig_pred$mean-predPost_orig_pred$mean > 1,]$ORIG_FID,], add=T, col="red", border="RED")

coastlines = readOGR("C:/DATA/General Maps/Coastlines/GSHHS_2.3.5_04-2016/USCoast_h_L1_Line.shp", "USCoast_h_L1_Line")
coastlines = spTransform(coastlines, proj4string(thk99buff))
stateMap = readOGR("C:/DATA/General Maps/Gulf States/US_GulfStates.shp", "US_GulfStates")
HUC4 = readOGR("C:/DATA/HUC/HUC_shapes/WBDHU4.shp", "WBDHU4")
HUC4 = spTransform(HUC4, proj4string(thk99buff))
HUC4 = HUC4[HUC4$HUC4 %in% crop(HUC4, extent(thk99buff[thk99buff$region == 9 | thk99buff$region == 10,]))$HUC4,]
plot(thk99buff[thk99buff$region == 9 | thk99buff$region == 10,])
plot(stateMap,add=T,lty=2)
plot(HUC4,add=T)
plot(thk99buff[thk99buff$region == 9 | thk99buff$region == 10,],add=T)
plot(thk99buff[thk99buff$ORIG_FID %in% data.frame(rcp85_2100_orig$data)[rcp85_2300_orig_pred$mean-predPost_orig_pred$mean > 1,]$ORIG_FID,], add=T, col="GREEN", border="RED")






# Deprecated Plots --------------------------------------------------------

# W vs E LA RCP8.5
# if (toFile){  png("Figures/Figure3.png", width = 8, height = 4, units = "in", res = 300)  }
# op=par(mfrow=c(1,2))
# plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
# lines(density(rcp85_2100_orig_pred$mean), col="black",lty=3)
# lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$region == 1,]$mean), col="blue")
# lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$region == 2,]$mean), col="red")
# legend(2, 0.8, c("Combined", "W LA", "E LA"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# 
# plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
# lines(density(predPost_orig_pred$mean), col="black",lty=3)
# lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$region == 1,]$mean), col="blue")
# lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$region == 2,]$mean), col="red")
# legend(2, 0.8, c("Combined", "W LA", "E LA"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# par(op)
# if (toFile){  dev.off()  }
# 
# 
# Restore vs Non-Restore RCP8.5
# if (toFile){  png("Figures/Figure4.png", width = 8, height = 4, units = "in", res = 300)  }
# op=par(mfrow=c(1,2))
# plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
# lines(density(predPost_orig_pred$mean), col="black",lty=3)
# lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$RESTORE == 1,]$mean), col="blue")
# lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$RESTORE == 0,]$mean), col="red")
# #lines(density(rcp85_2100_pred$mean), col="green")
# legend(2, 0.8, c("Combined", "RESTORE", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# 
# plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
# lines(density(predPost_orig_pred$mean), col="black",lty=3)
# lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$RESTORE == 1,]$mean), col="blue")
# lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$RESTORE == 0,]$mean), col="red")
# #lines(density(rcp85_2300_pred$mean), col="green")
# legend(2, 0.8, c("Combined", "RESTORE", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# par(op)
# if (toFile){  dev.off()  }


# Compare Restoration Types RCP 8.5
# if (toFile){  png("Figures/Figure5.png", width = 8, height = 8, units = "in", res = 300)  }
# op=par(mfrow=c(2,2))
# # op=par(mfrow=c(1,2))
# plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
# lines(density(predPost_orig_pred$mean), col="black",lty=3)
# lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$HA == 1,]$mean), col="blue")
# lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$HA == 0,]$mean), col="red")
# #lines(density(rcp85_2100_pred$mean), col="green")
# legend(2, 0.8, c("Combined", "HA", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# 
# plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
# lines(density(predPost_orig_pred$mean), col="black",lty=3)
# lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$HA == 1,]$mean), col="blue")
# lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$HA == 0,]$mean), col="red")
# #lines(density(rcp85_2300_pred$mean), col="green")
# legend(2, 0.8, c("Combined", "HA", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# # par(op)
# # 
# # 
# # op=par(mfrow=c(1,2))
# plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
# lines(density(predPost_orig_pred$mean), col="black",lty=3)
# lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$BW == 1,]$mean), col="blue")
# lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$BW == 0,]$mean), col="red")
# #lines(density(rcp85_2100_pred$mean), col="green")
# legend(2, 0.8, c("Combined", "BW", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# 
# plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
# lines(density(predPost_orig_pred$mean), col="black",lty=3)
# lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$BW == 1,]$mean), col="blue")
# lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$BW == 0,]$mean), col="red")
# #lines(density(rcp85_2300_pred$mean), col="green")
# legend(2, 0.8, c("Combined", "BW", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# par(op)
# if (toFile){  dev.off()  }
# 
# 
# 
# 
# 
# 
# plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
# lines(density(predPost_orig_pred$mean), col="black",lty=3)
# lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$BW == 1,]$mean), col="blue")
# lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$BW == 0,]$mean), col="red")
# #lines(density(rcp85_2100_pred$mean), col="green")
# legend(2, 0.8, c("Combined", "BW", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# 
# plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
# lines(density(predPost_orig_pred$mean), col="black",lty=3)
# lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$BW == 1,]$mean), col="blue")
# lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$BW == 0,]$mean), col="red")
# #lines(density(rcp85_2300_pred$mean), col="green")
# legend(2, 0.8, c("Combined", "BW", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# par(op)




library(ggplot2)


# Regional Stacked Density ------------------------------------------------
post_regional = predPost_orig_pred %>%
  mutate(region = sapply(predPost_orig$data$region, FUN=function(x) { if (x == 1) return("West LA") else return("East LA") }))
names(post_regional)[4:6] = c("q2.5", "median", "q97.5")

r3_2100_regional = rcp3_2100_orig_pred %>%
  mutate(region = sapply(predPost_orig$data$region, FUN=function(x) { if (x == 1) return("West LA") else return("East LA") }))
names(r3_2100_regional)[4:6] = c("q2.5", "median", "q97.5")

r3_2300_regional = rcp3_2300_orig_pred %>%
  mutate(region = sapply(predPost_orig$data$region, FUN=function(x) { if (x == 1) return("West LA") else return("East LA") }))
names(r3_2300_regional)[4:6] = c("q2.5", "median", "q97.5")

r85_2100_regional = rcp85_2100_orig_pred %>%
  mutate(region = sapply(predPost_orig$data$region, FUN=function(x) { if (x == 1) return("West LA") else return("East LA") }))
names(r85_2100_regional)[4:6] = c("q2.5", "median", "q97.5")

r85_2300_regional = rcp85_2300_orig_pred %>%
  mutate(region = sapply(predPost_orig$data$region, FUN=function(x) { if (x == 1) return("West LA") else return("East LA") }))
names(r85_2300_regional)[4:6] = c("q2.5", "median", "q97.5")

# Base
p1 = ggplot(data=post_regional, aes(x=median, group=region, fill=region)) +
  geom_density(adjust=1.5, position="fill") + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,max(r85_2300_regional$median)), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual(values = c("darkgray","lightgray")) +
  xlab("Wetland Loss") + 
  ylab("% Total Loss in W LA") +
  geom_vline(xintercept=max(post_regional$median), linetype = "dashed")

multiplot(p1+ggtitle("a) RCP3 2100"),
          p1+ggtitle("c) RCP8.5 2100"),
          p1+ggtitle("b) RCP3 2300"),
          p1+ggtitle("d) RCP8.5 2300"), cols=2)

# RCP3
p2 = ggplot(data=r3_2100_regional, aes(x=median, group=region, fill=region)) +
  geom_density(adjust=1.5, position="fill", size=1.25) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,max(r85_2300_regional$median)), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual( values = c("white","white")) +
  xlab("Wetland Loss") + 
  ylab("% Total Loss in W LA")

p3 = ggplot(data=r3_2300_regional, aes(x=median, group=region, fill=region)) +
  geom_density(adjust=1.5, position="fill", size=1.25) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,max(r85_2300_regional$median)), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual( values = c("white","white")) +
  xlab("Wetland Loss") + 
  ylab("% Total Loss in W LA")

# RCP 8.5
p4 = ggplot(data=r85_2100_regional, aes(x=median, group=region, fill=region)) +
  geom_density(adjust=1.5, position="fill", size=1.5) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,max(r85_2300_regional$median)), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual( values = c("white","white")) +
  xlab("Wetland Loss") + 
  ylab("% Total Loss in W LA")

p5 = ggplot(data=r85_2300_regional, aes(x=median, group=region, fill=region)) +
  geom_density(adjust=1.5, position="fill", size=1.5) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,max(r85_2300_regional$median)), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual( values = c("white","white")) +
  xlab("Wetland Loss") + 
  ylab("% Total Loss in W LA")

multiplot(p2+ggtitle("a) RCP3 2100"),
          p4+ggtitle("c) RCP8.5 2100"),
          p3+ggtitle("b) RCP3 2300"),
          p5+ggtitle("d) RCP8.5 2300"),cols=2)


# Regional Stacked Density 2 ----------------------------------------------
ggplot(data=post_regional, aes(x=median, group=region, fill=region)) +
  geom_density(adjust=1.5, position="stack") + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,max(r85_2300_regional$median)), expand = c(0,0)) +
  # scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual(values = c("darkgray","lightgray")) #+
# xlab("Wetland Loss") + 
# ylab("% Total Loss in W LA")

ggplot(data=r85_2100_regional, aes(x=median, group=region, fill=region)) +
  geom_density(adjust=1.5, position="stack") + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,max(r85_2300_regional$median)), expand = c(0,0)) +
  # scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual(values = c("darkgray","lightgray")) #+
# xlab("Wetland Loss") + 
# ylab("% Total Loss in W LA")

ggplot(data=r85_2300_regional, aes(x=median, group=region, fill=region)) +
  geom_density(adjust=1.5, position="stack") + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  scale_x_continuous(limits = c(0,max(r85_2300_regional$median)), expand = c(0,0)) +
  # scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual(values = c("darkgray","lightgray")) #+
# xlab("Wetland Loss") + 
# ylab("% Total Loss in W LA")

# Restorational Stacked Density -------------------------------------------
post_restorational = predPost_orig_pred
post_restorational$restore = rep(NA, nrow(post_restorational))
post_restorational$hasRestore = rep(NA, nrow(post_restorational))
df = data.frame(rcp85_2300_orig$data)
for (i in 1:nrow(post_restorational)) {
  dfrow = df[i,]
  type = "None"
  for (j in 1:length(restoreParams))
  {
    rParam = substring(restoreParams,2,4)[j]
    if (dfrow[rParam] == 1)
    {
      if (type != "None")
      {
        if (type == "HA" || rParam == "HA")
        {
          type = "HA"
        }
      } else {
        type = rParam
      }
    }
  }
  
  post_restorational[i,]$restore = type
  post_restorational[i,]$hasRestore = (type != "None")
}
names(post_restorational)[4:6] = c("q2.5", "median", "q97.5")

r85_restorational = rcp85_2300_orig_pred
r85_restorational$restore = rep(NA, nrow(r85_restorational))
r85_restorational$hasRestore = rep(NA, nrow(r85_restorational))
df = data.frame(rcp85_2300_orig$data)
for (i in 1:nrow(r85_restorational)) {
  dfrow = df[i,]
  type = "None"
  for (j in 1:length(restoreParams))
  {
    rParam = substring(restoreParams,2,4)[j]
    if (dfrow[rParam] == 1)
    {
      if (type != "None")
      {
        if (type == "HA" || rParam == "HA")
        {
          type = "HA"
        }
      } else {
        type = rParam
      }
    }
  }
  
  r85_restorational[i,]$restore = type
  r85_restorational[i,]$hasRestore = (type != "None")
}
names(r85_restorational)[4:6] = c("q2.5", "median", "q97.5")


source("../../RUtilityFunctions/multiplot.R")
p1=ggplot(data=post_restorational[post_restorational$restore != "VP",], aes(x=median, group=hasRestore, fill=hasRestore)) +
  geom_density(adjust=1, position="stack") +
  theme(legend.position = c(0.75,0.75), legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,5), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,2.75), expand = c(0,0)) +
  scale_fill_manual(guide=guide_legend(reverse=TRUE), values = c("darkgray","palegreen"), labels=c("None", "Restoration")) +
  xlab("") +
  ylab("") +
  ggtitle("a)")

p2=ggplot(data=post_restorational[post_restorational$restore != "VP",], aes(x=median, group=restore, fill=restore)) +
  geom_density(adjust=1, position="stack") +
  theme(legend.position = c(0.75,0.75), legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,5), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,2.75), expand = c(0,0)) +
  scale_fill_manual(values = c("lightgoldenrod1","steelblue3","darkgray"), labels=c("Breakwaters", "Hydrological Alteration", "None")) +
  xlab("") +
  ylab("Prob. Density Total Loss") +
  ggtitle("b)")

p3=ggplot(data=r85_restorational[r85_restorational$restore != "VP",], aes(x=median, group=restore, fill=restore)) +
  geom_density(adjust=1, position="stack") +
  theme(legend.position = c(0.75,0.75), legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,5), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,2.75), expand = c(0,0)) +
  scale_fill_manual(values = c("lightgoldenrod1","steelblue3","darkgray"), labels=c("Breakwaters", "Hydrological Alteration", "None")) +
  xlab("Wetland Loss") +
  ylab("") +
  ggtitle("c)")

multiplot(p1,p2,p3)

# Restorational Stacked Density 2 -----------------------------------------
ggplot(data=post_restorational[post_restorational$restore != "VP",], aes(x=median, group=restore, fill=restore)) +
  geom_density(adjust=1, position="fill") +
  theme(legend.position = c(0.75,0.75), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,max(r85_restorational$median)), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual(values = c("darkgray","steelblue3","lightgoldenrod1", "Black")) +
  # xlab("Wetland Loss") + 
  # ylab("% Total Loss in W LA") +
  # geom_vline(xintercept=max(post_restorational$median), linetype = "dashed") +
  geom_rect(mapping=aes(xmin=max(post_restorational$median), ymin=0, xmax=max(r85_restorational$median), ymax=1), color="gray33", fill="gray33")

ggplot(data=r85_restorational[r85_restorational$restore != "VP",], aes(x=median, group=restore, fill=restore)) +
  geom_density(adjust=1, position="fill") +
  theme(legend.position = c(0.75,0.75), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  scale_x_continuous(limits = c(0,max(r85_restorational$median)), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual(values = c("darkgray","steelblue3","lightgoldenrod1", "Black")) #+
# scale_fill_manual( values = c("white","white")) +
# xlab("Wetland Loss") +
# ylab("% Total Loss in W LA")



