library(dplyr)
library(MCMCvis)
source("../../RUtilityFunctions/bayesianTools.R")

source("../loadTHK99.R")

loadTHK99data(local=T, regions="ALL")

# Restoration Posteriors --------------------------------------------------
predPost_orig = getCI("58-Any-10y-Y-od")
MCMCplot(predPost_orig$samples, params = c("bBW", "bVP", "bHA", "bMC"), ref_ovl = T)


# How to Remove Year ------------------------------------------------------
params = c("RSLR","WH","TR","CS", "NDVI")
restoreParams = c("HA", "MC", "BW", "VP")

thk99buff_la = thk99buff@data[thk99buff$region %in% c(9,10),]
thk99buff_la$region = thk99buff_la$region-8

rcp3_2100_noyear = RunRestorationModel(F, F, 10, 58, F, F, F, RSLRscen = c(RCPscens$RCP3_2100.mu, RCPscens$RCP3_2100.sd), nameMod = "RCP3_2100-noyear")
MCMCplot(rcp3_2100_noyear$samples, params = paste0("b", c(params,restoreParams)), ref_ovl = T)

predPost = RunRestorationModel(F, F, 10, 58, T, F, F)
MCMCplot(predPost$samples, params = paste0("b", c(params,restoreParams)), ref_ovl = T)


predPost_orig_ny = RunRestorationModel(F, F, 10, 58, F, F, F, nameMod = "orig_ny", data=thk99buff_la)
MCMCplot(predPost_orig_ny$samples, params = paste0("b", c(params,restoreParams)), ref_ovl = T)

predPost_orig_y = RunRestorationModel(F, F, 10, 58, T, F, F, nameMod = "orig_y", data=thk99buff_la)
MCMCplot(predPost_orig_y$samples, params = paste0("b", c(params,restoreParams)), ref_ovl = T)

predPost_orig_y2 = RunRestorationModel(F, F, 10, 58, T, F, F, nameMod = "orig_y2", data=thk99buff_la)
MCMCplot(predPost_orig_y2$samples, params = paste0("b", c(params,restoreParams)), ref_ovl = T)

predPost_orig_ny_u15 = RunRestorationModel(F, F, 10, 58, F, F, F, nameMod = "orig_y2_u15", data=thk99buff_la[thk99buff_la$YEAR < 15,])
MCMCplot(predPost_orig_ny_u15$samples, params = paste0("b", c(params,restoreParams)), ref_ovl = T)

predPost_orig_ny_u10 = RunRestorationModel(F, F, 10, 58, F, F, F, nameMod = "orig_y2_u10", data=thk99buff_la[thk99buff_la$YEAR < 10,])
MCMCplot(predPost_orig_ny_u10$samples, params = paste0("b", c(params,restoreParams)), ref_ovl = T)

predPost_orig_ny_nha = RunRestorationModel(F, F, 10, 58, F, F, F, nameMod = "orig_y2_nha", data=thk99buff_la[thk99buff_la$HA == 0,])
MCMCplot(predPost_orig_ny_nha$samples, params = paste0("b", c(params,restoreParams))[paste0("b", c(params,restoreParams))!="bHA"], ref_ovl = T)

# ??????????????????????????


# Run SLR Scenarios -------------------------------------------------------
rcp3_2100_orig = getCI("58-Any-10y-Y-RCP3_2100-od")
rcp3_2300_orig = getCI("58-Any-10y-Y-RCP3_2300-od")
rcp85_2100_orig = getCI("58-Any-10y-Y-RCP85_2100-od")
rcp85_2300_orig = getCI("58-Any-10y-Y-RCP85_2300-od")

predPost_orig_pred = MCMCsummary(predPost_orig$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp3_2100_orig_pred = MCMCsummary(rcp3_2100_orig$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp3_2300_orig_pred = MCMCsummary(rcp3_2300_orig$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp85_2100_orig_pred = MCMCsummary(rcp85_2100_orig$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))
rcp85_2300_orig_pred = MCMCsummary(rcp85_2300_orig$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% dplyr::filter(grepl("logWET.p", param))

op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP3",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp3_2100_orig_pred$mean), col="blue")
lines(density(rcp3_2300_orig_pred$mean), col="red", lty=2)
legend(3, 0.8, c("Base", "2100", "2300"), col=c("black","blue","red"), lty=c(3,1,2), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_orig_pred$mean), col="blue")
lines(density(rcp85_2300_orig_pred$mean), col="red",lty=2)
legend(3, 0.8, c("Base", "2100", "2300"), col=c("black","blue","red"), lty=c(3,1,2), cex=0.75)
par(op)

op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
lines(density(rcp85_2100_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$region == 1,]$mean), col="blue")
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$region == 2,]$mean), col="red")
legend(2, 0.8, c("Combined", "W LA", "E LA"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$region == 1,]$mean), col="blue")
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$region == 2,]$mean), col="red")
legend(2, 0.8, c("Combined", "W LA", "E LA"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
par(op)





op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$RESTORE == 1,]$mean), col="blue")
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$RESTORE == 0,]$mean), col="red")
#lines(density(rcp85_2100_pred$mean), col="green")
legend(2, 0.8, c("Combined", "RESTORE", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$RESTORE == 1,]$mean), col="blue")
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$RESTORE == 0,]$mean), col="red")
#lines(density(rcp85_2300_pred$mean), col="green")
legend(2, 0.8, c("Combined", "RESTORE", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
par(op)

op=par(mfrow=c(2,2))
# op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$HA == 1,]$mean), col="blue")
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$HA == 0,]$mean), col="red")
#lines(density(rcp85_2100_pred$mean), col="green")
legend(2, 0.8, c("Combined", "HA", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$HA == 1,]$mean), col="blue")
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$HA == 0,]$mean), col="red")
#lines(density(rcp85_2300_pred$mean), col="green")
legend(2, 0.8, c("Combined", "HA", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
# par(op)
# 
# 
# op=par(mfrow=c(1,2))
plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2100",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$BW == 1,]$mean), col="blue")
lines(density(rcp85_2100_orig_pred[rcp85_2100_orig$data$BW == 0,]$mean), col="red")
#lines(density(rcp85_2100_pred$mean), col="green")
legend(2, 0.8, c("Combined", "BW", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)

plot(density(thk99buff_n$logWET), ylim=c(0,0.9), main = "RCP8.5 2300",lty=0)
lines(density(predPost_orig_pred$mean), col="black",lty=3)
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$BW == 1,]$mean), col="blue")
lines(density(rcp85_2300_orig_pred[rcp85_2300_orig$data$BW == 0,]$mean), col="red")
#lines(density(rcp85_2300_pred$mean), col="green")
legend(2, 0.8, c("Combined", "BW", "NONE"), col=c("black","blue","red"), lty=c(3,1,1,1), cex=0.75)
par(op)

MCMCplot(predPost_orig$samples, params=c(paste0("b", c(params, restoreParams)), "bYEAR"), ref_ovl=T)
MCMCtrace(predPost_orig$samples, params=c(paste0("b", c(params, restoreParams)), "bYEAR"))



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

# Compare E to W LA
compareRegions = function(scenario){
  wLA = sum(exp(scenario$`50%`[scenario$region == "West LA"]))
  eLA = sum(exp(scenario$`50%`[scenario$region == "West LA"]))
  c(total=sum(exp(scenario$`50%`)), meanT=mean(exp(scenario$`50%`)),
    WestLA=wLA, meanW=wLA/nrow(scenario[scenario$region == "West LA",]),
    EastLA=eLA, meanE=eLA/nrow(scenario[scenario$region == "East LA",]),
    ratio=wLA/eLA, prop=wLA/(wLA+eLA))
}
post_regional = predPost_orig_pred %>%
  mutate(region = sapply(predPost_orig$data$region, FUN=function(x) { if (x == 1) return("West LA") else return("East LA") }))
r85_2100_regional = rcp85_2100_orig_pred %>%
  mutate(region = sapply(predPost_orig$data$region, FUN=function(x) { if (x == 1) return("West LA") else return("East LA") }))
r85_2300_regional = rcp85_2300_orig_pred %>%
  mutate(region = sapply(predPost_orig$data$region, FUN=function(x) { if (x == 1) return("West LA") else return("East LA") }))

compareRegions(post_regional)
compareRegions(r85_2100_regional)
compareRegions(r85_2300_regional)
table(predPost_orig$data$region)


# Restoration Type Barplot ------------------------------------------------
df1 = data.frame(predPost_orig$data) %>% mutate(type = apply(., 1, FUN=function(x){
  typ = "None"
  for (r in c("BW","VP","MC","HA")) {
    if (x[r] == 1) {
      if (typ != "None") {
        return("Mixed")
      }
      else {
        typ = r
      }
    }
  }
  return(typ)
}))
x = barplot(table(df1$type), ylim=c(0,max(table(df1$type))*1.1))
text(x = x, y = table(df1$type), label = table(df1$type), pos = 3)


# Increase from RCP Scenarios ---------------------------------------------
c(base=mean(predPost_orig_pred$`50%`),
  rcp3_2100=mean(rcp3_2100_orig_pred$`50%`),
  rcp3_2300=mean(rcp3_2300_orig_pred$`50%`),
  rcp85_2100=mean(rcp85_2100_orig_pred$`50%`),
  rcp85_2300=mean(rcp85_2300_orig_pred$`50%`))
