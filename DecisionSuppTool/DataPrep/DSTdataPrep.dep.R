library(MCMCvis)
library(magrittr)
library(dplyr)
source("../../RUtilityFunctions/BayesianTools.R")
source("../../RUtilityFunctions/codaSamplesDIC.R")

# coeffs = MCMCsummary(getCI("logWET-14R-NDVI/58", dir="../../WetlandModel/")$samples) %>% data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param))
# save(coeffs, file = "../ModelCoefficients.RData")

# source("../../WetlandModel/loadTHK99.R")
# source("../../WetlandModel/SLR/prepRestoreData.R")

source("../../WetlandModel/loadTHK99.R")
loadTHK99data(local=T, regions="ALL")
# LA ONLY::
# dat = getCI("58-Any-10y-Y-od", dir="../../WetlandModel/SLR/")


# Model 58 Area Coefficients ----------------------------------------------
params=c("NDVI","RSLR","WH","CS","TR")
data = append(list(Nobs=nrow(thk99buff_n), Nregion=14), thk99buff_n)
model = jags.model("../../WetlandModel/Models/logWET-14R-NDVI/58.txt", 
                                      data = data,
                                      n.chains=3,
                                      n.adapt=2000)
output = coda.samples.dic(model = model,
                         variable.names=c("b0", paste0("b", params)),
                         n.iter=20000,
                         thin=1)

coeffs = MCMCsummary(output$samples) %>% data.frame() %>%
  cbind(param=row.names(.), .) %>%
  mutate(param=as.character(param))  %>% 
  mutate(region=sapply(param, FUN=function(x) {
    pAr = unlist(strsplit(x, "\\["))
    if (length(pAr) > 1) {
      return(as.numeric(gsub("\\]", "", pAr[2])))
    } else {
      return(-1)
    }
  }))%>%
  mutate(param=sapply(param, FUN=function(x) {
    pAr = unlist(strsplit(x, "\\["))
    return(pAr[1])
  }))

save(coeffs, file = "../FullModelCoefficients.RData")



# LA Restore Scenarios ----------------------------------------------------

thk99buff_resto = getCI("58-Any-10y-Y-od", dir="../../WetlandModel/SLR/")$data %>% data.frame() %>% select(-Nobs, -Nregion)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
RunRestorationModel(F, F, 10, 58, T, F, F, data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = matrix(0, nrow=length(c("BW","HA","MC","VP","RESTORE","YEAR")), ncol=nrow(thk99buff_resto))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-Null", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("VPx")] = rep(1, nrow(thk99buff_resto))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-NullVP", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("HAx")] = rep(0, nrow(thk99buff_resto))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-NullHA", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("BWx")] = rep(0, nrow(thk99buff_resto))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-NullBW", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("BWx","HAx")] = cbind(rep(0, nrow(thk99buff_resto)),rep(0, nrow(thk99buff_resto)))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-NullBWHA", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("BWx","VPx")] = cbind(rep(0, nrow(thk99buff_resto)),rep(0, nrow(thk99buff_resto)))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-NullBWVP", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("HAx","VPx")] = cbind(rep(0, nrow(thk99buff_resto)),rep(0, nrow(thk99buff_resto)))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-NullHAVP", data=thk99buff_resto, modData=T)


null = MCMCsummary(getCI("58-Any-10y-Y-Null")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
nullHA = MCMCsummary(getCI("58-Any-10y-Y-NullHA")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
nullBW = MCMCsummary(getCI("58-Any-10y-Y-NullBW")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
nullVP = MCMCsummary(getCI("58-Any-10y-Y-NullVP")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
nullBWHA = MCMCsummary(getCI("58-Any-10y-Y-NullBWHA")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
nullBWVP = MCMCsummary(getCI("58-Any-10y-Y-NullBWVP")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
nullHAVP = MCMCsummary(getCI("58-Any-10y-Y-NullHAVP")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))





thk99buff_resto = getCI("58-Any-10y-Y-od", dir="../../WetlandModel/SLR/")$data %>% data.frame() %>% select(-Nobs, -Nregion)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = matrix(1, nrow=length(c("BW","HA","MC","VP","RESTORE","YEAR")), ncol=nrow(thk99buff_resto))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-Full", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("VPx")] = rep(1, nrow(thk99buff_resto))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-FullVP", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("HAx")] = rep(1, nrow(thk99buff_resto))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-FullHA", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("BWx")] = rep(1, nrow(thk99buff_resto))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-FullBW", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("BWx","HAx")] = cbind(rep(1, nrow(thk99buff_resto)),rep(1, nrow(thk99buff_resto)))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-FullBWHA", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("BWx","VPx")] = cbind(rep(1, nrow(thk99buff_resto)),rep(1, nrow(thk99buff_resto)))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-FullBWVP", data=thk99buff_resto, modData=T)

thk99buff_resto[c("BWx","HAx","MCx","VPx","RESTOREx", "YEARx")] = thk99buff_resto[c("BW","HA","MC","VP","RESTORE", "YEAR")]
thk99buff_resto[c("HAx","VPx")] = cbind(rep(1, nrow(thk99buff_resto)),rep(1, nrow(thk99buff_resto)))
RunRestorationModel(F, F, 10, 58, T, F, F, nameMod="-FullHAVP", data=thk99buff_resto, modData=T)

full = MCMCsummary(getCI("58-Any-10y-Y-Full")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
fullHA = MCMCsummary(getCI("58-Any-10y-Y-FullHA")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
fullBW = MCMCsummary(getCI("58-Any-10y-Y-FullBW")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
fullVP = MCMCsummary(getCI("58-Any-10y-Y-FullVP")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
fullBWHA = MCMCsummary(getCI("58-Any-10y-Y-FullBWHA")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
fullBWVP = MCMCsummary(getCI("58-Any-10y-Y-FullBWVP")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))
fullHAVP = MCMCsummary(getCI("58-Any-10y-Y-FullHAVP")$samples) %>% as.data.frame() %>% cbind(param=row.names(.), .) %>% mutate(param=as.character(param)) %>% filter(grepl("logWET", param))




