
library(rjags)
library(sp)
library(magrittr)
library(rgdal)
source("../RUtilityFunctions/CodaSamplesDIC.R")


inlandbuff = readOGR("C:\\Users\\GCRLWuHardy\\Documents\\071416-InlandTHK\\inlandbuff.shp","inlandbuff")

huc2AvgDis = readOGR("huc2AvgDis.shp", "huc2AvgDis")
huc2AvgDis = spTransform(huc2AvgDis, proj4string(inlandbuff))
huc2AvgDis = huc2AvgDis[huc2AvgDis$Avg_v_av_d>0,]

plot(huc2AvgDis)


hucZone=over(inlandbuff,huc2AvgDis[,"HUC2"]) #3=FL, 12=TX, 8=LA
hucZone$ORIG_FID=seq(0:(nrow(hucZone)-1))

dat=read.delim("../wetlandLossData.txt")
dat = merge(dat, hucZone, by="ORIG_FID")
dat$region = rep(2,nrow(dat))
dat[dat$HUC2 == "03" | dat$HUC2 == "12",]$region = 1

dat$WET = dat$WET*900/10000

data=list(Nobs=nrow(dat), Nregion=2)

model=jags.model(textConnection("model {
    for (i in 1:Nobs) {
      v.mu[i] <- b0 + bTR*TR[i] + bRSLR*RSLR[i]
      logWET[i] ~ dnorm(v.mu[i], v.tau)
    }
      
    b0 ~ dnorm(0,0.00001)

    #bCS ~ dnorm(0,0.00001)
    #bWH ~ dnorm(0,0.00001)
    bTR ~ dnorm(0,0.00001)
    bRSLR ~ dnorm(0,0.00001)

    #for (j in 1:Nregion){
    #  bNDMI[j] ~ dnorm(0,0.00001)
    #}
    #bNDMI ~ dnorm(0,0.00001)

    v.tau ~ dgamma(1,1)
    v.sigma <- 1/sqrt(v.tau)
    }"), data=append(data,dat), n.chains=3, n.adapt=2000)

output=coda.samples.dic(model=model,variable.names=c("b0", "bCS", "bWH", "bTR", "bRSLR", "bNDMI"), n.iter=20000, thin=1)
summary(output$samples)
output$dic
