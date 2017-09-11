library(tidyr)
library(dplyr)

dat = read.delim("TEEB-NoBT-USD2007.txt")
dat=dat[!is.na(dat$ServiceArea),]
dat$RegionID = as.numeric(dat$Continent)

#Group Regions
dat$RegionID_new=rep(2,nrow(dat))
dat[dat$RegionID == 2| dat$RegionID == 4,]$RegionID_new = 1

oldScipen <- options(scipen=500,digits=5)
write.table(data.frame(
  Region=dat$RegionID_new,
  WetArea=log(dat$ServiceArea),
  USD2007=log(dat$USD2007)
), "Region2WetUSD.txt", quote=FALSE, row.names = FALSE)
options(oldScipen)

popRegion = read.delim("./Population/worldPop.txt")
popRegion = popRegion[order(popRegion$RegionID),]

gdpRegion = read.delim("./GDP/worldPPGDP2010.txt")
gdpRegion = gdpRegion[order(gdpRegion$RegionID),]


popRegion2 = c(mean(popRegion$X2007[2],popRegion$X2007[4]),
               mean(popRegion$X2007[1],popRegion$X2007[3],popRegion$X2007[5],popRegion$X2007[6]))
gdpRegion2 = c(mean(gdpRegion$PPGDPUSD2010[2],gdpRegion$PPGDPUSD2010[4]),
              mean(gdpRegion$PPGDPUSD2010[1],gdpRegion$PPGDPUSD2010[3],gdpRegion$PPGDPUSD2010[5],gdpRegion$PPGDPUSD2010[6]))

rbind(popRegion2, gdpRegion2)
