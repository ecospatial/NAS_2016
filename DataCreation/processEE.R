thk99 = read.csv("T0/thk99.csv")

wetloss = read.csv("T0/thk99wetloss.csv")
names(wetloss) = gsub("count","WET",names(wetloss))

ndvi = read.csv("T0/thk99NDVI.csv")
names(ndvi) = gsub("mean","ndvi",names(ndvi))

ndmi = read.csv("T0/thk99NDMI.csv")
names(ndmi) = gsub("mean","ndmi",names(ndmi))

data = merge(merge(merge(thk99,wetloss,by="system.index"),ndvi,by="system.index"),ndmi,by="system.index")
names(data)
data = data[c("ORIG_FID.x", "SLOPE___","SL_MM_YR_", "TIDE_M_","WAVES_M_","WET","ndvi","ndmi")]
names(data) = c("ORIG_FID", "CS", "RSLR", "TR", "WH", "WET", "NDVI","NDMI")
names(data)

data$logWET = rep(NA, nrow(data))
data$logWET[data$WET > 0] = log(data$WET[data$WET > 0])

write.csv(data,"T1/fullData.csv")



plot(WET~RSLR, data=data)
plot(WET~NDVI, data=data, ylim=c(0,500),xlim=c(-1,1))
plot(WET~NDMI, data=data, ylim=c(0,500),xlim=c(-1,1))
plot(logWET~NDVI, data=data)

plot(data, pch=".")

plot(NDVI~NDMI, data=data,xlim=c(-1,1),ylim=c(-1,1))
lm1 = lm(NDVI~NDMI, data=data)
abline(lm1)

plot(WET~NDVI, data=data)
plot(WET~NDMI, data=data)

plot(logWET~NDVI, data=data)
plot(logWET~NDMI, data=data)


plot(logWET~NDVI:RSLR, data=data)
plot(logWET~NDMI:RSLR, data=data)