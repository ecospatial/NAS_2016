dir = "C:/DATA/EarthEngine/"

thk99 = read.csv(paste0(dir, "T0/thk99.csv"))

wetloss = read.csv(paste0(dir, "T0/thk99wetloss.csv"))
names(wetloss) = gsub("count","WET",names(wetloss))

ndvi = read.csv(paste0(dir, "T0/thk99NDVI.csv"))
names(ndvi) = gsub("mean","ndvi",names(ndvi))

ndmi = read.csv(paste0(dir, "T0/thk99NDMI.csv"))
names(ndmi) = gsub("mean","ndmi",names(ndmi))

wet96 = read.csv(paste0(dir, "T0/thk99wet96.csv"))
names(wet96) = gsub("count","wet96",names(wet96))

data = merge(merge(merge(merge(thk99,wetloss,by="system.index"),ndvi,by="system.index"),ndmi,by="system.index"),wet96,by="system.index")
names(data)
data = data[c("ORIG_FID.x", "SLOPE___","SL_MM_YR_", "TIDE_M_","WAVES_M_","WET","ndvi","ndmi","wet96")]
names(data) = c("ORIG_FID", "CS", "RSLR", "TR", "WH", "WET", "NDVI","NDMI","WET96")
names(data)

data$logWET = rep(NA, nrow(data))
data$logWET[data$WET > 0] = log(data$WET[data$WET > 0])

data$logWET96 = rep(NA, nrow(data))
data$logWET96[data$WET96 > 0] = log(data$WET96[data$WET96 > 0])

data$PCT = rep(NA, nrow(data))
data$PCT[data$WET96 > 0] = data$WET[data$WET96 > 0]/data$WET96[data$WET96 > 0]

data$logPCT = rep(NA, nrow(data))
data$logPCT[!is.na(data$PCT) & data$PCT != 0] = log(data$PCT[!is.na(data$PCT) & data$PCT != 0])

write.csv(data, paste0(dir, "T1/fullData.csv"))



# Modify kml file for R
kmlDat = readLines(file(paste0(dir, "T0/thk99buff.kml")))
kmlDatNew = kmlDat[1:3]
kmlDatNew = c(kmlDatNew, "<Folder><name>thk99buff</name>")
kmlDatNew = c(kmlDatNew, kmlDat[4:(length(kmlDat)-2)])
kmlDatNew = c(kmlDatNew, "</Folder>")
kmlDatNew = c(kmlDatNew, kmlDat[(length(kmlDat)-2+1):length(kmlDat)])
writeLines(kmlDatNew, paste0(dir, "T1/thk99buff.kml"))


# Plots
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