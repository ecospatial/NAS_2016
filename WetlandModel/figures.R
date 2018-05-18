library(MCMCvis)
library(dplyr)

library(RColorBrewer)
library(classInt)
library(sp)
library(TeachingDemos) #subplot

source("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/RUtilityFunctions/bayesianTools.R")
# Write Signifs to File ---------------------------------------------------
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel")

# Load Map Data
regionNamesTH = {c(
  "S FL", #1
  "W FL", #2
  "BgBnd FL", #3
  "E PH FL", #4
  "C PH FL", #5
  "W PH FL", #6
  "Mob AL", #7
  "MS Coa", #8
  "W LA", #9
  "MS Delta", #10
  "E TX", #11
  "C TX", #12
  "W TX", #13
  "S TX" #14
)}
regionNamesShort = {c(
  "TRIN",# "Trinit",
  "GALV",# "Galves",
  "BRAZ",# "Brazos",
  "CNTX",# "Cnt TX",
  "SWTX",# "SWe TX",
  "CHES",# "ChoEsc",
  "SFLO",# "So Flo",
  "TAMP",# "Tampa",
  "APAL",# "Apalac",
  "LCSB",# "LCSBCo",
  "MOBL",# "Mobile",
  "MSDL",# "MissDl",
  "PERL",# "Pearl",
  "PASC",# "Pascag",
  "CHEN",# "W LA",
  "OCHL",# "Ochloc",
  "RIOG",# "RioGra",
  "SUWA"# "Suwann"
)}
{
  # Read in discharge data
  hucData = read.delim("Discharge/dischargePerHUC.txt", sep="\t")
  hucData$HUC4 = sprintf("%04d", hucData$HUC4)
  hucData$SHORTNAME = regionNamesShort
  thk99regions = thk99buff[c("HUC4","region")]@data
  thk99regions$HUC4 = as.character(thk99regions$HUC4)
  thk99regions = unique(thk99regions)
  hucData = merge(hucData, thk99regions, by = "HUC4", all = T, sort = F)
  hucData = merge(hucData, cbind(SHORTNAME2=regionNamesTH, region=1:14), by="region")
  HUC4 = readOGR("C:/DATA/HUC/HUC_shapes/WBDHU4.shp", "WBDHU4")
  HUC4 = spTransform(HUC4, proj4string(thk99buff))
  #HUC4inPlot = crop(HUC4, extent(thk99buff))
  HUC4inPlot = HUC4[HUC4$HUC4 %in% thk99buff$HUC4,]
  HUC4inPlot = merge(HUC4inPlot, thk99buff@data %>% group_by(HUC4) %>% summarise(NSITES = n()), by = "HUC4")
  HUC4inPlot = merge(HUC4inPlot, hucData, by = "HUC4")
  
  strangeOrder = order(HUC4inPlot$region) # Order HUC4inPlot from 1 to 14
  strangeOrder[9:10] = strangeOrder[10:9] # Swap regions 9 and 10
  strangeOrder[1:length(strangeOrder)] = strangeOrder[length(strangeOrder):1] # Order from 14 to 1 with strange order (9 and 10 swapped)
  HUC4inPlot = HUC4inPlot[strangeOrder,]
  
  #Load other map data
  coastlines = readOGR("C:/DATA/General Maps/Coastlines/GSHHS_2.3.5_04-2016/USCoast_h_L1_Line.shp", "USCoast_h_L1_Line")
  coastlines = spTransform(coastlines, proj4string(thk99buff))
  stateMap = readOGR("C:/DATA/General Maps/Gulf States/US_GulfStates.shp", "US_GulfStates")
}


# Table 1 -----------------------------------------------------------------
areal=combineDIC("logWET-14R-NDVI")
areal=areal[c("modelNo","fixed","random","DIC","sig","type")]
areal
write.table(areal, "Figures/TableI.txt", sep="\t")

# Figure 1 ----------------------------------------------------------------
toFile = T

if (toFile){
  png("Figures/Figure1.png", width = 8, height = 8*0.85, units="in", res=300)
}
# Plot
{
  # Initialize plot
  plot(thk99buff, border=NA, col=NA, axes=T)
  
  # Coastlines
  plot(coastlines,add=T,lty=2)
  
  # HUCs shaded by discharge
  nBreaks = 9
  pal = brewer.pal(nBreaks, "PuBu")
  breaks = classIntervals(HUC4inPlot$avgdis, n = nBreaks, style = "jenks")$brks
  brknDis = cut(HUC4inPlot$avgdis, breaks)
  cols = pal[brknDis]
  cols[HUC4inPlot$avgdis == breaks[1]] = pal[1]
  cols[is.na(cols)] = "red"
  plot(HUC4inPlot, col=cols, add=T)
  
  # State lines
  plot(stateMap, add=T, lty=2)
  
  # THK99
  plot(thk99buff, add=T, border="orange", col="yellow")
  #points(coordinates(thk99buff), col="yellow", pch=21)
  
  # Labels
  labs = paste0(HUC4inPlot$SHORTNAME2, "\n", HUC4inPlot$region)
  HUC4visible = crop(HUC4inPlot, extent(par('usr'))) # Crop HUC4inPlot to only the plot region so that labels are placed within plot
  text(coordinates(HUC4visible), labels = labs, cex = 0.7)
  text(coordinates(HUC4visible[HUC4visible$avgdis >= breaks[8],]), labels = labs[HUC4visible$avgdis >= breaks[8]], cex = 0.7, col="WHITE")
  
  
  # Bar plot for n
  if (toFile)
  {
    scale = 1.2
    sz = c(3,2)*scale
  } else {
    sz = c(3,2)
  }
  subplot(
    x=mean(par("usr")[1:2]),
    y=24.5,
    size = sz,
    type = 'fig',
    pars = list(
      mar=c(4,2,0,0),
      mgp=c(1.5,0.75,0)
    ),
    fun = {
      x = barplot(HUC4inPlot$NSITES,
                  xlab = "Region",
                  ylab="n",
                  ylim=c(0, 1.20*max(HUC4inPlot$NSITES)),
                  names=HUC4inPlot$region,
                  col="orange", #cols
                  cex.axis=0.6,
                  cex.names=0.6,
                  cex.lab=0.6)
      
      text(x = x, y = HUC4inPlot$NSITES, label = HUC4inPlot$NSITES, pos = 3, cex = 0.6)
    })
}
if (toFile) {
  dev.off()
}


# Figure 2 ----------------------------------------------------------------
# DAG

# Figure 3 ----------------------------------------------------------------
regionNames = hucData %>% filter(HUC4 %in% thk99buff$HUC4) %>% arrange(region) %>% pull(SHORTNAME)
op=par(mfrow=c(2,2))
modelNo = 58
MCMCplot(getCI(modelNo, "logWET-14R-NDVI")$samples,
         params = "bRSLR",
         ref_ovl=T,
         labels_sz = 0.7,
         labels = regionNames,
         ax_sz = 2,
         xlim = c(-3, 3),
         xlab="RSLR param value")
MCMCplot(getCI(modelNo, "logWET-14R-NDVI")$samples,
         params = "bTR",
         ref_ovl=T,
         labels_sz = 0.7,
         labels = regionNames,
         ax_sz = 2,
         xlim = c(-3, 3),
         xlab="TR param value")
MCMCplot(getCI(modelNo, "logWET-14R-NDVI")$samples,
         params = "bWH",
         ref_ovl=T,
         labels_sz = 0.7,
         labels = regionNames,
         ax_sz = 2,
         xlim = c(-3, 3),
         xlab="WH param value")
MCMCplot(getCI(modelNo, "logWET-14R-NDVI-rB0")$samples,
         params = "b0",
         ref_ovl=T,
         labels_sz = 0.7,
         labels = regionNames,
         ax_sz = 2,
         xlim = c(-3, 3),
         xlab="b0 param value")
par(op)

# Figure 4 ----------------------------------------------------------------
modelNo = 241
op = par(mfrow=c(1,2))
MCMCplot(getCI(modelNo, "logWET-14R-NDVI")$samples,
         params = "bNDVI",
         ref_ovl=T,
         labels = "Region Wide",
         ax_sz = 2,
         xlim = c(-3, 3),
         xlab="NDVI param value")

MCMCplot(getCI(modelNo, "logWET-14R-NDVI")$samples,
         params = "bNDVI",
         ref_ovl=T,
         labels = "Region Wide",
         ax_sz = 2,
         xlab="NDVI param value")
par(op)


# PERCENT

# Table 2 -----------------------------------------------------------------
percent=combineDIC("logPCT-14R-NDVI")
percent=percent[c("modelNo","fixed","random","DIC","sig","type")]
percent
write.table(percent, "Figures/TableII.txt", sep="\t")


# Figure 5 ----------------------------------------------------------------
op=par(mfrow=c(2,2))
modelNo = 210
MCMCplot(getCI(modelNo,  "logPCT-14R-NDVI-rB0")$samples,
         params = "bCS",
         ref_ovl=T,
         labels_sz = 0.7,
         labels = regionNames,
         ax_sz = 2,
         xlim=c(-3, 3),
         xlab="CS param value")
MCMCplot(getCI(modelNo,  "logPCT-14R-NDVI-rB0")$samples,
         params = "bTR",
         ref_ovl=T,
         labels_sz = 0.7,
         labels = regionNames,
         ax_sz = 2,
         xlim=c(-3, 3),
         xlab="TR param value")
MCMCplot(getCI(modelNo,  "logPCT-14R-NDVI-rB0")$samples,
         params = "bWH",
         ref_ovl=T,
         labels_sz = 0.7,
         labels = regionNames,
         ax_sz = 2,
         xlim=c(-3, 3),
         xlab="WH param value")
MCMCplot(getCI(modelNo, "logPCT-14R-NDVI-rB0")$samples,
         params = "bNDVI",
         ref_ovl=T,
         labels_sz = 0.7,
         labels = regionNames,
         ax_sz = 2,
         xlim=c(-3, 3),
         xlab="NDVI param value")
par(op)

# Figure 6 ----------------------------------------------------------------
modelNo = 226
MCMCplot(getCI(modelNo,  "logPCT-14R-NDVI-rB0")$samples,
         params = "bRSLR",
         ref_ovl=T,
         labels = "Region Wide",
         ax_sz = 2,
         xlab="RSLR param value")
op = par(mfrow=c(1,2))
MCMCplot(getCI(modelNo,  "logPCT-14R-NDVI-rB0")$samples,
         params = "bRSLR",
         ref_ovl=T,
         labels = "Region Wide",
         ax_sz = 2,
         xlim=c(-3, 3),
         xlab="RSLR param value")

MCMCplot(getCI(modelNo,  "logPCT-14R-NDVI-rB0")$samples,
         params = "bRSLR",
         ref_ovl=T,
         labels = "Region Wide",
         ax_sz = 2,
         xlab="RSLR param value")
par(op)



# Figure 7 ----------------------------------------------------------------
nBreaks = 9
pal = brewer.pal(nBreaks, "RdYlBu")
pal = pal[length(pal):1]

# modelNo = 58; modelForm = "logWET-14R-NDVI"; figureNo = 998 #areal
modelNo = 210; modelForm = "logPCT-14R-NDVI-rB0"; figureNo = 999 #percent
modelOutput = getCI(modelNo, modelForm)$samples

toFile = T

for (covar in c("CS","WH","TR","RSLR","NDVI")) {
  tryCatch({
    summ = MCMCsummary(modelOutput, paste0("b", covar)) %>% as.data.frame() %>% cbind(region=1:nrow(.))
    
    # tryCatch will exit here if the covariate is non-regional
    
    if (toFile){
      png(sprintf("Figures/Figure%s-%s-%s.png", figureNo, modelNo, covar), width = 8, height = (8*0.85), units="in", res=300) # Division by 4 is for the 2x2 plot matrix
    }
    
    summ[9:10,] = summ[10:9,] # Swap summaries 9 and 10
    row.names(summ)[9:10] = row.names(summ)[10:9] # Swap summaries 9 and 10
    summ = summ[nrow(summ):1,] # Reverse summaries to match W to E map
    
    # Plot
    plot(thk99buff, border=NA, col=NA, axes=T, main=sprintf("%s ~ %s", substring(modelForm, 4, 6), covar))
    plot(coastlines,add=T,lty=2)
    
    breaks = classIntervals(seq(-3,3,by=0.05), n=nBreaks, style="quantile")$brks
    brknMed = cut(summ$`50%`, breaks)
    cols = pal[brknMed]
    cols[summ$`50%` == breaks[1]] = pal[1]
    cols[is.na(cols)] = "red"
    
    plot(HUC4inPlot, col=cols, add=T)
    
    # Labels
    labs = paste0(HUC4inPlot$SHORTNAME2, "\n", HUC4inPlot$region)
    HUC4visible = crop(HUC4inPlot, extent(par('usr'))) # Crop HUC4inPlot to only the plot region so that labels are placed within plot
    text(coordinates(HUC4visible), labels = labs, cex = 0.7)
    
    plot(stateMap, add=T, lty=2)
    
    # Whisker plot for 95% CIs
    if (toFile)
    {
      scale = 1.2
      sz = c(3,2)*scale
    } else {
      sz = c(3,2)
    }
    subplot(x = mean(par("usr")[1:2]),
            y = 25,
            size = sz,
            type = 'fig',
            pars = list(
              mar=c(2.5,2,0,0),
              mgp=c(1.25,0.25,0)
            ),
            fun = {
              signif = (summ$`2.5%` * summ$`97.5%`) > 0
              pch = ifelse(signif, 19, 21)

              plot(summ$`50%`,
                   pch = pch,
                   ylim = c(-3,3),#c(min(summ$`2.5%`), max(summ$`97.5%`)),
                   xaxt = "n",
                   yaxt = "n",
                   xlab = "Region",
                   ylab = "Coefficient",
                   cex.axis=0.6,
                   cex.lab=0.6
              )
              
              # Background
              trueXlim = par('usr')[1:2]
              trueYlim = par('usr')[3:4]
              for (i in 1:length(pal))
              {
                col = pal[i]
                col = gsub('^(#)(.*)$', '\\1\\255', col) # Apply transparency to color
                if (i == 1) {
                  rect(trueXlim[1], trueYlim[1], trueXlim[2], breaks[i+1], col=col, border=NA)
                } else if (i == length(pal)) {
                  rect(trueXlim[1], breaks[i], trueXlim[2], trueYlim[2], col=col, border=NA)
                } else {
                  rect(trueXlim[1], breaks[i], trueXlim[2], breaks[i+1], col=col, border=NA)
                }
              }
              # Points and bars
              axis(1, at = 1:nrow(summ), labels = summ$region, cex.axis = 0.6, tck=0.01)
              axis(2, at = -3:3, labels = -3:3, cex.axis = 0.6, tck=0.01)
              arrows(1:nrow(summ), summ$`2.5%`, 1:nrow(summ), summ$`97.5%`, length=0.05, angle=90, code=3)
              points(summ$`50%`, pch = pch, bg="white")
              abline(h=0, lty=2)
            })
    if (toFile) {
      dev.off()
    }
    
  }, error=function(x){
    print(sprintf("%s not found in posteriors? %s", covar, x))
  })
}


# Not Included ------------------------------------------------------------

# Correlation plot
panel.cor <- function(x, y, digits=3, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
} #https://www.r-bloggers.com/scatterplot-matrices-in-r/
pairs(~CS+TR+RSLR+WH+NDVI, data=thk99buff_n,
             lower.panel=panel.smooth, upper.panel=panel.cor, 
             pch=20, main="Correlation Matrix")




# Data values plots
source("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/RUtilityFunctions/plotRegions.R")
plotRegions(states=T)
points(coordinates(thk99buff), pch=21, cex=thk99buff_n$logWET+2, col="black", bg="red")

plotRegions(states=T)
points(coordinates(thk99buff), pch=21, cex=thk99buff_n$RSLR+2, col="black", bg="blue")

plotRegions(states=T)
points(coordinates(thk99buff), pch=21, cex=thk99buff_n$NDVI+2, col="black", bg="green")

plotRegions(states=T)
points(coordinates(thk99buff), pch=21, col="black", bg="black")
points(coordinates(thk99buff), pch=21, cex=thk99buff_n$WH+2, col="black", bg="red")

plotRegions(states=T)
points(coordinates(thk99buff), pch=21, cex=thk99buff_n$TR, col="black", bg="yellow")



# More data values plots
impacts = data.frame(
  covar=c("CS","WH","TR","RSLR","NDVI"),
  impact=c(-1, 1, -1, 1, -1)
)

covarSummPerRegion = thk99buff@data %>% group_by(region) %>% dplyr::select(region, CS, WH, TR, RSLR, NDVI) %>% #group and extract
  summarize_all(funs(a_q025=quantile(., probs=0.025), b_mean=mean, c_med=median, d_q975=quantile(., probs=0.975))) %>% #summarise
  dplyr::select(noquote(order(colnames(.)))) %>% # order columns so covariates summaries are near each other
  merge(., hucData[c("region", "SHORTNAME2")], by = "region")

summs = names(covarSummPerRegion)[!names(covarSummPerRegion) %in% c("region","SHORTNAME2")]

nBreaks = 5
pal = brewer.pal(nBreaks, "RdYlBu")
pal = pal[length(pal):1]

op = par(mfrow = c(5,3),mar=c(0,0,0,0),oma=c(0,0,0,0))
for (covar in summs)
{
  baseCovar = unlist(strsplit(covar, "_"))[1] #e.g. CS_mean --> baseCovar = CS
  type = unlist(strsplit(covar, "_"))[3] #e.g. CS_mean --> type = mean
  if (type == "mean") {
    next()
  }
  
  vals = covarSummPerRegion %>% dplyr::select(region, covar)
  vals = vals[HUC4inPlot$region,] %>% pull(covar)
  
  # Multiply each value by 1 or -1 depending on impact correlation
  # (e.g. as NDVI increases, wetloss decreases, so to color by impact, mult by -1)
  vals = vals * impacts[impacts$covar == baseCovar,]$impact
  
  breaks = classIntervals(vals, n = nBreaks, style = "quantile")$brks
  # Apply breaks
  tryCatch({
    brknCovar = cut(vals, breaks)
  }, error = function(x){
    print(sprintf("Smaller breaks for %s", covar))
    smBreaks = classIntervals(vals, n = 4, style = "quantile")$brks
    smBreaks[1] = -Inf
    smBreaks[length(smBreaks)] = Inf
    brknCovar <<- cut(vals, smBreaks)
  })
  cols = pal[brknCovar]
  cols[vals == breaks[1]] = pal[1]
  cols[is.na(cols)] = "green"
  plot(HUC4inPlot, col=cols)
  title(main = paste(baseCovar, type), line = -6)
}
par(op)

