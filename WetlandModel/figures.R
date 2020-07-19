library(MCMCvis)
library(dplyr)
library(rgdal)
library(raster)

library(RColorBrewer)
library(classInt)
library(sp)
library(TeachingDemos) #subplot
library(prettymapr) #scalebar and north arrow

source("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/RUtilityFunctions/bayesianTools.R")
source("loadTHK99.R")
setwd("C:/Users/GCRLWuHardy/Documents/00000 2017/NAS_2016/WetlandModel")




toFile = F



loadTHK99data(local=T,regions="ALL")

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

# Load general map data
coastlines = readOGR("C:/DATA/General Maps/Coastlines/GSHHS_2.3.5_04-2016/USCoast_h_L1_Line.shp", "USCoast_h_L1_Line")
coastlines = spTransform(coastlines, proj4string(thk99buff))
stateMap = readOGR("C:/DATA/General Maps/Gulf States/US_GulfStates.shp", "US_GulfStates")

# Function to scale subplot to match plots-to-file to RStudio appearance
scaleSubplot = function(size, toFile) {
  if (toFile)
  {
    scale = 1.2
    sz = size*scale
  } else {
    sz = size
  }
}

# Table 1 -----------------------------------------------------------------
areal=combineDIC("logWET-14R-NDVI")
areal=areal[c("modelNo","fixed","random","DIC","sig","type")]
areal
write.table(areal, "Figures/TableI.txt", sep="\t")

# Table 2 -----------------------------------------------------------------
percent=combineDIC("logPCT-14R-NDVI")
percent=percent[c("modelNo","fixed","random","DIC","sig","type")]
percent
write.table(percent, "Figures/TableII.txt", sep="\t")

# Figure 1 - Study Area ---------------------------------------------------

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
HUC4inPlot = merge(HUC4inPlot, thk99buff@data %>% dplyr::group_by(HUC4) %>% summarise(NSITES = n()), by = "HUC4")
HUC4inPlot = merge(HUC4inPlot, hucData, by = "HUC4")

strangeOrder = order(HUC4inPlot$region) # Order HUC4inPlot from 1 to 14
strangeOrder[9:10] = strangeOrder[10:9] # Swap regions 9 and 10
strangeOrder[1:length(strangeOrder)] = strangeOrder[length(strangeOrder):1] # Order from 14 to 1 with strange order (9 and 10 swapped)
HUC4inPlot = HUC4inPlot[strangeOrder,]

# Plot 1.1
if (toFile){
  png("Figures/Figure1.1.png", width = 8, height = 6, units="in", res=300)
}
{
  op=par(mar=c(2,2,0,1))
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
  hackyReorder = c(1:4, 6:5, 7:14)
  labs = paste0(HUC4inPlot$SHORTNAME2, "\n", HUC4inPlot$region[hackyReorder])
  HUC4visible = crop(HUC4inPlot, extent(par('usr'))) # Crop HUC4inPlot to only the plot region so that labels are placed within plot
  text(coordinates(HUC4visible), labels = labs, cex = 0.7)
  text(coordinates(HUC4visible[HUC4visible$avgdis >= breaks[8],]), labels = labs[HUC4visible$avgdis >= breaks[8]], cex = 0.7, col="WHITE")

  # Widgets
  addscalebar()
  addnortharrow(scale=0.5)
  
  # Zoom boundaries
  rect(-90.5, 28, -89.5, 31, border="red", lwd=3)
  
  # Bar plot for n
  labCex = 0.8
  subplot(
    x = par("usr")[1] + (par("usr")[2]-par("usr")[1])*0.4,
    y=24.5,
    size = scaleSubplot(c(3,2), toFile),
    type = 'fig',
    pars = list(
      mar=c(4,2,0,0),
      mgp=c(1.5,0.75,0)
    ),
    fun = {
      x = barplot(HUC4inPlot$NSITES,
                  xlab = "Watershed",
                  ylab="n",
                  ylim=c(0, 1.20*max(HUC4inPlot$NSITES)),
                  names=HUC4inPlot$region[hackyReorder],
                  col="orange", #cols
                  cex.axis=labCex,
                  cex.names=labCex,
                  cex.lab=labCex)
  
      text(x = x, y = HUC4inPlot$NSITES, label = HUC4inPlot$NSITES, pos = 3, cex = labCex)
    })

  # Scalebar
  subplot(
    x = par("usr")[1] + (par("usr")[2]-par("usr")[1])*0.8,
    y = 25,
    size = scaleSubplot(c(1,1.5), toFile),
    fun = {
      xl <- 1
      yb <- 1
      xr <- 1.2
      yt <- 2
      plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
      rect(
        xl,
        head(seq(yb,yt,(yt-yb)/nBreaks),-1),
        xr,
        tail(seq(yb,yt,(yt-yb)/nBreaks),-1),
        col=pal
      )
      mtext("Median\nDischarge",side=3,at=1,cex=labCex)

      mtext(round(breaks, digits=0),side=2,at=c(1, tail(seq(yb,yt,(yt-yb)/nBreaks),-1)),las=2,cex=labCex)
    })
}
if (toFile) {
  dev.off()
}
# Plot 1.2
if (toFile){
  png("Figures/Figure1.2.png", width = 3, height = 6, units="in", res=300)
}
{
  op=par(mar=c(2,2,0,1))
  # Initialize plot
  plot(thk99buff, xlim=c(-90.5,-89.5), ylim=c(28,31), border=NA, col=NA, axes=T)
  
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
  
  addscalebar()
  addnortharrow(scale=0.5)
}
if (toFile) {
  dev.off()
}


# Figure 2 - Boxplots of Covariates ---------------------------------------
if (toFile)
{
  png(sprintf("Figures/Figure2.png", figureNo, covar), width = 12, height = 6, units="in", res=300)
}
units = list(
  RSLR = "mm/yr",
  WH = "m",
  CS = "%",
  TR = "m",
  NDVI = "NDVI",
  logWET = "Wetland loss (log hectares)"
)

# Reverse order regions
thk99buff$regionO = ordered(thk99buff$region, levels=levels(factor(thk99buff$region))[14:1])

op = par(mfrow=c(2,3))
for (covar in c("RSLR", "WH", "TR", "CS", "NDVI", "logWET"))
{
  frmla = formula(sprintf("%s~regionO", covar))
  op1 = par(mar=c(5,4,0,0))
  boxplot(frmla, data=thk99buff, xlab = "Watershed", ylab = units[[covar]], cex.axis=1.3, cex.lab=1.3)
  
  plotDims = par("usr")
  height = plotDims[4] - plotDims[3]
  xPos = plotDims[1] - ifelse(toFile==T, 0, 1.5)
  yPos = plotDims[4] - height * 0.01
  labelRegion = legend(xPos, yPos, sprintf("%s)", covar), bty="n", cex=1.3)
  par(op1)
}
par(op)
if (toFile)
{
  dev.off()
}

# Figure 3 - DAG ----------------------------------------------------------
# DAG


# Figure 4 - Predictions vs Observed --------------------------------------
if (toFile)
{
  png("Figures/Figure4.png", width = 8, height = (8*0.85), units="in", res=300)
}

#preds = MCMCpstr(getCI("PP-model58"), params="logWET.p")

set.seed(1337135)
random200 = sample(1:nrow(thk99buff), 200, replace=F)

plot(preds$logWET.p[random200]~thk99buff$logWET[random200],
     xlab = "Observed Loss (log hectares)",
     ylab = "Predicted Loss (log hectares)",
     xlim = range(thk99buff$logWET[random200]),
     ylim = range(thk99buff$logWET[random200]),
     pch=19, col = "#00000077",
     cex.axis=1.5, cex.lab=1.5, cex=1.5)
abline(0,1)

if (toFile)
{
  dev.off()
}


# Figures 5 and 6 ---------------------------------------------------------
nBreaks = 8
pal = brewer.pal(nBreaks, "RdYlBu")
pal = pal[length(pal):1]
labCex = 0.8

#modelNo = 58; modelForm = "logWET-14R-NDVI"; figureNo = 5; plotOrder=c("RSLR","WH","TR") #areal #rB0 for intercept
#modelNo = 58; modelForm = "logWET-14R-NDVI-rB0"; figureNo = 5; plotOrder=c("0") #areal #rB0 for intercept
modelNo = 210; modelForm = "logPCT-14R-NDVI-rB0"; figureNo = 6; plotOrder=c("NDVI","WH","TR","CS") #percent
modelOutput = getCI(modelNo, modelForm)$samples

for (plotNo in 1:length(plotOrder)) {
  covar = plotOrder[plotNo]
  tryCatch({
    summ = MCMCsummary(modelOutput, paste0("b", covar)) %>% as.data.frame() %>% cbind(region=1:nrow(.))
    
    # tryCatch will exit here if the covariate is non-regional
    
    if (toFile){
      png(sprintf("Figures/Fig3and5Parts/Figure%s-%s.png", figureNo, covar), width = 8, height = (8*0.85), units="in", res=300)
    }
    
    summ[9:10,] = summ[10:9,] # Swap summaries 9 and 10
    row.names(summ)[9:10] = row.names(summ)[10:9] # Swap summaries 9 and 10
    summ = summ[nrow(summ):1,] # Reverse summaries to match W to E map
    
    hackyReorder = c(1:4, 6:5, 7:14) 
    
    summ = summ[hackyReorder,]
    
    # Plot
    par(mar=c(5.1,4.1,2.1,2.1))
    plot(thk99buff, border=NA, col=NA,
         axes=T,
         xaxt=if(plotNo>2 | (plotNo==1 & length(plotOrder)==1)) "s" else "n",
         yaxt=if((plotNo==1 | plotNo==3) & length(plotOrder)>1) "s" else "n")
    plot(coastlines,add=T,lty=2)
    
    breaks = classIntervals(seq(-1.5,1.5,by=0.05), n=nBreaks, style="quantile")$brks
    brknMed = cut(summ$`50%`, breaks)
    cols = pal[brknMed]
    cols[summ$`50%` == breaks[1]] = pal[1]
    cols[is.na(cols)] = "red"
    
    plot(HUC4inPlot, col=cols[hackyReorder], add=T)
    
    # Labels
    labs = paste0(HUC4inPlot$SHORTNAME2, "\n", HUC4inPlot$region[hackyReorder])
    HUC4visible = crop(HUC4inPlot, extent(par('usr'))) # Crop HUC4inPlot to only the plot region so that labels are placed within plot
    text(coordinates(HUC4visible), labels = labs, cex = 0.7)
    
    plot(stateMap, add=T, lty=2)
    
    # Top left label
    if (covar == "0") { covar = "b0" }
    labelRegion = legend("topleft", sprintf("%s)", covar), plot = F)$rect
    rect(labelRegion$left + labelRegion$w*0.3, labelRegion$top - labelRegion$h*0.2, labelRegion$left + labelRegion$w, labelRegion$top - labelRegion$h,
         col="white", border=NA)
    legend("topleft", sprintf("%s)", covar), bty="n")
    
    
    #Legend
    subplot(
      x = par("usr")[1] + (par("usr")[2]-par("usr")[1])*0.83,
      y = 25.65,
      size = scaleSubplot(c(1,1.65), toFile),
      fun = {
        xl <- 1
        yb <- 1
        xr <- 1.2
        yt <- 2
        plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
        rect(
          xl,
          head(seq(yb,yt,(yt-yb)/nBreaks),-1),
          xr,
          tail(seq(yb,yt,(yt-yb)/nBreaks),-1),
          col=pal
        )
        
        mtext("Coefficient\nMedian",side=3,at=1,cex=labCex)
        mtext(sprintf("%.2f", round(breaks, digits=2)),side=2,at=c(1, tail(seq(yb,yt,(yt-yb)/nBreaks),-1)),las=2,cex=labCex)
      })
    
    # Whisker plot for 95% CIs
    subplot(x = -91.5,
            y = 25,
            size = scaleSubplot(c(3,2), toFile),
            type = 'fig',
            pars = list(
              mar=c(2.5,2,0,0),
              mgp=c(1.25,0.25,0)
            ),
            fun = {
              summ = summ[hackyReorder,]
              signif = (summ$`2.5%` * summ$`97.5%`) > 0
              pch = ifelse(signif, 19, 21)
              
              plot(summ$`50%`,
                   pch = pch,
                   ylim = c(-3,3),#c(min(summ$`2.5%`), max(summ$`97.5%`)),
                   xaxt = "n",
                   yaxt = "n",
                   xlab = "Watershed",
                   ylab = "Coefficient",
                   cex.axis=labCex,
                   cex.lab=labCex
              )
              
              # Background
              # trueXlim = par('usr')[1:2]
              # trueYlim = par('usr')[3:4]
              # for (i in 1:length(pal))
              # {
              #   col = pal[i]
              #   col = gsub('^(#)(.*)$', '\\1\\255', col) # Apply transparency to color
              #   if (i == 1) {
              #     rect(trueXlim[1], trueYlim[1], trueXlim[2], breaks[i+1], col=col, border=NA)
              #   } else if (i == length(pal)) {
              #     rect(trueXlim[1], breaks[i], trueXlim[2], trueYlim[2], col=col, border=NA)
              #   } else {
              #     rect(trueXlim[1], breaks[i], trueXlim[2], breaks[i+1], col=col, border=NA)
              #   }
              # }
              # Axes
              axis(1, at = 1:nrow(summ), labels = summ$region[hackyReorder], cex.axis = labCex, tck=0.01)
              axis(2, at = -3:3, labels = -3:3, cex.axis = labCex, tck=0.01)
              # Points and bars
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
