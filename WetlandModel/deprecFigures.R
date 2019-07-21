# Deprecated figures that are not included in the manuscript

# Areal Posteriors' Caterpillar Plots -------------------------------------
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

# Areal NDVI Posterior Caterpillar PlotS ----------------------------------
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



# Percent Posteriors' Caterpillar Plots -----------------------------------
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



# Percent RSLR Posterior Caterpillar PlotS --------------------------------
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


# Figure X1 - Areal NDVI Posterior Caterpillar Plot -----------------------
MCMCplot(getCI(241, "logWET-14R-NDVI")$samples,
         params = "bNDVI",
         ref_ovl=T,
         labels = "NGOM Wide",
         ax_sz = 2,
         xlab="NDVI param value")

# Figure X2 - Percent RSLR Posterior Caterpillar Plot ---------------------
MCMCplot(getCI(226,  "logPCT-14R-NDVI-rB0")$samples,
         params = "bRSLR",
         ref_ovl=T,
         labels = "NGOM Wide",
         ax_sz = 2,
         xlab="RSLR param value")
