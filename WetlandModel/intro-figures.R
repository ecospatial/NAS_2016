
# Intro Figure 1 ----------------------------------------------------------

toFile = F

if (toFile){
  png("Figures/Figure1Intro.png", width = 8, height = 8*0.85, units="in", res=300)
}
# Plot
{
  # Initialize plot
  plot(thk99buff, border=NA, col=NA, axes=T)
  rec = par("usr")
  rect(rec[1], rec[3], rec[2], rec[4], col="lightblue")
  
  # Land
  land = readOGR("C:/DATA/General Maps/Coastlines/GSHHS_2.3.5_04-2016/USCoast_h_L1.shp", "USCoast_h_L1")
  plot(land, add=T, col="beige", border=NA)
  
  # State lines
  plot(stateMap, add=T, lty=2)
  
  # Coastlines
  plot(coastlines, add=T,lty=2)
  
  # State labels
  stateMapCrop = crop(stateMap, extent(par("usr")))
  stateMapCrop$STATE = c("AL", "FL", "LA", "MS", "TX")
  labCoords = coordinates(stateMapCrop)
  labCoords[2,1] = labCoords[2,1]+0.8
  labCoords[3,1] = labCoords[3,1]-0.7
  text(labCoords, labels = stateMapCrop$STATE, cex = 0.7)
  
  # THK99
  plot(thk99buff, add=T, border="red", col="orange")
  #points(coordinates(thk99buff), col="yellow", pch=21)
  
  # Zoom plot box
  recZoom = c(-90.78583, -88.82394, 28.94080, 30.12881)
  rect(recZoom[1], recZoom[3], recZoom[2], recZoom[4], col=NA, border="blue")
  zoomW = recZoom[2] - recZoom[1]
  zoomH = recZoom[4] - recZoom[3]
  
  addscalebar()
  addnortharrow(scale=0.5)
  
  # Zoom subplot
  subplot(
    x = mean(par("usr")[1:2]),
    y = 26,
    size = scaleSubplot(c(2.5,(2.5*zoomH)/zoomW) , toFile),
    type = 'fig',
    pars = list(
      mar=c(0,0,0,0),
      mgp=c(1.5,0.75,0)
    ),
    fun = {
      plot(thk99buff, xlim=c(extent(recZoom)@xmin, extent(recZoom)@xmax), ylim=c(extent(recZoom)@ymin, extent(recZoom)@ymax))
      rec = par("usr")
      rect(rec[1], rec[3], rec[2], rec[4], col="lightblue")
      box(lty = 1, col = "blue")
      
      # Land
      land = readOGR("C:/DATA/General Maps/Coastlines/GSHHS_2.3.5_04-2016/USCoast_h_L1.shp", "USCoast_h_L1")
      plot(land, add=T, col="beige", border=NA)
      
      # State lines
      plot(stateMap, add=T, lty=2)
      
      # Coastlines
      plot(coastlines, add=T,lty=2)
      plot(thk99buff, xlim=c(extent(recZoom)@xmin, extent(recZoom)@xmax), ylim=c(extent(recZoom)@ymin, extent(recZoom)@ymax),
           border="red", col="orange", add=T)
      addscalebar()
      
    })
}
if (toFile) {
  dev.off()
}

