#
# This is the server logic of a Shiny web application. 

library(shiny)
library(leaflet)
library(rgdal)

library(RPostgreSQL)
library(postGIStools)

source("mysqlcfg.R")

maxWET = 6

#########################################
# Database Connection + Loading/Merging
#########################################
if(exists("user") || exists("pw")) {  
  con <- dbConnect(PostgreSQL(), dbname = "postgiz", user = user,
                    host = "52.14.87.100", port = 5432,
                    password = pw)
  rm(pw);rm(user)
}


# TODO: Merge can likely be done on the database side
wetloss = get_postgis_query(con, sprintf('SELECT * FROM wetloss WHERE "WET" <= %s', maxWET))
inlandbuff = get_postgis_query(con, "SELECT * FROM thkbuffers", geom_name = "geom")
inlandbuff@data = merge(inlandbuff@data, wetloss, by = "ORIG_FID")

#########################################
# Restoration Scenarios
#########################################
#ORIG_FID list for each baseline
raccBaseline = c(415, 420, 407, 433, 484, 430, 442, 445, 457)
trinityBaseline = raccBaseline
gisleBaseline = c(422, 438, 454, 510, 535, 538, 558, 583)
#ORIG_FID list for restoration
raccRestore = c(462, 447, 465)
trinityRestore = c(404, 409, 413)
gisleRestore = c(480, 487)

raccAvgNDMI = mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% raccRestore]) -
  mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% raccBaseline])
trinityAvgNDMI = mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% trinityRestore]) -
  mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% trinityBaseline])
gisleAvgNDMI = mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% gisleRestore]) -
  mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% gisleBaseline])

bwNDMI = mean(c(raccAvgNDMI, gisleAvgNDMI))
vegNDMI = trinityAvgNDMI

#########################################
# Oil Drilling Scenarios
#########################################
# Subsidence (Chang, Mallman, Zoback 2014)
years=c(1965,1982,1992,2002,2012)
cumsub=c(0,-8,-16,-17.75,-19)

s_pred_max = 20
asymp_pred = -20
inflect_pred = 1987

s.try=data.frame(s=seq(0,s_pred_max,by=0.01), ssr=rep(NA, length(seq(0,s_pred_max,by=0.01))))
for(s in seq(0,s_pred_max,by=0.01)){
  fit=SSlogis(years,asymp_pred,inflect_pred,s)
  ssr=sum((cumsub-fit)^2)
  s.try[s.try$s==s,]$ssr = ssr
}
s_pred = s.try$s[s.try$ssr == min(s.try$ssr)]

cumsub_pred = SSlogis(seq(1965,2014),asymp_pred,inflect_pred,s_pred)
# plot(cumsub_pred~seq(1965,2015))
# points(cumsub~years,col="red")

sub_rate=-diff(cumsub_pred)

# Oil NDMI Reduction via Spill (Mishra et al 2012; Khanna et al 2013; Hese and Schmullius 2009)
oiledNDMI = c(0.079,0.127,0.253,.347,0.393,0.418,0.413,0.408,0.400,0.398,0.400,0.400)
nonNDMI = c(0.134,0.209,0.356,0.400,0.411,0.416,0.411,0.400,0.395,0.395,0.387,0.387)

ratioNDMI = 1-oiledNDMI/nonNDMI

oilNDMI = mean(ratioNDMI[1:3])

#########################################
# Drop Database connection
#########################################
dbDisconnect(con)

#########################################
# Shiny Server Interaction
#########################################

colordefault = c("green","yellow","red")
colorblind = c("#7fbf7b", "#f7f7f7","#af8dc3")
legendColors = colordefault

pal = colorNumeric(
  palette = legendColors,
  domain = 0:maxWET, #inlandbuff@data$WET)
  na.color="#FF0000")

#Prediction Function (!!!DETERMINISTIC!!!)
regionNDMI = c(-0.00019, -0.00041)
regionTR = c(0.361169,1.177906)
calcNewWET = function(RSLR,NDMI,TR,region) {return(-1.3022 + 0.156583*RSLR + regionNDMI[region]*NDMI + regionTR[region]*TR)}

shinyServer(function(input, output, session) {

  # Initial map draw
  output$mymap <- renderLeaflet({
    
    # firstRender = TRUE
    # if(firstRender)
    #Calculations
    
    
    #Draw map
    leaflet(data=inlandbuff) %>%
      addProviderTiles("Stamen.Toner") %>%
      #addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke=F,
                  fillColor=~pal(WET), fillOpacity=1) %>%
      addLegend(position = "bottomright", pal = pal, values = 0:maxWET, title = "Wetland Change <br> (hectares)")
      #addMarkers(lng=174.768, lat=-36.852)
    
  })
  
  # Updater
  observe({
    input$slr
    
    dat=inlandbuff@data[c("RSLR","NDMI","TR", "region")]
    if(input$oilSubsidence)
      dat$RSLR = dat$RSLR + sub_rate[input$years]*10
    
    if(input$oilSpill && input$years < 10)
      dat$NDMI = dat$NDMI-abs(dat$NDMI)*oilNDMI
    
    if(input$vegRestore)
      dat$NDMI = dat$NDMI + vegNDMI
    
    if(input$bwRestore)
      dat$NDMI = dat$NDMI + bwNDMI
    
    inlandbuff@data$preds = exp(apply(dat,1,function(x) calcNewWET(x[1],x[2],x[3],x[4])))
    
    leafletProxy("mymap", data = inlandbuff) %>% clearShapes() %>%
      addPolygons(stroke=F,
                  fillColor=~pal(preds), fillOpacity=1)
  })
  
  # output$histWetLoss <- renderPlot({
  #   op=par(bg="#d5e8ea")
  #   plot(density(inlandbuff@data$WET), main = "Wetland Loss", xlab="Hectares of Wetland Loss", ylab = "Density of Areas with Loss")
  #   par(op)
  # })
  
  output$rslrUI <- renderUI({
    slrVals = paste(paste0('"', c("Current","1","2","3"), '"'), collapse = ',')
    list(
      (HTML(
        sprintf("
                <script type='text/javascript'>
                /*$(document).ready(function() {
                var vals = [%s];
                $('#slr').data('ionRangeSlider').update(
                {values:vals,
                min: 0,
                max: 3,
                from: 0})
                })*/
                </script>
                ", slrVals)))
    )
  })
  
  #Close database connection on app exit
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
})
