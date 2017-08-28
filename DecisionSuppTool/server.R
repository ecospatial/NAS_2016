#
# This is the server logic of a Shiny web application. 

library(shiny)
library(leaflet)
library(rgdal)

library(RPostgreSQL)
library(postGIStools)

source("mysqlcfg.R")

maxWET = 6 #Maximum wetland loss to report; set at 6 because it aids in visualization and excludes outliers.

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

source("scenarios.R")

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
