library(shiny)

library(RPostgreSQL)
library(postGIStools)

library(raster)

library(magrittr)
library(dplyr)
library(MCMCvis)


getModName = function(modelNo, climScen, year, ha=NULL, bw=NULL, vp=NULL, mc=NULL) {
  if (year > 2006 && climScen > 1) { year = paste0("_", year) } else { year = ""}
  climScen = c("", "-RCP3", "-RCP85")[climScen]
  if (all(!is.null(c(ha, bw, vp, mc)))) {
    restore = paste0("-", ha, bw, vp, mc)
  } else {
    restore = ""
  }

  return(sprintf("%s%s%s%s", modelNo, climScen, year, restore))
}

loadScenario = function(modelNo, climScen, year, ha=NULL, bw=NULL, vp=NULL, mc=NULL) {
  modName = getModName(modelNo, climScen, year, ha, bw, vp, mc)

  x = read.delim(sprintf("Data/%s.txt", modName), sep = "\t", check.names=F)

  assign(modName, x, envir = .GlobalEnv)
  return(x)
}

pw = "powpass17"
con <- dbConnect(PostgreSQL(), dbname = "gistest", user = "power_user",
                 host = "13.58.102.61", port = 5432,
                 password = pw)
rm(pw)

thk99buff = get_postgis_query(con, 'SELECT * FROM thk99buff', geom_name = "geom")
# thk99buff = readOGR("Data/thk99buff.shp", "thk99buff")
thk99buff_n = read.delim("Data/thk99buff_n.txt", sep="\t", check.names = F)

# if(!exists("thk99buff") | !exists("thk99buff_n")) {
# thk99buff = readOGR("Data/thk99buff.shp", "thk99buff")
#   thk99buff_n = read.delim("Data/thk99buff_n.txt", sep="\t", check.names = F)
# }

minWET = floor(min(thk99buff$logWET))
maxWET = ceiling(max(thk99buff_n$logWET))
# maxWET = ceiling(max(`58-RCP85_2300`$`50%`))
# maxWET = ceiling(max(`58`$`50%`))

scenName = "Base"
baseLoss = sum(exp(thk99buff_n$logWET))


colordefault = c("green","yellow","red")
colorblind = c("#7fbf7b", "#f7f7f7","#af8dc3")
legendColors = colordefault
pal = colorNumeric(
  palette = legendColors,
  domain = minWET:maxWET,
  na.color="#FF0000")

shinyServer(function(input, output, session) {
  
  # Initial map draw
  # ex = extent(thk99buff[thk99buff$region %in% c(9,10),])
  
  output$mymap <- renderLeaflet({
    leaflet(data=thk99buff) %>%
      addProviderTiles("Stamen.Toner") %>%
      #addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke=F,
                  fillColor=~pal(logWET),
                  fillOpacity=1,
                  label = ~as.character(round(exp(logWET), digits=2))) %>%
      addLegend(position = "bottomright",
                colors = pal(minWET:maxWET),
                labels = round(exp(minWET:maxWET),digits = 2),
                title = "Wetland Change <br> (hectares)") # %>%
      #fitBounds(ex@xmin, ex@ymin, ex@xmax, ex@ymax)
  })
  
  # Updater
  observe({
    input$year #Numerical year
    input$scenario #Coded scenario
    
    year = input$year
    scenNo = as.numeric(input$scenario)
    
    # input$vegRestore
    # input$bwRestore
    # input$haRestore
    
    if (input$year == 2006 || input$scenario == 1)
    {
      leafletProxy("mymap", data = thk99buff) %>% clearShapes() %>%
        addPolygons(stroke=F,
                    fillColor=~pal(logWET),
                    fillOpacity=1,
                    label = ~paste(round(exp(logWET), digits=2), "hectares"))
      return()
    }
    
    # scens = c("",sprintf("-RCP3_%s", year),sprintf("-RCP85_%s", year))
    # dType = c("","-NR","-od")
    # scenName = sprintf("58-Any-10y-Y%s%s", scens[scenNo], dType[3])
    # if (exists(scenName))
    # {
    #   scen = get(scenName)
    # } else {
    #   loc = sprintf("../../DataPrep/Results/LAonly/%s.RData", scenName)
    #   load(loc)
    # 
    #   scen = MCMCsummary(output$samples) %>%
    #     as.data.frame() %>%
    #     cbind(param=row.names(.), .) %>%
    #     mutate(param=as.character(param)) %>%
    #     filter(grepl("logWET", param))
    # 
    #   assign(scenName, scen, envir = .GlobalEnv)
    # 
    #   rm(ls="output")
    # }
    
    scenName = getModName(58, scenNo, year)
    if (exists(scenName))
    {
      scen = get(scenName)
    } else {
      scen = loadScenario(58, scenNo, year)
    }
    
    leafletProxy("mymap", data = thk99buff) %>%
      clearShapes() %>%
      #clearControls() %>%
      addPolygons(stroke=F,
                  fillColor=~pal(exp(scen$`50%`)),
                  fillOpacity=1,
                  label = ~paste(round(exp(logWET), digits=2), "hectares"))
    # 
    # output$wetloss <- renderUI({
    #   scenarioLoss = sum(exp(scen$`50%`))
    #   HTML(paste(sep="<br/>",
    #     paste("Base Wetland Loss:", round(baseLoss, digits=2), "hectares"),
    #     paste("Scenario Wetland Loss:", round(scenarioLoss, digits=2), "hectares"),
    #     paste("Change in Loss:", round(scenarioLoss - baseLoss, digits=2), "hectares")
    #   ))
    # })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if (exists(scenName)) {
        paste(scenName, '.csv', sep='')
      } else {
        return("wetloss9606.csv")
      }
    },
    content = function(con) {
      if (exists(scenName)) {
        write.csv(get(scenName), con)
      } else {
        write.csv(thk99buff_n[c("ORIG_FID","WET","logWET")])
      }
    }
  )
  
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
})
