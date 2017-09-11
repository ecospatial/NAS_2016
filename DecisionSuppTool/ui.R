#
# This is the user-interface definition of a Shiny web application. 

library(shiny)
library(shinythemes)
library(leaflet)
library(shinyBS)
library(shinycssloaders)

source("blurbs.R")

shinyUI(
  bootstrapPage(
    theme=shinytheme("spacelab"),
    #Make content fill whole page
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    #Make loading spinner visible and centered; move zoom buttons to top right
    tags$style(type = "text/css", ".shiny-spinner-output-container {
                                      width: 100%;
                                      height: 100%;
                                   }
               
                                   .leaflet-top.leaflet-left {
                                      right: 0;
                                      left: initial;
                                   }
                                   .leaflet-left .leaflet-control {
                                      margin-right: 10px;
                                   }"),
    #Add text to loading spinner
    tags$head(tags$script(HTML("$(document).ready(function(){
                                  //Add text to loading spinner
                                  $('[id^=spinner-]').after('<center>Running Scenario...</center>');
                               });"))),
    
    #Load map into main panel with loading spinner
    withSpinner(leafletOutput("mymap", width = "100%", height = "100%"), type=6),
    
    #Left panel
    absolutePanel(top=10,left=10, style="background: rgba(255,255,255,0.95); padding:20px; border: 1px solid #333333; border-radius: 5px", draggable=TRUE,
        h3("Options"),
        hr(style="color:#337ab7"),
        
        #########################################
        
        h4("Climate Scenario"),
        uiOutput("rslrUI"),
        sliderInput(inputId = "slr", label = "Sea-level Rise Acceleration (mm/yr/yr)",
                    min = 0, max = 3,
                    step = 1,
                    value = 0
        ),
        bsPopover(id = "slr", title = "Sea-level Rise Acceleration",
                  content = blurbs$slr,
                  placement = "right", trigger = "hover", options=list(container="body")),
        
        #########################################
        
        h4("Oil Production Trade-offs"),
        div(checkboxInput("oilSpill", "Oil Spills", F), id = "oilSpillLab"),
        bsPopover(id = "oilSpillLab", title = "Oil Spill Presence",
                  content = blurbs$oilSpill,
                  placement = "right", trigger = "hover", options=list(container="body")),
        
        div(checkboxInput("oilSubsidence", "Subsidence", F), id = "oilSubsidenceLab"),
        bsPopover(id = "oilSubsidenceLab", title = "Drilling-induced Subsidence",
                  content = blurbs$subsidence,
                  placement = "right", trigger = "hover", options=list(container="body")),
        
        #########################################
        
        h4("Restoration Decisions"),
        div(checkboxInput("vegRestore", "Vegetative Restoration", F), id='vegRestoreLab'),
        bsPopover(id = "vegRestoreLab", title = "Restoration with Plantings",
                  content = blurbs$vegRestore,
                  placement = "right", trigger = "hover", options=list(container="body")),
        
        div(checkboxInput("bwRestore", "Breakwater Restoration", F), id = "bwRestoreLab"),
        bsPopover(id = "bwRestoreLab", title = "Restoration with Breakwaters",
                  content = blurbs$bwRestore,
                  placement = "right", trigger = "hover", options=list(container="body")),
        
        # plotOutput("histWetLoss")
        
        #########################################
        
        h4("Timespan"),
        sliderInput(inputId = "years", label = "Number of Years to Project:",
                   min = 1, max = 50,
                   step = 1,
                   value = 0
        )
                   
    )
    ,

    #Bottom Panel
    absolutePanel(bottom = 10, left = "40%", width = 500, height = 300, style="background: #d5e8ea; padding:20px; border: 1px solid #337ab7; border-radius: 5px", draggable=TRUE,
        div()
    )
    ,
    #Data attribution
    absolutePanel(bottom = 0,left=0,right=396,height="16px",style="background: rgba(255,255,255,0.7); font-size: 11px; color: #333",tags$span("Modeling by"),tags$a("Tyler Hardy",href="www.tylerthardy.com"),tags$span("and Wei Wu 2016-2017; Data from USGS, and others referenced in respective tooltips."))
  )
)