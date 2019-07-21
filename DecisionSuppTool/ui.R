#
# This is the user-interface definition of a Shiny web application. 

library(shiny)
library(shinythemes)
library(leaflet)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)

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
                               $('[id^=spinner-]').after('<center>Running Scenarios...</center>');
                               });"))),
    
    #Make popovers wider
    tags$style(type = "text/css", ".popover{
            max-width: 600px;
          }"),   
    
    #Unbolden control labels
    tags$style(type = "text/css", "label.control-label{
            font-weight: normal;
          }"),
    
    #Load map into main panel with loading spinner
    withSpinner(leafletOutput("mymap", width = "100%", height = "100%"), type=6),
    
    #Left panel
    absolutePanel(top=10,
                  left=10,
                  style="z-index:999;
                        background: rgba(255,255,255,0.95);
                        padding:20px;
                        border: 1px solid #333333;
                        border-radius: 5px",
                  draggable=F,
                  
                  ##############################
                  
                  h4("Year"),
                  div(sliderTextInput(inputId = "year", label = NULL, #"Year",
                              choices = list("2006","2100","2300"),
                              grid=T,
                              selected = "2006"
                  ), id = "yearLab"),
                  bsPopover(id = "yearLab", title = "Year",
                            content = blurbs$year,
                            placement = "right", trigger = "hover", options=list(container="body")),
                  
                  ##############################
                  
                  hr(style="color:#337ab7"),
                  
                  h4("Climate Change Scenario"),
                  div(radioButtons("scenario", label = NULL, #"Climate Change Scenario",
                               choices = list("None" = 1, "RCP 3" = 2, "RCP 8.5" = 3), 
                               selected = 1, inline = TRUE), id = "scenarioLab"),
                  bsPopover(id = "scenarioLab", title = "Climate Change Scenarios",
                            content = blurbs$scenarios,
                            placement = "right", trigger = "hover", options=list(container="body")),
                  
                  ##############################
                  
                  hr(style="color:#337ab7"),
                  
                  h4("Restoration Decisions"),
                  div(sliderTextInput(inputId = "vegRestore", label = "Vegetative Restoration",
                                      choices = list("None","Current","All"),
                                      grid=T,
                                      selected = "Current"
                  ), id = "vegRestoreLab"),
                  bsPopover(id = "vegRestoreLab", title = "Restoration with Plantings",
                            content = blurbs$vegRestore,
                            placement = "right", trigger = "hover", options=list(container="body")),
                  
                  div(sliderTextInput(inputId = "bwRestore", label = "Breakwater Restoration",
                                      choices = list("None","Current","All"),
                                      grid=T,
                                      selected = "Current"
                  ), id = "bwRestoreLab"),
                  bsPopover(id = "bwRestoreLab", title = "Restoration with Breakwaters",
                            content = blurbs$bwRestore,
                            placement = "right", trigger = "hover", options=list(container="body")),
                  
                  div(sliderTextInput(inputId = "haRestore", label = "Hydrological Restoration",
                                      choices = list("None","Current","All"),
                                      grid=T,
                                      selected = "Current"
                  ), id = "haRestoreLab"),
                  bsPopover(id = "haRestoreLab", title = "Hydrological Alteration",
                            content = blurbs$haRestore,
                            placement = "right", trigger = "hover", options=list(container="body")),
                  
                  ##############################
                  
                  hr(style="color:#337ab7"),
                  
                  h4("Oil Production Trade-offs"),
                  div(checkboxInput("oilSpill", "Oil Spills", F), id = "oilSpillLab"),
                  bsPopover(id = "oilSpillLab", title = "Oil Spill Presence",
                            content = blurbs$oilSpill,
                            placement = "right", trigger = "hover", options=list(container="body")),
                  
                  
                  div(checkboxInput("oilSubsidence", "Subsidence", F), id = "oilSubsidenceLab"),
                  bsPopover(id = "oilSubsidenceLab", title = "Drilling-induced Subsidence",
                            content = blurbs$subsidence,
                            placement = "right", trigger = "hover", options=list(container="body")),

                  ##############################
                  
                  hr(style="color:#337ab7"),
                  # htmlOutput("wetloss"),
                  downloadButton('downloadData', 'Download Wetloss')
    )
    ,
    
    # #Bottom Panel
    # absolutePanel(bottom = 20, left = "40%", width = 500, height = 300, style="z-index:999; background: #d5e8ea; padding:20px; border: 1px solid #337ab7; border-radius: 5px", draggable=TRUE,
    #               div()
    # ),
    #Data attribution
    absolutePanel(bottom = 0,left=0,right=396,height="16px",style="z-index:999; background: rgba(255,255,255,0.7); font-size: 11px; color: #333",tags$span("Modeling by"),tags$a("Tyler Hardy",href="www.tylerthardy.com"),tags$span("and Wei Wu 2016-2017; Data from USGS, and others referenced in respective tooltips."))
    )
  )