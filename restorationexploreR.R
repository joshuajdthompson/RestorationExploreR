################################################################################
#                   Author: Joshua Thompson
#   O__  ----       Email:  pwthom19@aacounty.org
#  c/ /'_ ---
# (*) \(*) --
# ======================== Script  Information =================================
# PURPOSE: Restoration Explorer App for NGOs
#
# PROJECT INFORMATION:
#   Name: Shiny App for NGOs
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	 01/28/2022    Created script                                   JThompson (JT)
#  02/24/2022    Added markdown docs                              JT
#  03/01/2022    Fixed selection drop bug                         JT
#  03/02/2022    Added land owner information and prevented       JT
#                Riparian Forest Planting credit under
#                transmission lines
#===============================  Environment Setup  ===========================
#==========================================================================================

library(shiny)
library(leaflet)
library(sf)
library(sp)
library(dplyr)
library(DT)
library(geojsonsf)
library(rmapshaper)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(Cairo)
library(tidyverse)
library(leaflet.extras)
library(tinytex)
library(rmarkdown)
library(webshot)
library(waiter)

# load data
load("stre_simp.RData")
load("rip_simp.RData")
load("shore_simp.RData")
load("retrobmps.RData")
load("retrocoordinates.RData")
load("ftw_simp.RData")
load("ftw_owners.RData")
load("retro_owners.RData")
load("rip_owners.RData")
load("shore_owners.RData")
load("stream_owners.RData")

# quick modifications
stre_simp <- merge(stre_simp,streamsegs,all.x=TRUE,by="SiteID")
stre_simp$OwnerNames <- ifelse(stre_simp$OwnerNames=="NA","UNKNOWN", stre_simp$OwnerNames)
rip_simp <- merge(rip_simp,ripsegs,all.x=TRUE,by.x="SiteID.rip",by.y="SiteID")
rip_simp$OwnerNames <- ifelse(rip_simp$OwnerNames=="NA","UNKNOWN", rip_simp$OwnerNames)
shore_simp <- merge(shore_simp,shoresegs,all.x=TRUE,by.x="SiteID.shore",by.y="SiteID")
shore_simp$OwnerNames <- ifelse(shore_simp$OwnerNames=="NA","UNKNOWN", shore_simp$OwnerNames)
retrobmps <- merge(retrobmps,restrosegs,all.x=TRUE,by.x="SiteID.retro",by.y="SiteID")
retrobmps$OwnerNames <- ifelse(retrobmps$OwnerNames=="NA","UNKNOWN", retrobmps$OwnerNames)
ftw_simp <- merge(ftw_simp,ftwsegs,all.x=TRUE,by.x="SiteID.ftw",by.y="SiteID")
ftw_simp$OwnerNames <- ifelse(ftw_simp$OwnerNames=="NA","UNKNOWN", ftw_simp$OwnerNames)

# user interface
ui = fluidPage(
  tags$head(HTML("<title>BWPR Restoration Estimator Tool</title> <link rel='icon' type='image/gif/png' href='logo.png'>")),
  titlePanel(
    fluidRow(
      column(9,h4(strong("Restoration Estimator Tool"),style="font-size:24px;font-style:normal; font-weight: 400")),
      column(9,p("BWPR's restoration estimator tool was developed to assist NGOs and others to explore restoration opportunities, and estimate potential impervious restoration credit and TMDL reductions resulting from project implementation.  Please select the relevant tab to click on features and estimate the credit and reductions.
    ",style="font-size:18px;font-style:normal; font-weight: 400")),column(3, tags$a(img(src='bwpr_logo_aarivers.jpg', align = "right",height = 279*0.4, width = 558*0.4, style="padding: 0px"),href="https://www.aarivers.org")),
      br(),
      column(12,p("Note: this tool is provided 'as is' without warranty of any kind, either expressed, implied, or statutory. The user assumes the entire risk as to quality and performance of the data from this tool.
                 ",style="font-size:11.5px;font-style:italic")),br(),

      column(3,a(actionButton(inputId = "email1", label = "  Contact",icon = icon("envelope", lib = "font-awesome")),href="mailto:pwthom19@aacounty.org"))
    )),
  br(),
  tabsetPanel(id = "tabs",

              #===================================================
              # Stream BMPs
              #===================================================
              tabPanel("Summary", value = "1", fluid = TRUE,icon=icon("info"),
                       fluidPage(
                         fluidRow(
                           br(),
                           column(6,wellPanel(style = "background-color: #cdf7d4;",
                                              h4(strong("Guidance")),
                                              tags$div(
                                                tags$p("The potential credit from different restoration measures were modeled across the County. Each of these practices has its own tab. Selecting a tab will load a map and table. To estimate potential credit for an area, the user can zoom in and click to select features."),
                                                tags$p("Features can be selected by clicking directly on the feature or by selecting the Site ID from the dropdown box below the map. Selected features will turn red and be added to a summary table below the map. To deselect features, simply re-click or delete Site ID from the dropdown box."),
                                                tags$p("The user can progressively work through the tabs to select potential restoration practices in their project area. The total credit from all BMPs will be displayed on this page to the right, together with a graph that breaks down the credit by practice. This tool provides estimates to assist with site selection, but field assessments must be conducted for official crediting.  To reset all selections, click the 'Reset all selections' button to the right.")
                                              ),
                                              h4(strong("Stream Restoration")),
                                              tags$div(
                                                tags$p("The stream restoration tab contains modeled credit available for ~200-ft segments of non-tidal streams across the County. The model uses LiDAR data collected in 2017 and 2020 and computes the volume changes in a 40-ft buffer around the stream network."),
                                                tags$p("TSS was calculated by converting the annual volume loss by a bulk density value of 87.5 lb per cu ft. Restoration efficiency was assumed to be 50%, and therefore, the amount of TSS credit was halved. TN and TP credit were calculated by multiplying the TSS credit, at 50% efficiency, by default rates outlined in the Chesapeake Bay Expert Panel guidance. These values were 2.28 and 1.05 lbs per ton for TN and TP, respectively."),
                                                tags$p("Only projects with Equivalent Impervious Area (EIA) treatment above 0.02 ac per linear foot for each 200-ft segment are displayed, as these are likely to be in need of restoration. Credit estimates only use Protocol 1. Projects with Protocol 2, 3, and 4 will likely have a higher total credit.")
                                              ),
                                              h4(strong("Riparian Planting")),
                                              tags$div(
                                                tags$p("The riparian planting tab contains modeled credit for riparian areas within 100-ft of a perennial stream. Areas within the 100-ft buffer were selected if they were either owned by Anne Arundel County or its affiliates, within 100-year FEMA floodplain, or already under an Anne Arundel County drainage easement. Finally, 2020 County land use and 2007 Tree Canopy data were then used to remove forested areas, and the road edges feature class was use to remove road areas."),
                                                tags$p("This tab displays the credit for both Riparian Forest Buffers and Riparian Conservation Landscaping. Within the tab there is a radio button to select the preferred land cover BMP.  This controls which land cover BMPs will be totaled below the map and on the summary page.")
                                              ),
                                              h4(strong("Shoreline Restoration")),
                                              tags$div(
                                                tags$p("The shoreline restoration tab contains modeled credit available for ~500-ft segments. The model uses the mean 'Recent Erosion Rate' from the DNR Coastal Atlas, the segment length, and the median elevation within a 30-ft buffer to compute a volume loss."),
                                                tags$p("TSS was calculated by converting the annual volume loss to a mass using a default bulk density value of 93.6 lb per cu-ft, and applying a sand reduction factor of 0.551. TN and TP credit were calculated by multiplying the mass of TSS by default rates outlined in the Chesapeake Bay Expert Panel guidance. These values were 0.000290 and 0.000205 lb per lb for TN and TP, respectively."),
                                                tags$p("Only projects with Equivalent Impervious Area (EIA) treatment above 0.04 ac per linear foot for each 500-ft segment are displayed, as these are likely to be in need of restoration. Credit estimates only use Protocol 1. Projects with Protocol 2, 3, and 4 will likely have a higher total credit.")
                                              ),
                                              h4(strong("Upland Retrofits")),
                                              tags$div(
                                                tags$p("The upland retrofits tab contains modeled credit from structural BMPs such as ponds, wetlands, and filtering systems. BMPs were selected where the current rainfall depth treated (Pe) was 0.5 inches or less, the date of construction was prior to 2015, and the drainage area was greater than 1 acre. The maximum retrofit opportunity was assumed to be 3 inches of rainfall depth treatment. The existing treatment credit was subtracted from potential retrofit credit."),
                                              ),
                                              h4(strong("Floating Treatment Wetlands")),
                                              tags$div(
                                                tags$p("The floating treatment wetland tab contains modeled credit from the implementation of floating treatment wetlands. Non-tidal ponds were identified by selecting 'Open Water' polygons in the 2020 County Land Cover dataset. Ponds were retained if they were associated with County BMP records classified as being a wet pond type BMP.  This tab allows the user to select the percentage of the pond wet surface area that will be covered by a floating treatment wetland using radio buttons, with the different levels of coverage affecting credited treatment and pollutant reductions."),
                                              )


                           ) #wellPanel
                           ), #column
                           column(6, wellPanel(style = "background-color: #e6f7ff;",fluidRow(actionButton("refresh", "Reset all selections"),downloadButton("downloadReport", label = "Download Report")#, textInput("projectName", label = "", value = "Enter project name...", width = '45%')
                           ),br(), h4(strong("Credit by BMP Type")), p("Once data are selected, the chart below displays the relative proportions of impervious acres restored, and the total nitrogen, total phosphorus, and total suspended solid reductions by BMP type.",style="font-size:14px;font-style:normal; font-weight: 400")
                           ,br(),plotOutput("plotpie"), br(), h4(strong("Summary Credit Table")),br(),br(),DT::dataTableOutput("mytable.total")%>% withSpinner(color="#e6f7ff")))# column
                         )#fluidrow

                       )),
              tabPanel("Stream Restoration", value = "2", fluid = TRUE,icon=icon("water"),
                       fluidPage(br(),
                                 leafletOutput("map")%>% withSpinner(color="#cdf7d4"),
                                 selectizeInput(inputId = "selected_locations",
                                                label = "Stream Segment ID",
                                                choices = stre_simp$SiteID,
                                                selected = NULL,
                                                multiple = TRUE),
                                 DT::dataTableOutput("mytable")%>% withSpinner(color="#e6f7ff")
                       )),
              tabPanel("Riparian Planting", value = "3", fluid = TRUE,icon=icon("pagelines"),
                       fluidPage(br(),
                                 leafletOutput("maprip")%>% withSpinner(color="#cdf7d4"),br(),
                                 fluidRow(column(5,selectizeInput(inputId = "selected.rip_locations",
                                                                  label = "Riparian Segment ID",
                                                                  choices = rip_simp$SiteID,
                                                                  selected = NULL,
                                                                  multiple = TRUE)),
                                          column(5, radioButtons(
                                            "CL", label="Land Cover Type",
                                            choiceNames=c("Riparian Forest Buffer","Riparian Conservation Landscaping"),
                                            choiceValues=c(0,1),
                                            selected= 0,
                                            inline=TRUE
                                          ))),
                                 DT::dataTableOutput("mytable.rip")%>% withSpinner(color="#e6f7ff")
                       )),
              tabPanel("Shoreline Restoration", value = "4", fluid = TRUE,icon=icon("ship"),
                       fluidPage(br(),
                                 leafletOutput("mapshore")%>% withSpinner(color="#cdf7d4"),
                                 selectizeInput(inputId = "selected.shore_locations",
                                                label = "Shoreline Segment ID",
                                                choices = shore_simp$SiteID,
                                                selected = NULL,
                                                multiple = TRUE),
                                 DT::dataTableOutput("mytable.shore")%>% withSpinner(color="#e6f7ff")
                       )),
              tabPanel("Upland Retrofits", value = "5", fluid = TRUE,icon=icon("tint"),
                       fluidPage(br(),
                                 leafletOutput("mapretro")%>% withSpinner(color="#e6f7ff"),br(),
                                 selectizeInput(inputId = "selected.retro_locations",
                                                label = "Upland Retrofit ID",
                                                choices = shore_simp$SiteID,
                                                selected = NULL,
                                                multiple = TRUE),
                                 DT::dataTableOutput("mytable.retro")%>% withSpinner(color="#e6f7ff")
                       )),
              tabPanel("Floating Treatment Wetlands", value = "7", fluid = TRUE,icon=icon("frog"),
                       fluidPage(br(),
                                 leafletOutput("mapftw")%>% withSpinner(color="#cdf7d4"),br(),
                                 fluidRow(column(5,selectizeInput(inputId = "selected.ftw_locations",
                                                                  label = "Floating Treatment Wetland Segment ID",
                                                                  choices = ftw_simp$SiteID,
                                                                  selected = NULL,
                                                                  multiple = TRUE)),
                                          column(5, radioButtons(
                                            "FTWcover", label="% of pond wet surface area covered by FTW",
                                            choiceNames=c("10%","11-20%","21-30%", "31-40%","41-50%"),
                                            choiceValues=c(0,1,2,3,4),
                                            selected= 0,
                                            inline=TRUE
                                          ))),
                                 DT::dataTableOutput("mytable.ftw")%>% withSpinner(color="#e6f7ff")
                       ))
  ))

server <- function(input, output, session){

  options(shiny.usecairo=T)

  #create empty vector to hold all the clicks
  selected_ids <- reactiveValues(ids = vector())

  observeEvent(input$refresh, { # refresh app - for restart
    session$reload()
  })

  #s tream restoration map output
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE), group = "Stamen Base Map")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
      addPolygons(data = stre_simp,
                  fillColor = "grey",
                  fillOpacity = 1,
                  color = "blue",
                  stroke = TRUE,
                  weight = 2,
                  layerId = ~SiteID,
                  group = "segs",
                  label = ~SiteID,
                  highlightOptions = highlightOptions(color = "hotpink",
                                                      opacity = 1.0,
                                                      weight = 2,
                                                      bringToFront = FALSE)) %>%
      addPolygons(data = stre_simp,
                  fillColor = "red",
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  stroke = TRUE,
                  layerId = ~SiteID_2,
                  group = ~SiteID) %>%
      hideGroup(group = stre_simp$SiteID) %>%
      addLayersControl(baseGroups = c("Stamen Base Map","Esri Imagery"),
                       options = layersControlOptions(collapsed = FALSE))




  })

  #define leaflet proxy selections
  proxy <- leafletProxy("map")

  #create empty vector to hold all the clicks
  selected <- reactiveValues(groups = vector())

  observeEvent(input$map_shape_click, {
    if(input$map_shape_click$group == "segs"){
      selected$groups <- c(selected$groups, input$map_shape_click$id)
      proxy %>% showGroup(group = input$map_shape_click$id)
    } else {
      selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
      proxy %>% hideGroup(group = input$map_shape_click$group)
    }
    updateSelectizeInput(session,
                         inputId = "selected_locations",
                         label = "Add or delete features from selection:",
                         choices = stre_simp$SiteID,
                         selected = selected$groups)
  })



  observeEvent(input$selected_locations, {
    removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
    added_via_selectInput <- setdiff(input$selected_locations, selected$groups)

    if(length(removed_via_selectInput) > 0){
      selected$groups <- input$selected_locations
      proxy %>% hideGroup(group = removed_via_selectInput)
    }

    if(length(added_via_selectInput) > 0){
      selected$groups <- input$selected_locations
      proxy %>% showGroup(group = added_via_selectInput)
    }
  }, ignoreNULL = FALSE)

  selectedLocations <- reactive({
    selectedLocations <- subset(stre_simp, SiteID %in% input$selected_locations)
    selectedLocations <- data.frame("STRE_TN" = round(sum(selectedLocations$STRE_TN),1),
                                    "STRE_TP" = round(sum(selectedLocations$STRE_TP),1),
                                    "STRE_TSS" = round(sum(selectedLocations$STRE_TSS)/2000,1),
                                    "STRE_EIA" = round(sum(selectedLocations$STRE_EIA),1))
    selectedLocations

  })

  streamtable <- reactive({subset(stre_simp, SiteID %in% input$selected_locations)%>%
      mutate(STRE_TN =   round(STRE_TN,1),
             STRE_TP =  round(STRE_TP,1),
             STRE_TSS =   round(STRE_TSS/2000,1),
             STRE_EIA =  round(STRE_EIA,1))%>%
      select(c(SiteID,TaxAccountIDs,OwnerNames,NumOwners,STRE_TN,STRE_TP,STRE_TSS,STRE_EIA))%>%
      st_drop_geometry()%>%
      remove_rownames()})



  output$mytable <- renderDataTable({
    datatable(subset(stre_simp, SiteID %in% input$selected_locations)%>%
                mutate(STRE_TN =   round(STRE_TN,1),
                       STRE_TP =  round(STRE_TP,1),
                       STRE_TSS =   round(STRE_TSS/2000,1),
                       STRE_EIA =  round(STRE_EIA,1))%>%
                select(c(SiteID,OwnerNames,NumOwners,STRE_TN,STRE_TP,STRE_TSS,STRE_EIA))%>%
                st_drop_geometry(),colnames = c('Site ID','Owners','Number of Owners','Stream Total N (lbs)', 'Stream Total P (lbs)', 'Stream TSS (tons)', 'Stream EIA (ac)'),rownames = FALSE,options = list(searching = FALSE))
  })


  #========================================================================#
  #========================================================================#
  #========================================================================#
  # riparian
  #========================================================================#
  #========================================================================#
  #========================================================================#

  #create empty vector to hold all click the riparian clicks
  selected_ids.rip <- reactiveValues(ids = vector())

  # riparian map output
  output$maprip <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE), group = "Stamen Base Map")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
      addPolygons(data = rip_simp,
                  fillColor = "grey",
                  fillOpacity = 1,
                  color = "blue",
                  stroke = TRUE,
                  weight = 2,
                  layerId = ~SiteID.rip,
                  group = "segs.rip",
                  label = ~SiteID.rip,
                  highlightOptions = highlightOptions(color = "hotpink",
                                                      opacity = 1.0,
                                                      weight = 2,
                                                      bringToFront = FALSE)) %>%
      addPolygons(data = rip_simp,
                  fillColor = "red",
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  stroke = TRUE,
                  layerId = ~SiteID_2.rip,
                  group = ~SiteID.rip) %>%
      hideGroup(group = rip_simp$SiteID.rip) %>%
      addLayersControl(baseGroups = c("Stamen Base Map","Esri Imagery"),
                       options = layersControlOptions(collapsed = FALSE))
  })

  #define leaflet proxy for riparian map
  proxyrip <- leafletProxy("maprip")

  #create empty vector to hold all the riparian clicks
  selected.rip <- reactiveValues(groups = vector())

  observeEvent(input$maprip_shape_click, {
    if(input$maprip_shape_click$group == "segs.rip"){
      selected.rip$groups <- c(selected.rip$groups, input$maprip_shape_click$id)
      proxyrip %>% showGroup(group = input$maprip_shape_click$id)
    } else {
      selected.rip$groups <- setdiff(selected.rip$groups, input$maprip_shape_click$group)
      proxyrip %>% hideGroup(group = input$maprip_shape_click$group)
    }
    updateSelectizeInput(session,
                         inputId = "selected.rip_locations",
                         label = "Add or delete features from selection:",
                         choices = rip_simp$SiteID.rip,
                         selected = selected.rip$groups)
  })



  observeEvent(input$selected.rip_locations, {
    removed_via_selectInput.rip <- setdiff(selected.rip$groups, input$selected.rip_locations)
    added_via_selectInput.rip <- setdiff(input$selected.rip_locations, selected.rip$groups)

    if(length(removed_via_selectInput.rip) > 0){
      selected.rip$groups <- input$selected.rip_locations
      proxyrip %>% hideGroup(group = removed_via_selectInput.rip)
    }

    if(length(added_via_selectInput.rip) > 0){
      selected.rip$groups <- input$selected.rip_locations
      proxyrip %>% showGroup(group = added_via_selectInput.rip)
    }
  }, ignoreNULL = FALSE)

  selected.ripLocations <- reactive({
    selected.ripLocations <- subset(rip_simp, SiteID.rip %in% input$selected.rip_locations)

    if(input$CL==0){ #Riparian Forest Buffer selection

      selected.ripLocations$LC_TN <-  ifelse(!is.na(selected.ripLocations$TransmissionLine),0,selected.ripLocations$RipAcres*14.34)
      selected.ripLocations$LC_TP <-  ifelse(!is.na(selected.ripLocations$TransmissionLine),0,selected.ripLocations$RipAcres*2.5)
      selected.ripLocations$LC_TSS <- ifelse(!is.na(selected.ripLocations$TransmissionLine),0,selected.ripLocations$RipAcres*4411)
      selected.ripLocations$LC_EIA <- ifelse(!is.na(selected.ripLocations$TransmissionLine),0,selected.ripLocations$RipAcres*1.5)

      selected.ripLocations <- data.frame("LC_TN" = round(sum(selected.ripLocations$LC_TN),1),
                                          "LC_TP" = round(sum(selected.ripLocations$LC_TP),1),
                                          "LC_TSS" = round(sum(selected.ripLocations$LC_TSS)/2000,1),
                                          "LC_EIA" = round(sum(selected.ripLocations$LC_EIA),1))

    } else if(input$CL==1){ #Riparian Conservation Landscaping selection

      selected.ripLocations$LC_TN <- selected.ripLocations$RipAcres*6.75
      selected.ripLocations$LC_TP <- selected.ripLocations$RipAcres*0.74
      selected.ripLocations$LC_TSS <- 0
      selected.ripLocations$LC_EIA <- selected.ripLocations$RipAcres*0.5

      selected.ripLocations <- data.frame("LC_TN" = round(sum(selected.ripLocations$LC_TN),1),
                                          "LC_TP" = round(sum(selected.ripLocations$LC_TP),1),
                                          "LC_TSS" = round(sum(selected.ripLocations$LC_TSS)/2000,1),
                                          "LC_EIA" = round(sum(selected.ripLocations$LC_EIA),1))

    }

    selected.ripLocations

  })

  riptable <- reactive({
    if(input$CL==0){
      subset(rip_simp, SiteID.rip %in% input$selected.rip_locations)%>%
        mutate(RipAcres = round(RipAcres,1),
               LC_TN =   ifelse(!is.na(TransmissionLine),"Transmission Line",round(RipAcres*14.34,1)),
               LC_TP =  ifelse(!is.na(TransmissionLine), "Transmission Line",round(RipAcres*2.5,1)),
               LC_TSS =   ifelse(!is.na(TransmissionLine), "Transmission Line",round((RipAcres*4411)/2000,1)),
               LC_EIA =  ifelse(!is.na(TransmissionLine), "Transmission Line",round(RipAcres*1.5,1)),
               Type = gsub("([a-z])([A-Z])","\\1 \\2",
                           gsub("([A-Z])([0-9])","\\1 \\2",
                                gsub("([a-z])([0-9])","\\1 \\2",
                                     gsub("([0-9])([A-Z])","\\1 \\2",Type)))))%>%
        select(c(SiteID.rip,Type,RipAcres,TaxAccountIDs,OwnerNames,NumOwners,LC_TN,LC_TP,LC_TSS,LC_EIA))%>%
        st_drop_geometry()%>%
        remove_rownames()
    } else if(input$CL==1) {
      subset(rip_simp, SiteID.rip %in% input$selected.rip_locations)%>%
        mutate(LC_TSS  = 0,
               RipAcres = round(RipAcres,1),
               LC_TN =  round(RipAcres*6.75,1),
               LC_TP = round(RipAcres*0.74,1),
               LC_EIA =round(RipAcres*0.5,1),
               Type = gsub("([a-z])([A-Z])","\\1 \\2",
                           gsub("([A-Z])([0-9])","\\1 \\2",
                                gsub("([a-z])([0-9])","\\1 \\2",
                                     gsub("([0-9])([A-Z])","\\1 \\2",Type)))))%>%
        select(c(SiteID.rip,Type,RipAcres,TaxAccountIDs,OwnerNames,NumOwners,LC_TN,LC_TP,LC_TSS, LC_EIA))%>%
        st_drop_geometry()%>%
        remove_rownames()
    }
  })

  output$mytable.rip <- renderDataTable({
    datatable(if(input$CL==0){
      subset(rip_simp, SiteID.rip %in% input$selected.rip_locations)%>%
        mutate(RipAcres = round(RipAcres,1),
               LC_TN =   ifelse(!is.na(TransmissionLine), "Transmission Line",round(RipAcres*14.34,1)),
               LC_TP =  ifelse(!is.na(TransmissionLine), "Transmission Line",round(RipAcres*2.5,1)),
               LC_TSS =   ifelse(!is.na(TransmissionLine), "Transmission Line",round((RipAcres*4411)/2000,1)),
               LC_EIA =  ifelse(!is.na(TransmissionLine), "Transmission Line",round(RipAcres*1.5,1)),
               Type = gsub("([a-z])([A-Z])","\\1 \\2",
                           gsub("([A-Z])([0-9])","\\1 \\2",
                                gsub("([a-z])([0-9])","\\1 \\2",
                                     gsub("([0-9])([A-Z])","\\1 \\2",Type)))))%>%
        select(c(SiteID.rip,Type,RipAcres,OwnerNames,NumOwners,LC_TN,LC_TP,LC_TSS,LC_EIA))%>%
        st_drop_geometry()
    } else if(input$CL==1) {
      subset(rip_simp, SiteID.rip %in% input$selected.rip_locations)%>%
        mutate(LC_TSS  = 0,
               RipAcres = round(RipAcres,1),
               LC_TN =  round(RipAcres*6.75,1),
               LC_TP = round(RipAcres*0.74,1),
               LC_EIA =round(RipAcres*0.5,1),
               Type = gsub("([a-z])([A-Z])","\\1 \\2",
                           gsub("([A-Z])([0-9])","\\1 \\2",
                                gsub("([a-z])([0-9])","\\1 \\2",
                                     gsub("([0-9])([A-Z])","\\1 \\2",Type)))))%>%
        select(c(SiteID.rip,Type,RipAcres,OwnerNames,NumOwners,LC_TN,LC_TP,LC_TSS, LC_EIA))%>%
        st_drop_geometry()
    },colnames = c('Site ID','Land Type','Acres', 'Owners','Number of Owners','Riparian Planting Total N (lbs)', 'Riparian Planting Total P (lbs)','Riparian Planting TSS (tons)', 'Riparian Planting EIA (ac)'),
    rownames = FALSE,options = list(searching = FALSE))
  })

  #========================================================================#
  #========================================================================#
  #========================================================================#
  # shoreline
  #========================================================================#
  #========================================================================#
  #========================================================================#

  #create empty vector to hold all the clicks
  selected_ids.shore <- reactiveValues(ids = vector())

  # shoreline map output
  output$mapshore <- renderLeaflet({
    leaflet() %>%

      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE), group = "Stamen Base Map")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
      addPolygons(data = shore_simp,
                  fillColor = "grey",
                  fillOpacity = 1,
                  color = "blue",
                  stroke = TRUE,
                  weight = 2,
                  layerId = ~SiteID.shore,
                  group = "segs.shore",
                  label = ~SiteID.shore,
                  highlightOptions = highlightOptions(color = "hotpink",
                                                      opacity = 1.0,
                                                      weight = 2,
                                                      bringToFront = FALSE)) %>%
      addPolygons(data = shore_simp,
                  fillColor = "red",
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  stroke = TRUE,
                  layerId = ~SiteID_2.shore,
                  group = ~SiteID.shore) %>%
      hideGroup(group = shore_simp$SiteID.shore) %>%
      addLayersControl(baseGroups = c("Stamen Base Map","Esri Imagery"),
                       options = layersControlOptions(collapsed = FALSE))
  })

  #define leaflet proxy for mapshore
  proxyshore <- leafletProxy("mapshore")

  #create empty vector to hold all the clicks
  selected.shore <- reactiveValues(groups = vector())

  observeEvent(input$mapshore_shape_click, {
    if(input$mapshore_shape_click$group == "segs.shore"){
      selected.shore$groups <- c(selected.shore$groups, input$mapshore_shape_click$id)
      proxyshore %>% showGroup(group = input$mapshore_shape_click$id)
    } else {
      selected.shore$groups <- setdiff(selected.shore$groups, input$mapshore_shape_click$group)
      proxyshore %>% hideGroup(group = input$mapshore_shape_click$group)
    }
    updateSelectizeInput(session,
                         inputId = "selected.shore_locations",
                         label = "Add or delete features from selection:",
                         choices = shore_simp$SiteID.shore,
                         selected = selected.shore$groups)
  })



  observeEvent(input$selected.shore_locations, {
    removed_via_selectInput.shore <- setdiff(selected.shore$groups, input$selected.shore_locations)
    added_via_selectInput.shore <- setdiff(input$selected.shore_locations, selected.shore$groups)

    if(length(removed_via_selectInput.shore) > 0){
      selected.shore$groups <- input$selected.shore_locations
      proxyshore %>% hideGroup(group = removed_via_selectInput.shore)
    }

    if(length(added_via_selectInput.shore) > 0){
      selected.shore$groups <- input$selected.shore_locations
      proxyshore %>% showGroup(group = added_via_selectInput.shore)
    }
  }, ignoreNULL = FALSE)

  selected.shoreLocations <- reactive({
    selected.shoreLocations <- subset(shore_simp, SiteID.shore %in% input$selected.shore_locations)

    selected.shoreLocations <- data.frame("SHST_TN" = round(sum(selected.shoreLocations$SHST_TN),1),
                                          "SHST_TP" = round(sum(selected.shoreLocations$SHST_TP),1),
                                          "SHST_TSS" = round(sum(selected.shoreLocations$SHST_TSS)/2000,1),
                                          "SHST_EIA" = round(sum(selected.shoreLocations$SHST_EIA),1))
    selected.shoreLocations

  })

  shoretable <- reactive({subset(shore_simp, SiteID.shore %in% input$selected.shore_locations)%>%
      mutate(SHST_TN =   round(SHST_TN,1),
             SHST_TP =  round(SHST_TP,1),
             SHST_TSS =   round(SHST_TSS/2000,1),
             SHST_EIA =  round(SHST_EIA,1))%>%
      select(c(SiteID.shore,TaxAccountIDs,OwnerNames,NumOwners,SHST_TN,SHST_TP,SHST_TSS,SHST_EIA))%>%
      st_drop_geometry()%>%
      remove_rownames()})

  output$mytable.shore <- renderDataTable({

    datatable(subset(shore_simp, SiteID.shore %in% input$selected.shore_locations)%>%
                mutate(SHST_TN =   round(SHST_TN,1),
                       SHST_TP =  round(SHST_TP,1),
                       SHST_TSS =   round(SHST_TSS/2000,1),
                       SHST_EIA =  round(SHST_EIA,1))%>%
                select(c(SiteID.shore,OwnerNames,NumOwners,SHST_TN,SHST_TP,SHST_TSS,SHST_EIA))%>%
                st_drop_geometry(),colnames = c('Site ID', 'Owners','Number of Owners','Shoreline Total N (lbs)', 'Shoreline Total P (lbs)', 'Shoreline TSS (tons)',
                                                'Shoreline EIA (ac)'),
              rownames = FALSE,options = list(searching = FALSE))

  })

  #========================================================================#
  #========================================================================#
  #========================================================================#
  # Upland Retrofit
  #========================================================================#
  #========================================================================#
  #========================================================================#

  #create empty vector to hold all the clicks
  selected_ids.retro <- reactiveValues(ids = vector())

  # base map
  output$mapretro <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE), group = "Stamen Base Map")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
      addCircles(data = retrobmps,
                 radius = 50,
                 lat = retrobmps$Latitude,
                 lng = retrobmps$Longitude,
                 fillColor = "grey",
                 fillOpacity = 1,
                 color = "blue",
                 weight = 2,
                 stroke = T,
                 layerId = ~SiteID.retro,
                 group = "segs.retro",
                 label = ~SiteID.retro,
                 highlightOptions = highlightOptions(color = "hotpink",
                                                     opacity = 1.0,
                                                     weight = 2,
                                                     bringToFront = FALSE))%>%
      addCircles(data = retrobmps,
                 radius = 50,
                 lat = ~Latitude,
                 lng = ~Longitude,
                 fillColor = "red",
                 fillOpacity = 1,
                 color = "black",
                 weight = 1,
                 stroke = T,
                 group = ~SiteID.retro,
                 layerId = ~secondSiteID.retro,
                 label = ~SiteID.retro,
                 highlightOptions = highlightOptions(color = "hotpink",
                                                     opacity = 1.0,
                                                     weight = 2,
                                                     bringToFront = FALSE)) %>%
      hideGroup(group = retrobmps$SiteID.retro)%>%
      addLayersControl(baseGroups = c("Stamen Base Map","Esri Imagery"),
                       options = layersControlOptions(collapsed = FALSE))
  })

  #define leaflet proxy for mapretro
  proxyretro <- leafletProxy("mapretro")

  #create empty vector to hold all the clicks
  selected.retro <- reactiveValues(groups = vector())

  observeEvent(input$mapretro_shape_click, {
    if(input$mapretro_shape_click$group == "segs.retro"){
      selected.retro$groups <- c(selected.retro$groups, input$mapretro_shape_click$id)
      proxyretro %>% showGroup(group = input$mapretro_shape_click$id)
    } else {
      selected.retro$groups <- setdiff(selected.retro$groups, input$mapretro_shape_click$group)
      proxyretro %>% hideGroup(group = input$mapretro_shape_click$group)
    }
    updateSelectizeInput(session,
                         inputId = "selected.retro_locations",
                         label = "Add or delete features from selection:",
                         choices = retrobmps$SiteID.retro,
                         selected = selected.retro$groups)
  })



  observeEvent(input$selected.retro_locations, {
    removed_via_selectInput.retro <- setdiff(selected.retro$groups, input$selected.retro_locations)
    added_via_selectInput.retro <- setdiff(input$selected.retro_locations, selected.retro$groups)

    if(length(removed_via_selectInput.retro) > 0){
      selected.retro$groups <- input$selected.retro_locations
      proxyretro %>% hideGroup(group = removed_via_selectInput.retro)
    }

    if(length(added_via_selectInput.retro) > 0){
      selected.retro$groups <- input$selected.retro_locations
      proxyretro %>% showGroup(group = added_via_selectInput.retro)
    }
  }, ignoreNULL = FALSE)


  selectedLocations.retro <- reactive({
    selectedLocations.retro <- subset(retrobmps, SiteID.retro %in% input$selected.retro_locations)%>%
      mutate(ImperviousPercent = round((IMP_ACRES/BMP_DRAIN_AREA)*100,1),
             BMP_DRAIN_AREA = round(BMP_DRAIN_AREA,1),
             RETRO_EIA=round(RETRO_EIA,1),
             RETRO_TN=round(RETRO_TN,1),
             RETRO_TP=round(RETRO_TP,1),
             RETRO_TSS = round(RETRO_TSS/2000,1)) %>%
      rename(SiteID=SiteID.retro,
             ImperviousRestoration=RETRO_EIA,
             TNReduction=RETRO_TN,
             TPReduction=RETRO_TP,
             TSSReduction=RETRO_TSS,
             BMPDrainageArea=BMP_DRAIN_AREA,
             BMPType=BMP_TYPE) %>%
      select(-c(IMP_ACRES, PE_ADR,GEN_COMMEN,NUM_BMPS,secondSiteID.retro,Longitude,Latitude))%>%
      select(SiteID,BMPType,BMPDrainageArea,ImperviousPercent,TaxAccountIDs,OwnerNames,NumOwners,TNReduction,TPReduction,TSSReduction,ImperviousRestoration)%>%
      remove_rownames()
    selectedLocations.retro
  })



  output$mytable.retro <- renderDataTable({
    datatable(selectedLocations.retro()%>%select(-c(TaxAccountIDs)),colnames = c('Site ID', 'BMP Type', 'BMP Drainage Area (ac)', 'Impervious Area in Drainage Area (%)',
                                                                                 'Owners','Number of Owners','Total N Reduction (lbs)', 'Total P Reduction (lbs)', 'TSS Reduction (tons)', 'Impervious Credit (ac)'),rownames = FALSE)
  })


  #========================================================================#
  #========================================================================#
  #========================================================================#
  # ftw
  #========================================================================#
  #========================================================================#
  #========================================================================#

  #create empty vector to hold all the clicks
  selected_ids.ftw <- reactiveValues(ids = vector())

  #  mapftw output
  output$mapftw <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE), group = "Stamen Base Map")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
      addPolygons(data = ftw_simp,
                  fillColor = "grey",
                  fillOpacity = 1,
                  color = "blue",
                  stroke = TRUE,
                  weight = 2,
                  layerId = ~SiteID.ftw,
                  group = "segs.ftw",
                  label = ~SiteID.ftw,
                  highlightOptions = highlightOptions(color = "hotpink",
                                                      opacity = 1.0,
                                                      weight = 2,
                                                      bringToFront = FALSE)) %>%
      addPolygons(data = ftw_simp,
                  fillColor = "red",
                  fillOpacity = 1,
                  weight = 1,
                  color = "black",
                  stroke = TRUE,
                  layerId = ~SiteID_2.ftw,
                  group = ~SiteID.ftw) %>%
      hideGroup(group = ftw_simp$SiteID.ftw)%>%
      addLayersControl(baseGroups = c("Stamen Base Map","Esri Imagery"),
                       options = layersControlOptions(collapsed = FALSE))
  })

  #define leaflet proxy for mapftw
  proxyftw <- leafletProxy("mapftw")

  #create empty vector to hold all the clicks
  selected.ftw <- reactiveValues(groups = vector())

  observeEvent(input$mapftw_shape_click, {
    if(input$mapftw_shape_click$group == "segs.ftw"){
      selected.ftw$groups <- c(selected.ftw$groups, input$mapftw_shape_click$id)
      proxyftw %>% showGroup(group = input$mapftw_shape_click$id)
    } else {
      selected.ftw$groups <- setdiff(selected.ftw$groups, input$mapftw_shape_click$group)
      proxyftw %>% hideGroup(group = input$mapftw_shape_click$group)
    }
    updateSelectizeInput(session,
                         inputId = "selected.ftw_locations",
                         label = "Add or delete features from selection:",
                         choices = ftw_simp$SiteID.ftw,
                         selected = selected.ftw$groups)
  })



  observeEvent(input$selected.ftw_locations, {
    removed_via_selectInput.ftw <- setdiff(selected.ftw$groups, input$selected.ftw_locations)
    added_via_selectInput.ftw <- setdiff(input$selected.ftw_locations, selected.ftw$groups)

    if(length(removed_via_selectInput.ftw) > 0){
      selected.ftw$groups <- input$selected.ftw_locations
      proxyftw %>% hideGroup(group = removed_via_selectInput.ftw)
    }

    if(length(added_via_selectInput.ftw) > 0){
      selected.ftw$groups <- input$selected.ftw_locations
      proxyftw %>% showGroup(group = added_via_selectInput.ftw)
    }
  }, ignoreNULL = FALSE)


  selected.ftwLocations<- reactive({
    selected.ftwLocations <- subset(ftw_simp, SiteID.ftw %in% input$selected.ftw_locations)
    if(input$FTWcover == 0){
      selected.ftwLocations$FTW_TN <- round(selected.ftwLocations$ImperviousArea*0.1,1)
      selected.ftwLocations$FTW_TP <- round(selected.ftwLocations$ImperviousArea*0.02,1)
      selected.ftwLocations$FTW_TSS <- round(selected.ftwLocations$ImperviousArea*74,1)
      selected.ftwLocations$FTW_EIA <- round(selected.ftwLocations$ImperviousArea*0.008,1)
    } else if(input$FTWcover == 1) {
      selected.ftwLocations$FTW_TN <- round(selected.ftwLocations$ImperviousArea*0.22,1)
      selected.ftwLocations$FTW_TP <- round(selected.ftwLocations$ImperviousArea*0.05,1)
      selected.ftwLocations$FTW_TSS <- round(selected.ftwLocations$ImperviousArea*151,1)
      selected.ftwLocations$FTW_EIA <- round(selected.ftwLocations$ImperviousArea*0.017,1)
    } else if(input$FTWcover == 2) {
      selected.ftwLocations$FTW_TN <- round(selected.ftwLocations$ImperviousArea*0.32,1)
      selected.ftwLocations$FTW_TP <- round(selected.ftwLocations$ImperviousArea*0.07,1)
      selected.ftwLocations$FTW_TSS <- round(selected.ftwLocations$ImperviousArea*225,1)
      selected.ftwLocations$FTW_EIA <- round(selected.ftwLocations$ImperviousArea*0.026,1)
    } else if(input$FTWcover == 3) {
      selected.ftwLocations$FTW_TN <- round(selected.ftwLocations$ImperviousArea*0.43,1)
      selected.ftwLocations$FTW_TP <- round(selected.ftwLocations$ImperviousArea*0.09,1)
      selected.ftwLocations$FTW_TSS <- round(selected.ftwLocations$ImperviousArea*295,1)
      selected.ftwLocations$FTW_EIA <- round(selected.ftwLocations$ImperviousArea*0.034,1)
    } else if(input$FTWcover == 4) {
      selected.ftwLocations$FTW_TN <- round(selected.ftwLocations$ImperviousArea*0.53,1)
      selected.ftwLocations$FTW_TP <- round(selected.ftwLocations$ImperviousArea*0.11,1)
      selected.ftwLocations$FTW_TSS <- round(selected.ftwLocations$ImperviousArea*369,1)
      selected.ftwLocations$FTW_EIA <- round(selected.ftwLocations$ImperviousArea*0.042,1)
    }

    selected.ftwLocations <- data.frame("FTW_TN" = round(sum(selected.ftwLocations$FTW_TN),1),
                                        "FTW_TP" = round(sum(selected.ftwLocations$FTW_TP),1),
                                        "FTW_TSS" = round(sum(selected.ftwLocations$FTW_TSS)/2000,1),
                                        "FTW_EIA" = round(sum(selected.ftwLocations$FTW_EIA),1))
    selected.ftwLocations


  })

  ftwtable <- reactive({selected.ftwLocations_tbl <- subset(ftw_simp, SiteID.ftw %in% input$selected.ftw_locations)
  if(input$FTWcover == 0){
    selected.ftwLocations_tbl$FTW_TN <- round(selected.ftwLocations_tbl$ImperviousArea*0.1,1)
    selected.ftwLocations_tbl$FTW_TP <- round(selected.ftwLocations_tbl$ImperviousArea*0.02,1)
    selected.ftwLocations_tbl$FTW_TSS <- round((selected.ftwLocations_tbl$ImperviousArea*74)/2000,1)
    selected.ftwLocations_tbl$FTW_EIA <- round(selected.ftwLocations_tbl$ImperviousArea*0.008,1)
  } else if(input$FTWcover == 1) {
    selected.ftwLocations_tbl$FTW_TN <- round(selected.ftwLocations_tbl$ImperviousArea*0.22,1)
    selected.ftwLocations_tbl$FTW_TP <- round(selected.ftwLocations_tbl$ImperviousArea*0.05,1)
    selected.ftwLocations_tbl$FTW_TSS <- round((selected.ftwLocations_tbl$ImperviousArea*151)/2000,1)
    selected.ftwLocations_tbl$FTW_EIA <- round(selected.ftwLocations_tbl$ImperviousArea*0.017,1)
  } else if(input$FTWcover == 2) {
    selected.ftwLocations_tbl$FTW_TN <- round(selected.ftwLocations_tbl$ImperviousArea*0.32,1)
    selected.ftwLocations_tbl$FTW_TP <- round(selected.ftwLocations_tbl$ImperviousArea*0.07,1)
    selected.ftwLocations_tbl$FTW_TSS <- round((selected.ftwLocations_tbl$ImperviousArea*225)/2000,1)
    selected.ftwLocations_tbl$FTW_EIA <- round(selected.ftwLocations_tbl$ImperviousArea*0.026,1)
  } else if(input$FTWcover == 3) {
    selected.ftwLocations_tbl$FTW_TN <- round(selected.ftwLocations_tbl$ImperviousArea*0.43,1)
    selected.ftwLocations_tbl$FTW_TP <- round(selected.ftwLocations_tbl$ImperviousArea*0.09,1)
    selected.ftwLocations_tbl$FTW_TSS <- round((selected.ftwLocations_tbl$ImperviousArea*295)/2000,1)
    selected.ftwLocations_tbl$FTW_EIA <- round(selected.ftwLocations_tbl$ImperviousArea*0.034,1)
  } else if(input$FTWcover == 4) {
    selected.ftwLocations_tbl$FTW_TN <- round(selected.ftwLocations_tbl$ImperviousArea*0.53,1)
    selected.ftwLocations_tbl$FTW_TP <- round(selected.ftwLocations_tbl$ImperviousArea*0.11,1)
    selected.ftwLocations_tbl$FTW_TSS <- round((selected.ftwLocations_tbl$ImperviousArea*369)/2000,1)
    selected.ftwLocations_tbl$FTW_EIA <- round(selected.ftwLocations_tbl$ImperviousArea*0.042,1)
  }
  selected.ftwLocations_tbl <- selected.ftwLocations_tbl %>% select(c(SiteID.ftw,PondSurfaceArea,TaxAccountIDs,OwnerNames,NumOwners,FTW_TN,FTW_TP,FTW_TSS,FTW_EIA,geometry))%>%
    st_drop_geometry()%>%
    remove_rownames()
  selected.ftwLocations_tbl})

  output$mytable.ftw <- renderDataTable({
    selected.ftwLocations_tbl <- subset(ftw_simp, SiteID.ftw %in% input$selected.ftw_locations)
    if(input$FTWcover == 0){
      selected.ftwLocations_tbl$FTW_TN <- round(selected.ftwLocations_tbl$ImperviousArea*0.1,1)
      selected.ftwLocations_tbl$FTW_TP <- round(selected.ftwLocations_tbl$ImperviousArea*0.02,1)
      selected.ftwLocations_tbl$FTW_TSS <- round((selected.ftwLocations_tbl$ImperviousArea*74)/2000,1)
      selected.ftwLocations_tbl$FTW_EIA <- round(selected.ftwLocations_tbl$ImperviousArea*0.008,1)
    } else if(input$FTWcover == 1) {
      selected.ftwLocations_tbl$FTW_TN <- round(selected.ftwLocations_tbl$ImperviousArea*0.22,1)
      selected.ftwLocations_tbl$FTW_TP <- round(selected.ftwLocations_tbl$ImperviousArea*0.05,1)
      selected.ftwLocations_tbl$FTW_TSS <- round((selected.ftwLocations_tbl$ImperviousArea*151)/2000,1)
      selected.ftwLocations_tbl$FTW_EIA <- round(selected.ftwLocations_tbl$ImperviousArea*0.017,1)
    } else if(input$FTWcover == 2) {
      selected.ftwLocations_tbl$FTW_TN <- round(selected.ftwLocations_tbl$ImperviousArea*0.32,1)
      selected.ftwLocations_tbl$FTW_TP <- round(selected.ftwLocations_tbl$ImperviousArea*0.07,1)
      selected.ftwLocations_tbl$FTW_TSS <- round((selected.ftwLocations_tbl$ImperviousArea*225)/2000,1)
      selected.ftwLocations_tbl$FTW_EIA <- round(selected.ftwLocations_tbl$ImperviousArea*0.026,1)
    } else if(input$FTWcover == 3) {
      selected.ftwLocations_tbl$FTW_TN <- round(selected.ftwLocations_tbl$ImperviousArea*0.43,1)
      selected.ftwLocations_tbl$FTW_TP <- round(selected.ftwLocations_tbl$ImperviousArea*0.09,1)
      selected.ftwLocations_tbl$FTW_TSS <- round((selected.ftwLocations_tbl$ImperviousArea*295)/2000,1)
      selected.ftwLocations_tbl$FTW_EIA <- round(selected.ftwLocations_tbl$ImperviousArea*0.034,1)
    } else if(input$FTWcover == 4) {
      selected.ftwLocations_tbl$FTW_TN <- round(selected.ftwLocations_tbl$ImperviousArea*0.53,1)
      selected.ftwLocations_tbl$FTW_TP <- round(selected.ftwLocations_tbl$ImperviousArea*0.11,1)
      selected.ftwLocations_tbl$FTW_TSS <- round((selected.ftwLocations_tbl$ImperviousArea*369)/2000,1)
      selected.ftwLocations_tbl$FTW_EIA <- round(selected.ftwLocations_tbl$ImperviousArea*0.042,1)
    }
    selected.ftwLocations_tbl <- selected.ftwLocations_tbl %>% select(c(SiteID.ftw,PondSurfaceArea,OwnerNames,NumOwners,FTW_TN,FTW_TP,FTW_TSS,FTW_EIA,geometry))%>%
      st_drop_geometry()
    selected.ftwLocations_tbl
    datatable(selected.ftwLocations_tbl,colnames = c('Site ID','Pond Surface Area (ac)', 'Owners','Number of Owners','FTW Total N (lbs)', 'FTW Total P (lbs)', 'FTW TSS (tons)', 'FTW EIA (ac)'),
              rownames = FALSE,options = list(searching = FALSE))
  })


  #========================================================================#
  #========================================================================#
  #========================================================================#
  # Summary
  #========================================================================#
  #========================================================================#
  #========================================================================#

  selected.all <- reactive({
    selectedLocations <- selectedLocations()
    selected.ripLocations <- selected.ripLocations()
    selected.shoreLocations <-selected.shoreLocations()
    selectedLocations.retro<-selectedLocations.retro()
    selected.ftwLocations<-selected.ftwLocations()

    selected.all <- data.frame("TN" = sum(selected.ftwLocations$FTW_TN,selectedLocations.retro$TNReduction, selected.ripLocations$LC_TN,  selectedLocations$STRE_TN,selected.shoreLocations$SHST_TN,na.rm = T),
                               "TP" = sum(selected.ftwLocations$FTW_TP,selectedLocations.retro$TPReduction, selected.ripLocations$LC_TP,  selectedLocations$STRE_TP,selected.shoreLocations$SHST_TP,na.rm = T),
                               "TSS" = sum(selected.ftwLocations$FTW_TSS,selectedLocations.retro$TSSReduction, selected.ripLocations$LC_TSS, selectedLocations$STRE_TSS,selected.shoreLocations$SHST_TSS,na.rm = T),
                               "EIA" = sum(selected.ftwLocations$FTW_EIA,selectedLocations.retro$ImperviousRestoration, selected.ripLocations$LC_EIA, selectedLocations$STRE_EIA,selected.shoreLocations$SHST_EIA,na.rm = T))

    selected.all
  })

  output$mytable.total <- renderDataTable({
    all(is.na(selected.all()$pct))
    validate(
      need(selected.all()$EIA >0, "Select data to create table")
    )
    datatable(selected.all(),colnames = c('Sum Total N (lbs)', 'Sum Total P (lbs)', 'Sum TSS (tons)', 'Sum EIA (ac)'),rownames = FALSE,options = list(searching = FALSE))
  })

  selectedallPlot <- reactive({
    selectedLocations <- selectedLocations()
    selected.ripLocations <- selected.ripLocations()
    selected.shoreLocations <-selected.shoreLocations()
    selected.retroLocations <- selectedLocations.retro()
    selected.ftwLocations<-selected.ftwLocations()
    selectedStre <- tibble("Total Nitrogen"= selectedLocations$STRE_TN, "Total Phosphorus"=selectedLocations$STRE_TP, "Total Suspended Solids"=selectedLocations$STRE_TSS, "Impervious Acres Restored"=selectedLocations$STRE_EIA, "BMPType"="Stream Restoration")
    selectedallRip <-tibble("Total Nitrogen"= selected.ripLocations$LC_TN, "Total Phosphorus"=selected.ripLocations$LC_TP, "Total Suspended Solids"=selected.ripLocations$LC_TSS, "Impervious Acres Restored"=selected.ripLocations$LC_EIA, "BMPType"="Forest Buffers")
    selectedShst <- tibble("Total Nitrogen"= selected.shoreLocations$SHST_TN, "Total Phosphorus"=selected.shoreLocations$SHST_TP, "Total Suspended Solids"=selected.shoreLocations$SHST_TSS, "Impervious Acres Restored"=selected.shoreLocations$SHST_EIA, "BMPType"="Shoreline Restoration")
    selectedRetro <- tibble("Total Nitrogen"= sum(selected.retroLocations$TNReduction,na.rm=T), "Total Phosphorus"=sum(selected.retroLocations$TPReduction,na.rm=T), "Total Suspended Solids"=sum(selected.retroLocations$TSSReduction,na.rm=T), "Impervious Acres Restored"=sum(selected.retroLocations$ImperviousRestoration,na.rm=T), "BMPType"="Upland Retrofit")
    selectedFTW <- tibble("Total Nitrogen"= sum(selected.ftwLocations$FTW_TN,na.rm=T), "Total Phosphorus"=sum(selected.ftwLocations$FTW_TP,na.rm=T), "Total Suspended Solids"=sum(selected.ftwLocations$FTW_TSS,na.rm=T), "Impervious Acres Restored"=sum(selected.ftwLocations$FTW_EIA,na.rm=T), "BMPType"="Floating Treatment Wetland")
    selectedallPlot <- rbind(selectedStre,rbind(selectedallRip,rbind(selectedShst,rbind(selectedRetro,selectedFTW)))) %>%
      pivot_longer(!BMPType, names_to = "Parameter", values_to = "Unit") %>%
      group_by(Parameter) %>%
      mutate(pct = Unit/sum(Unit)*100)
    selectedallPlot
  })

  sumplot <- reactive({
    ggplot(selectedallPlot(), aes(x = 1, y = pct, fill = BMPType)) +
      geom_col(color = "black") +
      facet_wrap(.~Parameter,ncol=2)+
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Paired") + guides(fill=guide_legend("")) +
      theme(strip.text = element_text(colour = 'white',face="bold",size = 12*0.87),
            panel.border=element_rect(colour="black",size=1,fill=NA),
            panel.background = element_blank(),
            strip.background = element_rect(fill="black"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill=NA, color=NA, size=0),
            legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
            legend.spacing.x = unit(0, "pt"),
            legend.margin=margin(t=-0.5,l=0.05,b=0.05,r=0.05, unit='cm'),
            legend.key.size = unit(1, 'cm'),
            legend.text=element_text(size=12*0.87))
  })




  output$plotpie <-renderPlot({
    all(is.na(selectedallPlot()$pct))
    validate(
      need(all(is.na(selectedallPlot()$pct)) == FALSE, "Select data to create plot.")
    )
    ggplot(selectedallPlot(), aes(x = 1, y = pct, fill = BMPType)) +
      geom_col(color = "black") +
      facet_wrap(.~Parameter,ncol=2)+
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Paired") + guides(fill=guide_legend("")) +
      theme(strip.text = element_text(colour = 'white',face="bold",size = 12*0.87),
            panel.border=element_rect(colour="black",size=1,fill=NA),
            panel.background = element_blank(),
            strip.background = element_rect(fill="black"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill="#e6f7ff", color="#e6f7ff", size=0),
            legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
            legend.spacing.x = unit(0, "pt"),
            legend.margin=margin(t=-0.5,l=0.05,b=0.05,r=0.05, unit='cm'),
            legend.key.size = unit(1, 'cm'),
            legend.text=element_text(size=12*0.87))
  },  bg="#e6f7ff", execOnResize=T, height = 300*1.25, width = 450*1.25)


  output$downloadReport <- downloadHandler(
    # name pdf output here
    filename = function() {
      paste(paste0('RestorationEstimatorReport_',Sys.Date()), sep = '.', PDF = 'pdf')
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      tempimage <- file.path(tempdir(), "bwpr_logo_aarivers.jpg")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      file.copy("bwpr_logo_aarivers.jpg", tempimage, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(sumplot=sumplot(),
                     sumtable=selected.all(),
                     streamtable=streamtable(),
                     riptable=riptable(),
                     shoretable=shoretable(),
                     retrotable=selectedLocations.retro(),
                     ftwtable = ftwtable(),
                     streamselections=input$selected_locations)

      # Knit the document, passing in the `params` list
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv(),pdf_document())
      )
    }
  )
}

shinyApp(ui = ui, server = server)
