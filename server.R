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
