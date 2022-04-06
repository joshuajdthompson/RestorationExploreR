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
