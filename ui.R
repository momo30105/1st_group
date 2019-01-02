library(shiny)
library(leaflet) 
library(rgdal) 
library(readr)
library(jsonlite)
library(RCurl)
library(bitops)
library(Rcpp)
library(ggplot2)
library(data.table)
library(hms)
library(pkgconfig)
library(datasets)
library(utils)
library(stats)
library(sp)
library(methods)
library(graphics)
library(grDevices)
library(XML)
library(rvest)
library(xml2)
library(maptools)
library(gstat)
library(gpclib)

vars <- c(
  "現在時間" ="0hour",
  "1小時後" = "1hour",
  "2小時後" = "2hour",
  "4小時後" = "4hour",
  "7小時後" = "7hour"
)

shinyUI(navbarPage("PM2.5 IDW",
  # headerPanel("666666666666666"),
  # titlePanel("8787"),
  # h1("Heatmap,nel("map1",
  
  #預測
  tabPanel("政府",
           # Include our custom CSS
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")),
           # draw
           leafletOutput("map",width = "100%",height = "100%")),
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = FALSE, top = "auto", left = 20, right = "auto", bottom = 150,
                         width = 200, height = 100,
                         
                         #h2("explorer"),
                         #actionButton("zoomButton", "Zoom to fit buses")
                         
                         
                         selectInput("Forecast", "未來預測數值", vars)
                         # selectInput("size", "Size", vars, selected = "adultpop"),
                         # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                         #                  # Only prompt for threshold when coloring or sizing by superzip
                         #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                         # ),
                         # 
                         # plotOutput("histCentile", height = 200),
                         # plotOutput("scatterCollegeIncome", height = 250)
           )),

   # tabPanel("政府2",
   #         # Include our custom CSS
   #         div(class="outer",
   #             tags$head(
   #               includeCSS("styles.css")),
   #         # draw
   #         leafletOutput("map_2",width = "100%",height = "100%"))),
  
  #混合1
  tabPanel("混合1",
           # Include our custom CSS
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")),
               # draw
               leafletOutput("map_mix_1",width = "100%",height = "100%"))),
  #混合2
  tabPanel("混合2",
           # Include our custom CSS
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")),
               # draw
               leafletOutput("map_mix_2",width = "100%",height = "100%"))),
  #airbox1
  tabPanel("airbox1",
           # Include our custom CSS
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")),
               # draw
               leafletOutput("map_3",width = "100%",height = "100%"))),
  #airbox2
  tabPanel("airbox2",
           # Include our custom CSS
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")),
               # draw
               leafletOutput("map_4",width = "100%",height = "100%")))
  
  )
)
