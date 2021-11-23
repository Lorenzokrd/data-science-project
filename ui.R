library(leaflet)
library(shiny)
library(cbsodataR)
library(tidyverse)
library(sf)
library(ggplot2)
library(plotly)
library(jsonlite)
library(geojsonio)
library(sp)
library(rgdal)
library(dplyr)
library(shinyjs)
library(treemapify)
library(shinythemes)
library(stringr)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  theme = shinytheme("slate"),
  useShinyjs(),
  navbarPage("Crime dashboard",
    tabPanel("Map",
      sidebarLayout(
        sidebarPanel( width = 3,
          tags$h3("Plot parameters")
        ),
        mainPanel( width = 9, 
          tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
          leafletOutput("map"),
        )
      )
    )
  )
  
  
)