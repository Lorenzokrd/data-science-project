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
source('server.R')

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  theme = shinytheme("slate"),
  useShinyjs(),
  navbarPage("Crime dashboard",id = "navBar",
tabPanel("Map", value="mapPanel",
         sidebarLayout(
           sidebarPanel( width = 3,
                         sliderInput("selectionYear", "Jaar", min = 2010, max = 2020, value = 2020, sep = "",
                                     animate = animationOptions(
                                       interval = 2000,
                                       loop = FALSE,
                                       playButton = NULL,
                                       pauseButton = NULL
                                     )),
                         selectInput("selectInput","Type misdrijf", choices = uniqueMisdrijf, selected = uniqueMisdrijf[0]),
                         tags$hr(),
                         tags$b("Bron"),
                         tags$p("Alle gebruikte data komt van de elektronische databank van het Centraal Bureau voor de Statistiek (CBS)."),
                         tags$a(href="https://opendata.cbs.nl/statline/#/CBS/nl/navigatieScherm/thema", "StatLine CBS")
           ),
           mainPanel( width = 9, 
                      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                      leafletOutput("map"),
           )
         ),),
tabPanel("gemeente",value = "gemeentePanel",
           # mainPanel(
           #            width = "100%",
           #            textInput("searchField","Gemeente zoeken",placeholder = "Naam van de gemeente", width = "100%"),
           #            actionButton("searchBtn", "Zoeken"),
           #            tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
           #            
           # ),
         fluidPage(
           fluidRow(
             column(11,
                   textInput("searchField","Gemeente zoeken",placeholder = "Naam van de gemeente", width = "100%"),
                   actionButton("searchBtn", "Zoeken"),
                   tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"))),
           tags$br(),
           fluidRow(
             column(
               6,
               plotlyOutput("piechart"),

             ),
             column(
               6,
               plotlyOutput("crimeRanking"))
           ),
           tags$br(),
           fluidRow(

             column(
               6,
               plotlyOutput("biketheftchart")),
             column(
               6,
               plotlyOutput("predictionChart")),
             ),
           tags$br(),
           fluidRow(
             
             column(
               6,
               plotlyOutput("companytheftchart")),
             column(
               6,
               plotlyOutput("storerobberychart")),
           )
           ),
         
         )
),
)