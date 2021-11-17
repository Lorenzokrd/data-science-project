library(leaflet)
library(shiny)

ui <- fluidPage(
  
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
  
)