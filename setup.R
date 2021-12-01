#Compares list of output from installed packages to list of packages and installs the new ones
list.of.packages <- c("leaflet","shiny","cbsodataR","tidyverse","sf","ggplot2","plotly","jsonlite","geojsonio","sp","rgdal","dplyr","shinyjs","treemapify","shinythemes","stringr","forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

