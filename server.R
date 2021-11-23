library(cbsodataR)
#load data from cbs
main_data <- cbs_get_data("83648NED",
                          Perioden = '2020JJ00',
                          RegioS = "GM1680",
                          select = c("SoortMisdrijf", "Perioden", "RegioS", "GeregistreerdeMisdrijvenPer1000Inw_3"))

main_data <- cbs_add_label_columns(main_data)
main_data <- as.data.frame(main_data)

#Get all of the unique crimeTypes and add names to them for the user
uniqueMisdrijf <- unique(main_data$SoortMisdrijf)
names(uniqueMisdrijf) <- unique(gsub('[[:digit:]]+', '',as.character(main_data$SoortMisdrijf_label)))

colnames(main_data)[which(names(main_data) == "RegioS")] <- "statcode"

#load GEOjson to create the map
gemeentegrenzen <- geojson_read("https://raw.githubusercontent.com/dijkstrar/NL-gemeentegrenzen2020/main/gemeente_grenzen_2020.json", what = "sp")

server <- function(input, output){
  toListen <- reactive({list(input$selectionYear,input$selectInput)})
  
  observeEvent(toListen(),
               {
                 #Filter data by year based on user input
                 data <- cbs_get_data("83648NED",
                                           Perioden = paste(input$selectionYear,"JJ00",sep = ""),
                                           RegioS = has_substring("GM"),
                                           SoortMisdrijf = input$selectInput,
                                           select = c("SoortMisdrijf", "Perioden", "RegioS", "GeregistreerdeMisdrijvenPer1000Inw_3"))
                 data <- cbs_add_label_columns(data)
                 colnames(data)[which(names(data) == "RegioS")] <- "statcode"
                 
                 gemeentegrenzen2 <- merge(gemeentegrenzen,data, by="statcode")
                 
                 #create bins and labels
                 qpal = colorBin("Reds", gemeentegrenzen2$GeregistreerdeMisdrijvenPer1000Inw_3, bins=4)
                 labels <- sprintf(
                   "<strong>%s</strong><br/>Misdrijven per 1000 inwoners: %s ",
                   gemeentegrenzen2$RegioS_label, gemeentegrenzen2$GeregistreerdeMisdrijvenPer1000Inw_3) %>% lapply(htmltools::HTML)
                 
                 #plot out the data on the map
                 output$map <- renderLeaflet({
                   
                   leaflet(gemeentegrenzen2) %>%
                     addProviderTiles("MapBox",  options = providerTileOptions(
                       accessToken = Sys.getenv('sk.eyJ1IjoibG9yZW56b2tyIiwiYSI6ImNrd2NjbXYzZjBqYmoydm4yaGp0NWdjdTAifQ.MgJHvSiJFzLj3AWLRlBWNg'))) %>%  
                     setView( lat=52.21441431507194, lng=5.5427232721445865 , zoom=8) %>% 
                     
                     addPolygons(stroke = TRUE,opacity = 1, fillOpacity = 0.5, smoothFactor = 0.5,
                                 color="black", fillColor = ~qpal(gemeentegrenzen2$GeregistreerdeMisdrijvenPer1000Inw_3),weight = 1,
                                 highlightOptions = highlightOptions(
                                   weight = 5,
                                   color = "#fff",
                                   fillOpacity = 0.8,
                                   bringToFront = TRUE),
                                 label = labels,
                                 labelOptions = labelOptions(
                                   style = list("font-weight" = "normal", padding = "3px 8px"),
                                   textsize = "15px",
                                   direction = "auto")) %>%
                     addLegend(values=~gemeentegrenzen2$GeregistreerdeMisdrijvenPer1000Inw_3,pal=qpal,title="Misdrijven per 1000 inwoners")
                   })
                 })
}