data <- cbs_get_data("83648NED", Perioden = "2020JJ00", SoortMisdrijf = "T001161")
data <- cbs_add_label_columns(data)
data <- as.data.frame(data)

colnames(data)[which(names(data) == "RegioS")] <- "statcode"

gemeentegrenzen <- geojson_read("https://raw.githubusercontent.com/dijkstrar/NL-gemeentegrenzen2020/main/gemeente_grenzen_2020.json", what = "sp")

gemeentegrenzen <- merge(gemeentegrenzen,data, by="statcode")

qpal = colorBin("Reds", gemeentegrenzen$GeregistreerdeMisdrijvenPer1000Inw_3, bins=4)

labels <- sprintf(
  "<strong>%s</strong><br/>Misdrijven per 1000 inwoners: %s ",
  gemeentegrenzen$RegioS_label, gemeentegrenzen$GeregistreerdeMisdrijvenPer1000Inw_3) %>% lapply(htmltools::HTML)

server <- function(input, output){
  
  output$map <- renderLeaflet({
    leaflet(gemeentegrenzen) %>%
      addProviderTiles("MapBox",  options = providerTileOptions(
        accessToken = Sys.getenv('sk.eyJ1IjoibG9yZW56b2tyIiwiYSI6ImNrd2NjbXYzZjBqYmoydm4yaGp0NWdjdTAifQ.MgJHvSiJFzLj3AWLRlBWNg'))) %>%  
      setView( lat=52.21441431507194, lng=5.5427232721445865 , zoom=8) %>% 
      
      addPolygons(stroke = TRUE,opacity = 1, fillOpacity = 0.5, smoothFactor = 0.5,
                  color="black", fillColor = ~qpal(gemeentegrenzen$GeregistreerdeMisdrijvenPer1000Inw_3),weight = 1,
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
      addLegend(values=~gemeentegrenzen$GeregistreerdeMisdrijvenPer1000Inw_3,pal=qpal,title="Misdrijven per 1000 inwoners") 
  })
  
}