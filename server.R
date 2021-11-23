data = cbs_get_data("84468NED",
                    select=c("WijkenEnBuurten","TotaalDiefstal_8","Gemeentenaam_1")) %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten),
         diefstal = TotaalDiefstal_8)

data <- as.data.frame(data)

colnames(data)[which(names(data) == "WijkenEnBuurten")] <- "statcode"

gemeentegrenzen <- geojson_read("https://raw.githubusercontent.com/dijkstrar/NL-gemeentegrenzen2020/main/gemeente_grenzen_2020.json", what = "sp")

gemeentegrenzen <- merge(gemeentegrenzen,data, by="statcode")

qpal = colorBin("Reds", gemeentegrenzen$diefstal, bins=4)

labels <- sprintf(
  "<strong>%s</strong><br/>Aantal diefstallen: %s ",
  gemeentegrenzen$Gemeentenaam_1, gemeentegrenzen$diefstal) %>% lapply(htmltools::HTML)

server <- function(input, output){
  
  output$map <- renderLeaflet({
    leaflet(gemeentegrenzen) %>%
      addTiles() %>% 
      setView( lat=52.21441431507194, lng=5.5427232721445865 , zoom=8) %>% 
      addPolygons(stroke = TRUE,opacity = 1, fillOpacity = 0.5, smoothFactor = 0.5,
                  color="black", fillColor = ~qpal(gemeentegrenzen$diefstal),weight = 1,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(values=~gemeentegrenzen$diefstal,pal=qpal,title="Aantal diefstallen") 
  })
  
}