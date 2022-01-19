library(cbsodataR)
library(forecast)
source('Utils.R')
#loading meta data of dataset
main_meta <- cbs_get_meta("83648NED")

#Get all of the unique crimeTypes and add names to them for the user
uniqueMisdrijf <- unique(main_meta$SoortMisdrijf$Key)
names(uniqueMisdrijf) <- unique(gsub('[[:digit:]]+', '',as.character(main_meta$SoortMisdrijf$Title)))

#load GEOjson to create the map
gemeentegrenzen <- geojson_read("https://raw.githubusercontent.com/dijkstrar/NL-gemeentegrenzen2020/main/gemeente_grenzen_2020.json", what = "sp")

server <- function(input, output, session){
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
       "<div class='llabel'>
          <div class='llabel-header'>
            <i class='glyphicon glyphicon-map-marker llabel-icon'></i> 
            <b class='llabel-title'>%s</b><i class='glyphicon glyphicon-chevron-right llabel-icon2'></i>
          </div>
          <div class='llabel-info'><span class='llabel-data-name'>Misdrijven per 1000 inwoners</span><span class='llabel-data'>%s</span </div>
        </div>",
       gemeentegrenzen2$RegioS_label, gemeentegrenzen2$GeregistreerdeMisdrijvenPer1000Inw_3) %>% lapply(htmltools::HTML)
     #plot out the data on the map
     output$map <- renderLeaflet({
       
       leaflet(gemeentegrenzen2) %>%
         addProviderTiles("MapBox",  options = providerTileOptions(
           accessToken = Sys.getenv('sk.eyJ1IjoibG9yZW56b2tyIiwiYSI6ImNrd2NjbXYzZjBqYmoydm4yaGp0NWdjdTAifQ.MgJHvSiJFzLj3AWLRlBWNg'))) %>%  
         setView( lat=52.21441431507194, lng=5.5427232721445865 , zoom=8) %>% 
         
         addPolygons(stroke = TRUE,opacity = 1, fillOpacity = 0.5, smoothFactor = 0.5, layerId = gemeentegrenzen2$statcode,
                     color="black", fillColor = ~qpal(gemeentegrenzen2$GeregistreerdeMisdrijvenPer1000Inw_3),weight = 1,
                     highlightOptions = highlightOptions(
                       weight = 5,
                       color = "#fff",
                       fillOpacity = 0.8,
                       bringToFront = TRUE),
                     label = labels,
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "8px 10px"),
                       textsize = "16px",
                       direction = "auto")) %>%
         addLegend(values=~gemeentegrenzen2$GeregistreerdeMisdrijvenPer1000Inw_3,pal=qpal, position = "bottomright", title="Misdrijven per 1000 inwoners")
       })
     })
  
  observeEvent(input$map_shape_click, {
    updateTabsetPanel(session, "navBar",
                      selected = "gemeentePanel")
    regio <- input$map_shape_click$id
    periode <-  paste(input$selectionYear,"JJ00",sep = "")
    regio_name <- main_meta$RegioS[main_meta$RegioS$Key == regio, "Title"]
    places <- cbs_get_meta("47015NED", catalog = "Politie")$Plaatsen
    
    updateTextInput(session, "searchField", value = regio_name)
    
    output$piechart <- renderPlotly({ createPrioPieChart(regio,periode) }) 
    output$crimeRanking <- renderPlotly({ create_crime_ranking(periode,regio)})

    if(length(places[tolower(places$Title) == tolower(regio_name), "Key"] > 0))
    {
      output$predictionChart <- renderPlotly({create_prediction_chart(regio_name)})
      theft_data <- get_theft_data(regio_name)
      output$theftDate <- renderText({paste(theft_data[1], theft_data[2],sep="/")}) 
      output$theftNumber <- renderText({as.character(theft_data[3])})
    }
  })
  
  observeEvent(input$searchBtn,{
    if(input$searchField != "" && tolower(input$searchField) %in% tolower(main_meta$RegioS$Title) )
    {
      periode <- paste(input$selectionYear,"JJ00",sep = "")
      regio <- main_meta$RegioS[tolower(main_meta$RegioS$Title) == tolower(input$searchField), "Key"]
      
      city_crime_data <- get_crime_data(periode,regio)
      output$piechart <- renderPlotly({ createPrioPieChart(regio,periode) })
      pred_result <- get_theft_prediction(input$searchField)
    }
  })
}