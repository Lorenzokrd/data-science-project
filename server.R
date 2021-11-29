library(cbsodataR)
library(forecast)

#loading meta data of dataset
main_meta <- cbs_get_meta("83648NED")

#Get all of the unique crimeTypes and add names to them for the user
uniqueMisdrijf <- unique(main_meta$SoortMisdrijf$Key)
names(uniqueMisdrijf) <- unique(gsub('[[:digit:]]+', '',as.character(main_meta$SoortMisdrijf$Title)))

#load GEOjson to create the map
gemeentegrenzen <- geojson_read("https://raw.githubusercontent.com/dijkstrar/NL-gemeentegrenzen2020/main/gemeente_grenzen_2020.json", what = "sp")

get_theft_prediction <- function(cityName)
{
  places <- cbs_get_meta("47015NED", catalog = "Politie")$Plaatsen
  data <- cbs_get_data("47015NED",
                       catalog = "Politie",
                       Perioden = has_substring("2019MM") | has_substring("2020MM") | has_substring("2021MM"),
                       Plaatsen = places[tolower(places$Title) == tolower(cityName), "Key"],
                       SoortMisdrijf = has_substring("1.1.1"))
  data <- cbs_add_label_columns(data)
  endMonth <- lubridate::month(Sys.Date()) - 1
  endYear <- lubridate::year(Sys.Date())
  time_s <- ts(data$GeregistreerdeMisdrijven_1,start=c(2019, 1), end=c(endYear, endMonth), frequency=12)
  fit <- nnetar(time_s, lambda = "auto")
  fcast <- forecast(fit, PI=TRUE, h=1, model = forecast.ets)
  
  return(data.frame(pred = fcast$mean, lowerBound = fcast$lower, upperBound = fcast$upper))
}

createPrioPieChart <- function(regio,periode) {
  prio1data <- cbs_get_data("47008NED",
                            catalog = "Politie",
                            Perioden = periode,
                            RegioS = regio)
  prio1data <- cbs_add_label_columns(prio1data)
  
  pieData <- data.frame(
    category=c("0-15 minuten","15-30 minuten","30-45 minuten","45-60 minuten","60-120 minuten","Meer dan 120 minuten"),
    count=c(
      prio1data$Reactietijd0Tot15Minuten_3,
      prio1data$Reactietijd15Tot30Minuten_4,
      prio1data$Reactietijd30Tot45Minuten_5,
      prio1data$Reactietijd45Tot60Minuten_6,
      prio1data$Reactietijd60Tot120Minuten_7,
      prio1data$ReactietijdMeerDan120Minuten_8
    )
  )
  
  pieData$percentage = pieData$count / sum(pieData$count)
  
  pieData$ymax = cumsum(pieData$percentage)
  
  pieData$ymin = c(0, head(pieData$ymax, n=-1))
  
  fig <- plot_ly(
    pieData,
    labels = ~category,
    values = ~count,
    marker = list(
      colors = c('#F6BB93', '#FBA490', '#FB8985', '#FC666F', '#B83253', '#651B40'),
      type = 'pie'
      )
    ) %>% add_pie(hole = 0.75)%>%
    layout(
      title= list(
        text = paste("<b>Reactietijd Prio 1-melding", prio1data$RegioS_label[1], "</b>"),
        y = 0.9,
        x = 0.20
      ),
      paper_bgcolor='#dddddd',
      plot_bgcolor='#dddddd',
      width = 900,
      height = 750,
      autosize = TRUE
    )
  return(fig)
}

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
    print(input$map_shape_click$id)
    

    updateTabsetPanel(session, "navBar",
                      selected = "gemeentePanel")
    regio <- input$map_shape_click$id
    periode <-  paste(input$selectionYear,"JJ00",sep = "")
    
    print(regio)
    print(periode)
    
    data <- cbs_get_data("83648NED",
                         Perioden = periode,
                         RegioS = regio,
                         select = c("SoortMisdrijf", "Perioden", "RegioS", "GeregistreerdeMisdrijvenPer1000Inw_3"))
    data <- cbs_add_label_columns(data)
    colnames(data)[which(names(data) == "RegioS")] <- "statcode"
    attributes(data$SoortMisdrijf) <- NULL
    attributes(data$GeregistreerdeMisdrijvenPer1000Inw_3) <- NULL
    output$piechart <- renderPlotly({ createPrioPieChart(regio,periode) })
  })
  
  observeEvent(input$searchBtn,{
    if(input$searchField != "" && tolower(input$searchField) %in% tolower(main_meta$RegioS$Title) )
    {
      city_crime_data <- cbs_get_data("83648NED",
                                      Perioden = paste(input$selectionYear,"JJ00",sep = ""),
                                      RegioS = main_meta$RegioS[tolower(main_meta$RegioS$Title) == tolower(input$searchField), "Key"],
                                      select = c("SoortMisdrijf", "Perioden", "RegioS", "GeregistreerdeMisdrijvenPer1000Inw_3"))
      city_crime_data <- cbs_add_label_columns(city_crime_data)
      
      pred_result <- get_theft_prediction(input$searchField)
      print(pred_result)
    }
  })
}