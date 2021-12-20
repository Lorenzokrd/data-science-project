get_theft_prediction <- function(cityName)
{
  places <- cbs_get_meta("47015NED", catalog = "Politie")$Plaatsen
  data <- cbs_get_data("47015NED",
                       catalog = "Politie",
                       Perioden = has_substring("2019MM") | has_substring("2020MM") | has_substring("2021MM"),
                       Plaatsen = places[tolower(places$Title) == tolower(cityName), "Key"],
                       SoortMisdrijf = has_substring("1.1.1"))
  data <- cbs_add_label_columns(data)
  last_period <- tail(data, n = 1)$Perioden[1]
  endMonth <- as.numeric(str_split(last_period,"MM", simplify = TRUE)[2])
  endYear <- as.numeric(str_split(last_period,"MM", simplify = TRUE)[1])
  time_s <- ts(data$GeregistreerdeMisdrijven_1,start=c(2019, 1), end=c(endYear, endMonth), frequency=12)
  fit <- nnetar(time_s, lambda = "auto")
  fcast <- forecast(fit, PI=TRUE, h=1, model = forecast.ets)
  
  return(fcast)
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
      type = 'pie'))%>%
    add_pie(hole = 0.75)%>%
    layout( 
      title = list(text = paste("<b>Reactietijd Prio 1-melding", prio1data$RegioS_label[1], "</b>")),
      paper_bgcolor='#dddddd',
      plot_bgcolor='#dddddd',
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      margin = list(t = 105))
  return(fig)
}

get_crime_data <- function(periode,regio)
{
  data <- cbs_get_data("83648NED",
                       Perioden = periode,
                       RegioS = regio,
                       select = c("SoortMisdrijf", "Perioden", "RegioS", "GeregistreerdeMisdrijvenPer1000Inw_3"))
  data <- cbs_add_label_columns(data)
  colnames(data)[which(names(data) == "RegioS")] <- "statcode"
  attributes(data$SoortMisdrijf) <- NULL
  attributes(data$GeregistreerdeMisdrijvenPer1000Inw_3) <- NULL
  
  return(data)
}

create_crime_ranking <- function(periode, regio)
{
  crimes <- get_crime_data(periode, regio)
  crimes$SoortMisdrijf_label <- gsub('[0-9]+','',crimes$SoortMisdrijf_label)
  crimes <- crimes %>% dplyr::arrange(desc(GeregistreerdeMisdrijvenPer1000Inw_3))%>% slice(1:6)
  crimes <- droplevels(crimes)
  
  fig <- plot_ly(crimes, y = ~SoortMisdrijf_label, x = ~GeregistreerdeMisdrijvenPer1000Inw_3, text = ~GeregistreerdeMisdrijvenPer1000Inw_3)
  fig <- fig %>% layout(yaxis = list(title = "Soorten Misdrijf",
                                     categoryorder = "array",
                                     categoryarray = desc(crimes$SoortMisdrijf_label)),
                        xaxis = list(title = "Misdrijven per 1000 inw."),
                        title = "Meest Voorkomende Misdrijven")
  return(fig)
}

get_theft_data <- function(cityName)
{
  places <- cbs_get_meta("47015NED", catalog = "Politie")$Plaatsen
  data <- cbs_get_data("47015NED", catalog = "Politie", Perioden = has_substring("2021MM"),
                       SoortMisdrijf = has_substring("1.2.3"),
                       Plaatsen = places[tolower(places$Title) == tolower(cityName), "Key"])
  last_period <- tail(data, n = 1)$Perioden[1]
  endMonth <- str_split(last_period,"MM", simplify = TRUE)[2]
  endYear <- str_split(last_period,"MM", simplify = TRUE)[1]
  last_theft_number <- tail(data, n = 1)$GeregistreerdeMisdrijven_1[1]
  return(c(endMonth, endYear,last_theft_number))
}

create_prediction_chart <- function(cityName)
{
  prediction <- as.data.frame(get_theft_prediction(cityName))

  fig <- plot_ly(prediction,alpha = 1 ,orientation = "h", type = "bar")
  fig <- fig %>% add_trace( text = round(prediction[,"Hi 95"]),y = row.names(prediction),x = prediction[,"Hi 95"], name = "Hoogst verwachte aantal",width = 0.3)
  fig <- fig %>%add_trace(text =round(prediction[,"Point Forecast"]) ,y = row.names(prediction),x = prediction[,"Point Forecast"], name = "Gemiddeld verwachte aantal",width = 0.3)
  fig <- fig %>% add_trace(text = round(prediction[,"Lo 95"]),y = row.names(prediction),x = prediction[,"Lo 95"], name = "Laagst verwachte aantal",width = 0.3)
  fig <- fig %>% layout(title = paste("Voorspelling aantal diefstallen", row.names(prediction),sep = " "),xaxis = list(title = "aantal diefstallen"), yaxis = list(title = "Maand", visible = FALSE), barmode = "overlay")
  return(fig)
}
