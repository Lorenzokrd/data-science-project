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
  
  
  prio1data_ned <- cbs_get_data("47008NED",
                                catalog = "Politie",
                                Perioden = periode,
                                RegioS = "NL01  ")
  prio1data_ned <- cbs_add_label_columns(prio1data_ned)
  
  pieData_ned <- data.frame(
    category=c("0-15 minuten","15-30 minuten","30-45 minuten","45-60 minuten","60-120 minuten","Meer dan 120 minuten"),
    count=c(
      prio1data_ned$Reactietijd0Tot15Minuten_3,
      prio1data_ned$Reactietijd15Tot30Minuten_4,
      prio1data_ned$Reactietijd30Tot45Minuten_5,
      prio1data_ned$Reactietijd45Tot60Minuten_6,
      prio1data_ned$Reactietijd60Tot120Minuten_7,
      prio1data_ned$ReactietijdMeerDan120Minuten_8
    )
  )
  
  pieData_ned$percentage = pieData_ned$count / sum(pieData_ned$count)
  
  pieData_ned$ymax = cumsum(pieData_ned$percentage)
  
  pieData_ned$ymin = c(0, head(pieData_ned$ymax, n=-1))
  
  fig <- plot_ly()
  fig <- fig%>%add_pie(data = pieData,
                       labels = ~category,
                       values = ~count,
                       marker = list(colors = c('#00BCFF', '#0096D6', '#0071AE', '#004E88', '#002E63', '#001847')),
                       domain = list(x = c(0, 0.4), y = c(0.4, 1)),
                       name = prio1data$RegioS_label,
                       hole = 0.75)
  
  fig <- fig %>%add_pie(data = pieData_ned,
                        labels = ~category,
                        values = ~count,
                        marker = list(colors = c('#FF7F0E', '#D45C00', '#AA3900', '#821200', '#5F0000', '#470000')),
                        domain = list(x = c(0.6, 1), y = c(0.4, 1)),
                        name = "Nederland",
                        hole = 0.75)
  fig <- fig %>%layout(title = list(text = paste("<b>Reactietijd Prio 1-melding", prio1data$RegioS_label[1], "</b>")),
                       paper_bgcolor='#dddddd',
                       plot_bgcolor='#dddddd',
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       showlegend = FALSE)

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
  crimes <- crimes %>% dplyr::arrange(desc(GeregistreerdeMisdrijvenPer1000Inw_3))%>% slice(2:7)
  crimes <- droplevels(crimes)
  
  crimes_ned <- get_crime_data(periode, "NL01  ")
  crimes_ned$SoortMisdrijf_label <- gsub('[0-9]+','',crimes_ned$SoortMisdrijf_label)
  crimes_ned <- crimes_ned[(crimes_ned$SoortMisdrijf_label %in% crimes$SoortMisdrijf_label),]
  
  fig <- plot_ly(crimes, y = ~SoortMisdrijf_label,
                 x = ~GeregistreerdeMisdrijvenPer1000Inw_3,
                 text = ~GeregistreerdeMisdrijvenPer1000Inw_3,
                 textfont = list(color = "#FFFFFF"),
                 name = ~RegioS_label,
                 type = "bar")
  fig <- fig %>% add_trace(x = ~crimes_ned$GeregistreerdeMisdrijvenPer1000Inw_3, name = "Nederland")
  fig <- fig %>% layout(yaxis = list(title = "Soorten Misdrijf",
                                     categoryorder = "array",
                                     categoryarray = desc(crimes$SoortMisdrijf_label)),
                        xaxis = list(title = "Misdrijven per 1000 inw."),
                        title = "Meest Voorkomende Misdrijven",
                        barmode = "group")
  
  return(fig)
}

get_theft_data <- function(cityName, crime_type)
{
  places <- cbs_get_meta("47013NED", catalog = "Politie")$RegioS
  data <- cbs_get_data("47013NED", catalog = "Politie", Perioden = has_substring("2021MM"),
                       SoortMisdrijf = has_substring(crime_type),
                       RegioS = places[tolower(places$Title) == tolower(cityName), "Key"])
  data <- cbs_add_label_columns(data)
  data <- cbs_add_date_column(data)
  return(tail(data, n = 6))
}

create_prediction_chart <- function(cityName)
{
  prediction <- as.data.frame(get_theft_prediction(cityName))

  fig <- plot_ly(prediction,alpha = 1 ,orientation = "h", type = "bar", textfont = list(color = "#FFFFFF"))
  fig <- fig %>% add_trace( text = round(prediction[,"Hi 95"]),y = row.names(prediction),x = prediction[,"Hi 95"], name = "Hoogst verwachte aantal",width = 0.3)
  fig <- fig %>%add_trace(text =round(prediction[,"Point Forecast"]) ,y = row.names(prediction),x = prediction[,"Point Forecast"], name = "Gemiddeld verwachte aantal",width = 0.3)
  fig <- fig %>% add_trace(text = round(prediction[,"Lo 95"]),y = row.names(prediction),x = prediction[,"Lo 95"], name = "Laagst verwachte aantal",width = 0.3)
  fig <- fig %>% layout(title = paste("Voorspelling aantal diefstallen", row.names(prediction),sep = " "),xaxis = list(title = "aantal diefstallen"), yaxis = list(title = "Maand", visible = FALSE), barmode = "overlay")
  return(fig)
}

bike_theft_chart <- function(cityName)
{
  bike_theft <- get_theft_data(cityName, "1.2.3")
  bike_theft_ned <- get_theft_data("Nederland", "1.2.3")
  population_meta <- cbs_get_meta("03759NED")$RegioS
  population_city <- cbs_get_data("03759NED", RegioS = population_meta[tolower(population_meta$Title) == tolower(cityName), "Key"],
                                       BurgerlijkeStaat = has_substring("T001019"),
                                       Leeftijd = "10000",
                                       Geslacht = has_substring("T001038"))
  population_ned <- cbs_get_data("03759NED", RegioS = has_substring("NL01"),
                                  BurgerlijkeStaat = has_substring("T001019"),
                                  Leeftijd = "10000",
                                  Geslacht = has_substring("T001038"))
  bike_theft_ned$GeregistreerdeMisdrijven_1 <- bike_theft_ned$GeregistreerdeMisdrijven_1 / (tail(population_ned$BevolkingOp1Januari_1)/10000)
  bike_theft$GeregistreerdeMisdrijven_1 <- bike_theft$GeregistreerdeMisdrijven_1 / (tail(population_city$BevolkingOp1Januari_1)/10000)
  
  fig <- plot_ly(bike_theft, y = ~GeregistreerdeMisdrijven_1, x = ~Perioden_Date,
                name = population_meta[tolower(population_meta$Title) == tolower(cityName), "Title"],
                type = "scatter",
                mode = "lines+markers")
  fig <- fig %>% add_trace(data = bike_theft_ned, y = ~GeregistreerdeMisdrijven_1, name = "Nederland", mode = "lines+markers")
  
  fig <- fig %>% layout(title = paste("Diefstal van brom-, snor-, fietsen","per 10 000 inwoners", sep = "\n"))
  return(fig)
}

company_robberies_chart <- function(cityName)
{
  robbery_data_city <- get_theft_data(cityName, "2.5.1")
  robbery_data_ned <- get_theft_data("Nederland", "2.5.1")
  population_meta <- cbs_get_meta("03759NED")$RegioS
  
  population_city <- cbs_get_data("03759NED", RegioS = population_meta[tolower(population_meta$Title) == tolower(cityName), "Key"],
                                  BurgerlijkeStaat = has_substring("T001019"),
                                  Leeftijd = "10000",
                                  Geslacht = has_substring("T001038"))
  
  population_ned <- cbs_get_data("03759NED", RegioS = has_substring("NL01"),
                                 BurgerlijkeStaat = has_substring("T001019"),
                                 Leeftijd = "10000",
                                 Geslacht = has_substring("T001038"))
  robbery_data_ned$GeregistreerdeMisdrijven_1 <- robbery_data_ned$GeregistreerdeMisdrijven_1 / (tail(population_ned$BevolkingOp1Januari_1)/10000)
  robbery_data_city$GeregistreerdeMisdrijven_1 <- robbery_data_city$GeregistreerdeMisdrijven_1 / (tail(population_city$BevolkingOp1Januari_1)/10000)
  
  fig <- plot_ly(robbery_data_city, y = ~GeregistreerdeMisdrijven_1, x = ~Perioden_Date,
                 name = population_meta[tolower(population_meta$Title) == tolower(cityName), "Title"],
                 type = "scatter",
                 mode = "lines+markers")
  fig <- fig %>% add_trace(data = robbery_data_ned, y = ~GeregistreerdeMisdrijven_1, name = "Nederland", mode = "lines+markers")
  
  fig <- fig %>% layout(title = paste("Diefstal en inbraak bij bedrijven","per 10 000 inwoners", sep = "\n"))
  return(fig)
}

store_robberies_chart <- function(cityName)
{
  robbery_data_city <- get_theft_data(cityName, "2.5.2")
  robbery_data_ned <- get_theft_data("Nederland", "2.5.2")
  population_meta <- cbs_get_meta("03759NED")$RegioS
  
  population_city <- cbs_get_data("03759NED", RegioS = population_meta[tolower(population_meta$Title) == tolower(cityName), "Key"],
                                  BurgerlijkeStaat = has_substring("T001019"),
                                  Leeftijd = "10000",
                                  Geslacht = has_substring("T001038"))
  
  population_ned <- cbs_get_data("03759NED", RegioS = has_substring("NL01"),
                                 BurgerlijkeStaat = has_substring("T001019"),
                                 Leeftijd = "10000",
                                 Geslacht = has_substring("T001038"))
  robbery_data_ned$GeregistreerdeMisdrijven_1 <- robbery_data_ned$GeregistreerdeMisdrijven_1 / (tail(population_ned$BevolkingOp1Januari_1)/10000)
  robbery_data_city$GeregistreerdeMisdrijven_1 <- robbery_data_city$GeregistreerdeMisdrijven_1 / (tail(population_city$BevolkingOp1Januari_1)/10000)
  
  fig <- plot_ly(robbery_data_city, y = ~GeregistreerdeMisdrijven_1, x = ~Perioden_Date,
                 name = population_meta[tolower(population_meta$Title) == tolower(cityName), "Title"],
                 type = "scatter",
                 mode = "lines+markers")
  fig <- fig %>% add_trace(data = robbery_data_ned, y = ~GeregistreerdeMisdrijven_1, name = "Nederland", mode = "lines+markers")
  
  fig <- fig %>% layout(title = paste("Winkeldiefstal","per 10 000 inwoners", sep = "\n"))
  return(fig)
}

