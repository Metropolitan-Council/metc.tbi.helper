source("presentation/_load_libraries.R")
source("presentation/_load_data.R")

polygonsMN <- councilR::import_from_gis("MNCounties") %>%
  clean_names() %>%
  mutate(centroid = st_centroid(wkt) %>% st_transform(4326)) %>%
  st_transform(4326) %>%
  as.data.table() %>%
  print
polygonsWI <- councilR::import_from_gis("WICounties") %>%
  clean_names() %>%
  mutate(centroid = st_centroid(wkt) %>% st_transform(4326)) %>%
  st_transform(4326) %>%
  as.data.table() %>%
  .[name == "St. Croix", name := "St Croix"] %>%
  .[name != "Washington"] %>%
  print


# trips by year -----------
plot_ly() %>%
  add_bars(
    data = trip[, sum(trip_weight, na.rm = TRUE), survey_year]
    , y = ~ V1
    , x = ~ survey_year %>% as.character()
    , color = ~ survey_year %>% as.character()
    , colors = c(colors$councilBlue, colors$esBlue)
    , text = ~ V1 %>% prettyNum(',')
    , textfont = list(color = "white")
  ) %>%
  layout(
    barmode = 'group'
    , yaxis = list(title = "Weekday Trips")
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    , legend = list(traceorder = "reversed")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/trip_count.svg", width = 400, height = 600)

#trip[, sum(trip_weight, na.rm = TRUE), survey_year][, diff(V1)/max(V1)]


# trip rate by year ----
trip[household, on="hh_id", home_county := i.home_county]
person[household, on="hh_id", home_county := i.home_county]
# person weight by county and year
population <- person[, .(pop = sum(person_weight, na.rm = TRUE)), .(home_county, survey_year)]
# person weight by year
person_wt <- person[, .(pop = sum(person_weight, na.rm = TRUE)), .(survey_year)]

trip_rate <- trip[
  , .(trips = sum(trip_weight, na.rm = TRUE), .N)
  , keyby = .(survey_year)
] %>%
  .[person_wt, on = .(survey_year), pop := i.pop] %>%
  .[, .(trip_rate = trips/pop), survey_year]

plot_ly() %>%
  add_bars(
    data = trip_rate
    , y = ~ trip_rate
    , x = ~ survey_year %>% as.character()
    , color = ~ survey_year %>% as.character()
    , colors = c(colors$councilBlue, colors$esBlue)
    , text = ~ trip_rate %>% round(2)
    , textfont = list(color = "white")
  ) %>%
  layout(
    barmode = 'group'
    , yaxis = list(title = "Weekday Trips per capita")
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    , legend = list(traceorder = "reversed")
    , margin = list(t = 50)
  ) %>%
  print


# map ----------
map_data <-
  trip[
    , .(trips = sum(trip_weight, na.rm = TRUE), .N)
    , keyby = .(home_county, survey_year)
  ] %>%
  .[population, on = .(home_county, survey_year), pop := i.pop] %>%
  .[, trip_rate := trips/pop] %>%
  .[, home_county :=  home_county %>% str_replace_all(" County, .*", '')] %>%
  #.[N > 1000] %>%
  dcast(home_county ~ survey_year, value.var = "trip_rate") %>%
  .[polygonsMN, on=.(home_county = co_name), geometry := wkt] %>%
  .[polygonsWI, on=.(home_county = name), geometry := wkt] %>%
  .[polygonsMN, on=.(home_county = co_name), centroid := i.centroid] %>%
  .[polygonsWI, on=.(home_county = name), centroid := i.centroid] %>%
  .[, pct_change_1 := (`2021` - `2019`)/`2019`] %>%
  .[, pct_change_2 := (`2023` - `2019`)/`2019`] %>%
  .[, pct_change_3 := (`2023` - `2021`)/`2021`] %>%
  setorder(pct_change_1) %>%
  na.omit() %>%
  print

pal <- colorBin(
  palette = c("#0E15CC", "#4E0ECC", "#740ECC", "#CC0E67"),
  # domain = map_data$pct_change,
  bins = c(-1, -0.15, 0, 1),
  reverse = FALSE
)

# change in Trip rate 2019 to 2021
labs <- map_data[, .N] %>%
  seq %>%
  lapply(\(i){
    with(map_data[i],
         sprintf('<strong>%s%%</strong>'
                 , round(pct_change_1 * 100, 1)
         )
         )
  }) %>%
  lapply(HTML)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = map_data$geometry
               , weight = 1
               , color = pal(map_data$pct_change_1)
               , fill = TRUE
               , fillColor = pal(map_data$pct_change_1)
               , fillOpacity = 0.5
               ) %>%
  addLabelOnlyMarkers(
    data = map_data$centroid
    , label = labs
    , labelOptions = labelOptions(noHide = TRUE
                                   , direction = 'center'
                                   , textOnly = TRUE
                                   , textsize = "15px"
                                  )

  ) %>%
  addLegend(
    pal = pal
    , values = map_data$pct_change_1
    , title = "Change in Trip Rate <br> 2019 to 2021"
    , opacity = 0.5
    , labFormat = labelFormat(suffix = "%", transform = \(x){100*x})
  )

# change in trip rate 2019 to 2023
labs <- map_data[, .N] %>%
  seq %>%
  lapply(\(i){
    with(map_data[i],
         sprintf('<strong>%s%%</strong>'
                 , round(pct_change_2 * 100, 1)
         )
    )
  }) %>%
  lapply(HTML)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = map_data$geometry
               , weight = 1
               , color = pal(map_data$pct_change_2)
               , fill = TRUE
               , fillColor = pal(map_data$pct_change_2)
               , fillOpacity = 0.5
  ) %>%
  addLabelOnlyMarkers(
    data = map_data$centroid
    , label = labs
    , labelOptions = labelOptions(noHide = TRUE
                                  , direction = 'center'
                                  , textOnly = TRUE
                                  , textsize = "15px"
    )

  ) %>%
  addLegend(
    pal = pal
    , values = map_data$pct_change_2
    , title = "Change in Trip Rate <br> 2019 to 2023"
    , opacity = 0.5
    , labFormat = labelFormat(suffix = "%", transform = \(x){100*x})
  )


# change in Trip rate 2021 to 2023
labs <- map_data[, .N] %>%
  seq %>%
  lapply(\(i){
    with(map_data[i],
         sprintf('<strong>%s%%</strong>'
                 , round(pct_change_3 * 100, 1)
         )
    )
  }) %>%
  lapply(HTML)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = map_data$geometry
               , weight = 1
               , color = pal(map_data$pct_change_3)
               , fill = TRUE
               , fillColor = pal(map_data$pct_change_3)
               , fillOpacity = 0.5
  ) %>%
  addLabelOnlyMarkers(
    data = map_data$centroid
    , label = labs
    , labelOptions = labelOptions(noHide = TRUE
                                  , direction = 'center'
                                  , textOnly = TRUE
                                  , textsize = "15px"
    )

  ) %>%
  addLegend(
    pal = pal
    , values = map_data$pct_change_3
    , title = "Change in Trip Rate <br> 2021 to 2023"
    , opacity = 0.5
    , labFormat = labelFormat(suffix = "%", transform = \(x){100*x})
  )


map_data[order(-`2021`), .(
  home_county,
  "2019 Trips" = `2019` %>% round() %>% prettyNum(','),
  "2021 Trips" = `2021` %>% round() %>% prettyNum(','),
  paste0(round(pct_change_1 * 100, 1)  , "%")
)] %>% fwrite("presentation/output/mapTrips_19-21.csv")

map_data[order(-`2023`), .(
  home_county,
  "2019 Trips" = `2019` %>% round() %>% prettyNum(','),
  "2023 Trips" = `2023` %>% round() %>% prettyNum(','),
  paste0(round(pct_change_2 * 100, 1)  , "%")
)] %>% fwrite("presentation/output/mapTrips_19-23.csv")



