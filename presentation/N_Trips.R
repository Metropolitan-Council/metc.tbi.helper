source("_load_libraries.R")
source("_load_data.R")

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

# by year -----------
plot_ly() %>%
  add_bars(
    data = tbi$trip[, sum(trip_weight), survey_year]
    , y =~ V1
    , x = ~ survey_year %>% as.character()
    , color = ~ survey_year %>% as.character()
    , marker = list(color = c(colors$councilBlue, colors$esBlue))
    , text =~ V1 %>% prettyNum(',')
    , textfont = list(color = "white")
  ) %>%
  layout(
    barmode = 'group'
    , yaxis = list(title = "Average Weekday Trips")
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    , legend = list(traceorder = "reversed")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/trip_count.svg", width = 400, height = 600)

tbi$trip[, sum(trip_weight), survey_year][, diff(V1)/max(V1)]

# map ----------
tbi$trip[tbi$household, on="hh_id", home_county := i.home_county]
tbi$trip[, home_county :=  home_county %>% str_replace_all(" County, .*", '')]
map_data <-
  tbi$trip[
    , .(trips = sum(trip_weight), .N)
    , keyby = .(home_county, survey_year)
  ] %>%
  dcast(home_county ~ survey_year, value.var = "trips") %>%
  .[polygonsMN, on=.(home_county = co_name), geometry := wkt] %>%
  .[polygonsWI, on=.(home_county = name), geometry := wkt] %>%
  .[polygonsMN, on=.(home_county = co_name), centroid := i.centroid] %>%
  .[polygonsWI, on=.(home_county = name), centroid := i.centroid] %>%
  .[, pct_change := (`2021` - `2019`)/`2019`] %>%
  setorder(pct_change) %>%
  print

pal <- colorBin(
  palette = c("#0E15CC", "#4E0ECC", "#740ECC", "#CC0E67"),
  # domain = map_data$pct_change,
  bins = c(-1, -0.15, 0, 1),
  reverse = F
)

labs <- map_data[, .N] %>%
  seq %>%
  lapply(\(i){
    with(map_data[i],
         sprintf('<strong>%s%%</strong>'
                 , round(pct_change * 100, 1)
         )
         )
  }) %>%
  lapply(HTML)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = map_data$geometry
               , weight = 1
               , color = pal(map_data$pct_change)
               , fill = T
               , fillColor = pal(map_data$pct_change)
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
    , values = map_data$pct_change
    , title = "Change in Trips <br> 2019 to 2021"
    , opacity = 0.5
    , labFormat = labelFormat(suffix = "%", transform = \(x){100*x})
  )



map_data[order(-`2021`), .(
  home_county,
  "2019 Trips" = `2019` %>% round() %>% prettyNum(','),
  "2021 Trips" = `2021` %>% round() %>% prettyNum(','),
  paste0(round(pct_change * 100, 1)  , "%")
)] %>% fwrite("output/mapTrips.csv")




