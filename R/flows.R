source("presentation/_load_libraries.R")
if(!exists("tbi")) source("presentation/_load_data.R")
source('presentation/_plot_styling.R')


setkey(tbi$trip, trip_id, leg_num)
d_city <-
  tbi$trip[, .(trip_d_city = data.table::last(trip_d_city)),
           keyby = .(linked_trip_id)]

# dest x year ----------
od_dest <-
  tbi$linked_trip %>%
  .[tbi$hh, on="hh_id", hh_city := i.hh_city %>% as.character()] %>%
  .[d_city, on='linked_trip_id', trip_d_city := i.trip_d_city] %>%
  .[!tbi$hh[!(hh_in_mpo) | is.na(hh_city)], on="hh_id"] %>%
  .[, trip_dest := case_when(
    hh_city == trip_d_city ~ 'Home City',
    trip_d_city == "Missing" ~ "Missing Destination",
    .default = "Different City"
  )] %>%
  .[is.na(trip_d_city), trip_dest := "Missing"] %>%
  .[, sum(trip_weight), keyby = .(survey_year, trip_dest)] %>%
  .[, pct := V1/sum(V1), survey_year] %>%
  print

od_dest %>%
  plot_ly() %>%
  add_bars(
    x=~trip_dest %>% fct_reorder(-pct),
    y=~ pct,
    color=~survey_year
  ) %>%
  councilR::plotly_layout(
    main_title = "Trip Destinations",
    subtitle = 'Source: TBI Household 2019-2023',
    y_title = "Pct of Trips",
    x_title = ""
  ) %>%
  layout(
    # barmode = 'stack',
    yaxis = list(tickformat = ".0%")
  )

# dest x employment x year = 2023 ----------
od_dest <-
  tbi$linked_trip %>%
  .[tbi$hh, on="hh_id", hh_city := i.hh_city %>% as.character()] %>%
  .[tbi$person, on="person_id", employment := i.employment] %>%
  .[d_city, on='linked_trip_id', trip_d_city := i.trip_d_city] %>%
  .[!tbi$hh[!(hh_in_mpo) | is.na(hh_city)], on="hh_id"] %>%
  .[, trip_dest := case_when(
    hh_city == trip_d_city ~ 'Home City',
    trip_d_city == "Missing" ~ "Missing Destination",
    .default = "Different City"
  )] %>%
  .[is.na(trip_d_city), trip_dest := "Missing"] %>%
  .[survey_year == 2023] %>%
  .[, sum(trip_weight), keyby = .(employment, trip_dest)] %>%
  .[, pct := V1/sum(V1), .(employment)] %>%
  print

catorder <- od_dest[trip_dest == "Home City"][order(pct), employment]
od_dest %>%
  filter(!employment %>% str_detect("100%")) %>%
  droplevels() %>%
  plot_ly() %>%
  add_bars(
    color=~trip_dest %>% fct_reorder(-pct),
    x=~ pct,
    y=~employment %>% factor(catorder, ordered = T)
  ) %>%
  councilR::plotly_layout(
    main_title = "Trip Destinations",
    subtitle = 'Source: TBI Household 2023',
    x_title = "Percent of Trips",
    y_title = "Employment"
  ) %>%
  layout(
    barmode = 'stack',
    xaxis = list(tickformat = ".0%")
  )

# dest x income x year = 2023 ----------
od_dest <-
  tbi$linked_trip %>%
  .[tbi$hh, on="hh_id", hh_city := i.hh_city %>% as.character()] %>%
  .[tbi$hh, on="hh_id", income_broad := i.income_broad] %>%
  .[d_city, on='linked_trip_id', trip_d_city := i.trip_d_city] %>%
  .[!tbi$hh[!(hh_in_mpo) | is.na(hh_city)], on="hh_id"] %>%
  .[, trip_dest := case_when(
    hh_city == trip_d_city ~ 'Home City',
    trip_d_city == "Missing" ~ "Missing Destination",
    .default = "Different City"
  )] %>%
  .[is.na(trip_d_city), trip_dest := "Missing"] %>%
  .[survey_year == 2023] %>%
  .[, sum(trip_weight), keyby = .(income_broad, trip_dest)] %>%
  .[, pct := V1/sum(V1), .(income_broad)] %>%
  print

od_dest %>%
  plot_ly() %>%
  add_bars(
    color=~trip_dest %>% fct_reorder(-pct),
    x=~ pct,
    y=~income_broad
  ) %>%
  councilR::plotly_layout(
    main_title = "Trip Destinations",
    subtitle = 'Source: TBI Household 2023',
    x_title = "Percent of Trips",
    y_title = ""
  ) %>%
  layout(
    barmode = 'stack',
    xaxis = list(tickformat = ".0%")
  )

# dest x race x year = 2023 ----------
od_dest <-
  tbi$linked_trip %>%
  .[tbi$hh, on="hh_id", hh_city := i.hh_city %>% as.character()] %>%
  .[tbi$person, on="person_id", race_ethnicity := i.race_ethnicity] %>%
  .[d_city, on='linked_trip_id', trip_d_city := i.trip_d_city] %>%
  .[!tbi$hh[!(hh_in_mpo) | is.na(hh_city)], on="hh_id"] %>%
  .[, trip_dest := case_when(
    hh_city == trip_d_city ~ 'Home City',
    trip_d_city == "Missing" ~ "Missing Destination",
    .default = "Different City"
  )] %>%
  .[is.na(trip_d_city), trip_dest := "Missing"] %>%
  .[survey_year == 2023] %>%
  .[, sum(trip_weight), keyby = .(race_ethnicity, trip_dest)] %>%
  .[, pct := V1/sum(V1), .(race_ethnicity)] %>%
  print

catorder <- od_dest[trip_dest == "Home City"][order(pct), race_ethnicity]
od_dest %>%
  plot_ly() %>%
  add_bars(
    color=~trip_dest %>% fct_reorder(-pct),
    x=~ pct,
    y=~race_ethnicity %>% factor(catorder, ordered = T)
  ) %>%
  councilR::plotly_layout(
    main_title = "Trip Destinations",
    subtitle = 'Source: TBI Household 2023',
    x_title = "Percent of Trips",
    y_title = ""
  ) %>%
  layout(
    barmode = 'stack',
    xaxis = list(tickformat = ".0%")
  )



