source("_load_libraries.R")
source("_load_data.R")

tbi$trip %>% names %>% str_subset("dist")
tbi$trip[survey_year == "2019", linked_trip_id := paste0(person_id, "_", linked_trip_num)]
tbi$trip[survey_year == '2019', depart_time := depart_time + 60^2]
tbi$trip[survey_year == '2019', depart_time_imputed := depart_time_imputed + 60^2]
tbi$trip[, depart_time := fifelse(!is.na(depart_time_imputed), depart_time_imputed, depart_time)]
tbi$trip[, depart_time_imputed := NULL]
setkey(tbi$trip, linked_trip_id, leg_num)

# trips that are serviced by transit
dest2019 <-
  tbi$trip[survey_year == "2019", .(trip_id, d_lat, d_lon)] %>%
  st_as_sf(coords = c("d_lon", "d_lat"), crs = 4326) %>%
  st_join(st_as_sf(transit_access_2019), join = st_within, left = F) %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  print

org2019 <-
  tbi$trip[survey_year == "2019", .(trip_id, o_lat, o_lon)] %>%
  st_as_sf(coords = c("o_lon", "o_lat"), crs = 4326) %>%
  st_join(st_as_sf(transit_access_2019), join = st_within, left = F) %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  print

dest2021 <-
  tbi$trip[survey_year == "2021", .(trip_id, d_lat, d_lon)] %>%
  st_as_sf(coords = c("d_lon", "d_lat"), crs = 4326) %>%
  st_join(st_as_sf(transit_access_2021), join = st_within, left = F) %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  print

org2021 <-
  tbi$trip[survey_year == "2021", .(trip_id, o_lat, o_lon)] %>%
  st_as_sf(coords = c("o_lon", "o_lat"), crs = 4326) %>%
  st_join(st_as_sf(transit_access_2021), join = st_within, left = F) %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  print

trips <-
rbind(
  tbi$trip[survey_year == 2019] %>%
    inner_join(dest2019, by="trip_id") %>%
    inner_join(org2019, by="trip_id"),
  tbi$trip[survey_year == 2021] %>%
    inner_join(dest2021, by="trip_id") %>%
    inner_join(org2021, by="trip_id")
)
tbi$trip[, transit_q_mile := F]
tbi$trip[trips, on='trip_id', transit_q_mile := T]

# plot
setkey(tbi$trip, linked_trip_id, leg_num)
tbi$trip[, uniqueN(trip_weight), .(survey_year, linked_trip_id)][, .N, .(survey_year, V1)]

tbi$trip[tbi$household, on='hh_id', sample_segment := i.sample_segment]

transit <-
  tbi$trip[
    transit_q_mile == T
    , .(
      transit_mode = fifelse("Rail" %in% mode_type | "Public Bus" %in% mode_type
                             , "Transit"
                             , mode_type[1])
      , trip_weight = min(trip_weight)
      , dist = sum(distance_miles)
    )
    , .(linked_trip_id, survey_year, sample_segment)] %>%
  srvyr::as_survey_design(weights = trip_weight, strata = sample_segment) %>%
  group_by(survey_year, transit_mode) %>%
  summarize(
    pct = survey_prop(),
    ct = survey_total(),
    dist = survey_mean(dist, na.rm = T, vartype = "ci")
  ) %>%
  as.data.table() %>%
  print

# transit rides share
plot_ly() %>%
  add_bars(data = transit[transit_mode == "Transit"]
           , x=~ survey_year
           , y=~ pct
           , color = ~survey_year
           , colors = c(colors$councilBlue, colors$esBlue)
           , text =~ sprintf("%sM<br>%s%%"
                             , round(ct/1000000, 2) %>% abs()
                             , round(pct * 100, 1) %>% abs())
           , textfont = list(color = "white")
           # , textangle = 0
           , textposition = "inside"
           ) %>%
  layout(
    title = "Reduction in Trasit Rides",
    yaxis = list(title = "Percent of Trips"
                 , tickformat = ".0%")
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    # , legend = list(traceorder = "reversed")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/transit_share.svg", width = 400, height = 600)

# transit trip distance
plot_ly() %>%
  add_bars(data = transit[transit_mode == "Transit"]
           , x=~ survey_year
           , y=~ dist
           , color = ~survey_year
           , colors = c(colors$councilBlue, colors$esBlue)
           , text =~ sprintf("%s", dist %>% round(1))
           , textfont = list(color = "white")
           , textposition = "inside"
           , insidetextanchor = "middle"
           , error_y = list(array = 1, color = 'black')
           ) %>%
  layout(
    title = "No Change in Average <br>Trip Distance",
    yaxis = list(title = "Miles"
                 # , tickformat = ".0%"
                 )
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    # , legend = list(traceorder = "reversed")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/transit_dist.svg", width = 400, height = 600)


# Trips per person ------------------
tbi$trip[
  , .(trip_weight = min(trip_weight))
  , .(linked_trip_id, survey_year, sample_segment, travel_date)
] %>%
  .[trip_weight > 0] %>%
  .[
    , .N
    , .(linked_trip_id, survey_year)
  ] %>%
  .[, .N, N]





