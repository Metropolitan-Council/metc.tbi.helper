source("R/_load_libraries_data.R")

# Load data - align levels ---------------
mode_21 <-
  tbi21 %>%
  pluck("trip") %>%
  mutate(month = lubridate::month(travel_date, label = T, abbr = T)) %>%
  mutate(mode_type_chr = as.character(mode_type)) %>%
  mutate(mode_type_chr = ifelse(
    grepl("bicy|bike", mode_type_detailed, ignore.case = T),
    "Bicycle",
    mode_type_chr
  )) %>%
  mutate(
    mode_group_2 =
      recode_factor(
        mode_type_chr
        , `Household vehicle` = "Drive"
        , `Other vehicle` = "Drive"
        , `Vehicle` = "Drive"
        , `Carshare` = "Drive"
        , `For-hire vehicle` = "Taxi/TNC"
        , Taxi = "Taxi/TNC"
        , `Smartphone-app ride-hailing service` = "Taxi/TNC"
        , `Public bus` = "Transit"
        , `Rail` = "Transit"
        , `Other bus` = "Transit"
        , `Shuttle` = "Transit"
        , `Commuter Rail` = "Transit"
        , `School bus` = "Transit"
        , `Bicycle or e-bicycle` = "Bicycle"
        , `Walk` = "Walk"
        , Other = "Other"
        , `Ferry` = "Other"
        , `Scooter-share` = "Other"
        , `Micromobility` = "Other"
        , `Long distance passenger mode` = "Other"
      )
  ) %>%
  select(-mode_type_chr)
# %>%
# filter(
#   trip_o_in_mpo == "Trip begins in Twin Cities region" |
#     trip_d_in_mpo == "Trip ends in Twin Cities region"
# )

mode_21[tbi21$hh, on=.(hh_id), hh_thrive_category_broad := i.hh_thrive_category_broad]
mode_21[is.na(hh_thrive_category_broad), hh_thrive_category_broad := "N/A"]
mode_21[tbi21$hh, on=.(hh_id), sample_segment := i.sample_segment]# Sample segment (2021)

# Mode --------
mode_share <- mode_21 %>%
  filter(!is.na(mode_group_2) & trip_weight > 0) %>%
  srvyr::as_survey_design(w = trip_weight, strata = sample_segment) %>%
  group_by(mode_group_2) %>%
  dplyr::summarize(
    pct = survey_prop(proportion = TRUE),
    ct = survey_total(),
    n = n()
  ) %>%
  mutate(year = "2021")

plot_ly(data = mode_share
        , x=~mode_group_2
        , y=~ct
        # , y=~pct
        # , y=~n
        , type = 'bar'
        ) %>%
  layout(xaxis = list(categoryorder = "total descending"))


# Mode x Commute --------
# mode_21[, .N, .(linked_trip_id)]
#
# mode_21[
#   mode_21[, .N, linked_trip_id][N>1], on=.(linked_trip_id)
# ][
#   trip_weight >0
# ][
#   look(linked_trip_id)
# ][
#   order(trip_num)
#   , .(linked_trip_id, trip_id, trip_num, trip_weight, mode_group_2, depart_time, o_purpose_category_broad,
#       arrive_time, d_purpose_category_broad)
# ]
#
# linked_trips <- mode_21[
#   mode_21[, .N, linked_trip_id][N>1], on=.(linked_trip_id)
# ]
# linked_trips
#
# single_trips <- mode_21[
#   !(mode_21[, .N, linked_trip_id][N>1]), on=.(linked_trip_id)
# ]
#
# linked_trips[, {
#   d_work <- which(d_purpose_category_broad == "Work")
#   o_work <- which(o_purpose_category_broad == "Work")
#   list(o_work, d_work, .N)
# }, linked_trip_id]
#
#
# mode_21[, .N, keyby = .(d_purpose_category_broad, d_purpose_category, d_purpose)] %>%
#   print(n = 1000)


mode_share_commute <-
  mode_21 %>%
  mutate(
    commute = ifelse(
      o_purpose_category_broad %in% "Work" |
        o_purpose_category_broad  %in% "Work"
      , "Commute"
      , "Non-Commute"
    )
  ) %>%
  filter(!is.na(mode_group_2) & trip_weight > 0) %>%
  srvyr::as_survey_design(w = trip_weight, strata = sample_segment) %>%
  group_by(mode_group_2, commute) %>%
  dplyr::summarize(
    pct = survey_prop(),
    ct = survey_total(),
    n = n()
  ) %>%
  mutate(year = "2021")

plot_ly(data = mode_share_commute
        , x=~mode_group_2
        , y=~n
        , color=~ commute
        , type = 'bar'
        ) %>%
  layout(xaxis = list(categoryorder = "total descending"))


plot_ly(data = mode_share_commute
        , x=~mode_group_2
        , y=~pct
        , color=~ commute
        , type = 'bar'
        ) %>%
  layout(barmode = "stack",
         xaxis = list(categoryorder = "max ascending"))


# Mode x Community Designation --------
mode_share_thrive_category <-
mode_21 %>%
  filter(!is.na(mode_group_2) & trip_weight > 0) %>%
  srvyr::as_survey_design(w = trip_weight, strata = sample_segment) %>%
  group_by(hh_thrive_category_broad, mode_group_2) %>%
  dplyr::summarize(
    pct = survey_prop(),
    ct = survey_total(),
    n = n()
  ) %>%
  mutate(year = "2021")

plot_ly(data = mode_share_thrive_category
        , x=~hh_thrive_category_broad
        , y=~n
        , color=~ mode_group_2
        , colors = 'Spectral'
        , type = 'bar'
) %>%
  layout(xaxis = list(categoryorder = "max decending"))

plot_ly(data = mode_share_thrive_category
        , x=~hh_thrive_category_broad
        , y=~pct
        , color=~ mode_group_2
        , colors = 'Spectral'
        , type = 'bar'
) %>%
  layout(
    barmode = "stack"
    , xaxis = list(categoryorder = "max decending")
  )


# Mode x Trip Origin --------
# Origins of trips by mode, this a map? by city? ..
mode_21
mode_21 %>%
  filter(!is.na(mode_group_2) & trip_weight > 0) %>%
  srvyr::as_survey_design(w = trip_weight, strata = sample_segment) %>%
  group_by(hh_thrive_category_broad, mode_group_2) %>%
  dplyr::summarize(
    pct = survey_prop(),
    ct = survey_total(),
    n = n()
  ) %>%
  mutate(year = "2021")






# __ Parking Lot -------
# align mode_type_detailed levels
# tbi21$trip <- tbi21 %>%
#   pluck("trip") %>%
#   mutate(
#     mode_type_detailed = mode_type_detailed %>%
#       as.character()
#     , mode_type_detailed = case_match(
#       mode_type_detailed
#       , "ATV or snowmobile" ~ "ATV or snowmobile"
#       , "Bike-share - electric bicycle" ~ "Bike-share eBike"
#       , "Bike-share - standard bicycle" ~ "Bike-share regBike"
#       , "Boat/ferry/water taxi" ~ "Watercraft"
#       , "Borrowed bicycle (e.g., from friend)" ~ "Bike borrwed"
#       , "Bus Rapid Transit (BRT) (e.g., A Line, C Line, Red Line)" ~ "Bus Rapid Transit"
#       , "Car rental" ~ "Car rental"
#       , "Carpool match (e.g., Waze Carpool)" ~ "Carpool match"
#       , "Carshare service (e.g., Zipcar)" ~ "Carshare service"
#       , "Electric bicycle (my household's)" ~ "eBicycle owned"
#       , "Employer-provided shuttle/bus" ~ "Employer shuttle/bus"
#       , "Express bus" ~ "Express bus"
#       , "Friend/relative/colleague's car" ~ "Car borrwed"
#       , "Golf cart" ~ "Golf cart"
#       , "Household vehicle 1" ~ "Household vehicle"
#       , "Household vehicle 2" ~ "Household vehicle"
#       , "Household vehicle 3" ~ "Household vehicle"
#       , "Household vehicle 4" ~ "Household vehicle"
#       , "Household vehicle 5" ~ "Household vehicle"
#       , "Household vehicle 6" ~ "Household vehicle"
#       , "Household vehicle 8" ~ "Household vehicle"
#       , "Intercity bus (e.g., Greyhound, Bolt Bus)" ~ "Intercity bus"
#       , "Intercity rail (e.g., Amtrak)" ~ "Intercity rail"
#       , "Light rail (e.g., Blue Line, Green Line)" ~ "Light rail"
#       , "Local fixed-route bus" ~ "Local bus"
#       , "Medical transportation service" ~ "Medical transportation service"
#       , "Metro Mobility" ~ "Metro Mobility"
#       , "Moped share (e.g., Scoot)"  ~ "Moped share"
#       , "Northstar" ~ "Northstar"
#       , "Other" ~ "Other"
#       , "Other boat (e.g., kayak)" ~ "Other"
#       , "Other bus" ~ "Other"
#       , "Other car" ~ "Other"
#       , "Other hired car service (e.g., black car, limo)" ~ "Other"
#       , "Other motorcycle" ~ "Other"
#       , "Other motorcycle (not my household's)" ~ "Other"
#       , "Other private shuttle/bus (e.g., a hotel's, an airport's)" ~ "Other"
#       , "Other rail" ~ "Other"
#       , "Other rented bicycle" ~ "Other"
#       , "Other scooter or moped" ~ "Other"
#       , "Other vehicle in household" ~ "Other"
#       , "Paratransit/Dial-A-Ride" ~ "Paratransit"
#       , "Personal scooter or moped (not shared)" ~ "Scooter owned"
#       , "Regular taxi" ~ "Taxi"
#       , "School bus" ~ "School bus"
#       , "Scooter-share (e.g., Bird, Lime)" ~ "Scooter-share"
#       , "Skateboard/rollerblade" ~ "Skateboard/rollerblade"
#       , "Snowmobile" ~ "ATV or snowmobile"
#       , "SouthWest Prime or MVTA Connect" ~ "Micro transit"
#       , "Standard bicycle (my household's)" ~ "Bicycle owned"
#       , "Uber, Lyft, or other smartphone-app ride service" ~ "App ride service"
#       , "University/college shuttle/bus" ~ "University shuttle"
#       , "Vanpool" ~ "Vanpool"
#       , "Vehicle ferry (took vehicle on board)" ~ "Watercraft"
#       , "Walk (or jog/wheelchair)" ~ "Walk, jog, roll"
#       , "Work vehicle" ~ "Work vehicle"
#
#     )
#   )













