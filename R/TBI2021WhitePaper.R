# load data & libraries
source("R/_load_libraries_data.R")

# link trips
source("R/linkingTrips.R")

# Load data - align levels ---------------

linked_trips_19_mpo <- linked_trips_19[
    , mode_type_chr := as.character(mode_type)
  ][
    , mode_type_chr := ifelse(
      grepl("bicy|bike", mode_type_detailed, ignore.case = T),
      "Bicycle",
      mode_type_chr
    )
  ][
    ,  mode_group_2 :=
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
  ][
    trip_o_in_mpo == "Trip begins in Twin Cities region" |
      trip_d_in_mpo == "Trip ends in Twin Cities region"
  ]

linked_trips_19_mpo[tbi19$hh, on=.(hh_id), hh_thrive_category_broad := i.hh_thrive_category_broad]
linked_trips_19_mpo[is.na(hh_thrive_category_broad), hh_thrive_category_broad := "N/A"]
linked_trips_19_mpo[tbi19$hh, on=.(hh_id), sample_segment := i.sample_segment]

# Mode --------
mode_share <- linked_trips_19_mpo[
  !is.na(mode_group_2) & trip_weight > 0
  , {
    as_survey_design(.SD, w = trip_weight, strata = sample_segment) %>%
      group_by(mode_group_2) %>%
      dplyr::summarize(
        pct = survey_prop(proportion = TRUE),
        ct = survey_total(),
        n = n()
      ) %>%
      mutate(year = "2021")
  }
]

plot_ly(data = mode_share
        , x=~mode_group_2
        , y=~ct
        # , y=~pct
        # , y=~n
        , type = 'bar'
        ) %>%
  layout(xaxis = list(categoryorder = "total descending"))


# # **** under construction ---------------------------------------------
# # There is an issue with weighting
#
# # single_leg_trips <- tbi19$trip[!(tbi19$trip[, .N, linked_trip_id][N>1]), on=.(linked_trip_id)]
# # multi_leg_trips <- tbi19$trip[tbi19$trip[, .N, linked_trip_id][N>1], on=.(linked_trip_id)]
#
# single_leg_trips <- mode_19[
#   !(mode_19[, .N, linked_trip_id][N>1]), on=.(linked_trip_id)
# ]
#
# multi_leg_trips <- mode_19[
#   mode_19[, .N, linked_trip_id][N>1], on=.(linked_trip_id)
# ]
# mode_19[unlinked_trip == "Yes"][, .N, linked_trip_id][, .N, keyby = N]
# mode_19[unlinked_trip == "No"][, .N, linked_trip_id][, .N, keyby = N]
#
#
# single_leg_trips[,  .(any("Transit" %in% mode_group), .N), linked_trip_id][, .N, V1]
# multi_leg_trips[,  .(any("Transit" %in% mode_group), .N), linked_trip_id][, .N, V1]
#
# multi_leg_trips[look(linked_trip_id)][order(trip_num), .(mode_type, mode_group, trip_weight, duration, distance, is_access, is_egress, day_num, trip_survey_complete, unlinked_trip, travel_dow)]
#
# #  **** end under construction ---------------------------------------------


# Mode x Commute -------
# linked_trips_19_mpo %>% names %>% str_subset("purp")
# linked_trips_19_mpo[, .N, keyby = o_purpose_category_imputed]

mode_share_commute <-
  linked_trips_19_mpo[
  , commute := fifelse(
    o_purpose_category_imputed %in% c("Work", "Work-related") |
      d_purpose_category_imputed  %in% c("Work", "Work-related")
    , "Commute"
    , "Non-Commute"
  )
][
  !is.na(mode_group_2) & trip_weight > 0
  , {
    as_survey_design(.SD, w = trip_weight, strata = sample_segment) %>%
      group_by(mode_group_2, commute) %>%
      dplyr::summarize(
        pct = survey_prop(proportion = TRUE),
        ct = survey_total(),
        n = n()
      ) %>%
      mutate(year = "2021")
  }
]

# plot_ly(data = mode_share_commute
#         , x=~commute
#         , y=~ct
#         , color=~mode_group_2
#         , type = 'bar'
#         ) %>%
#   layout(xaxis = list(categoryorder = "total descending"))

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
  linked_trips_19_mpo %>%
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
# Origins of trips by mode, this a map? by city? ...
# puma geo, city, county less MPLS & StP
# Maps of origin.
mode_share_origin <- linked_trips_19_mpo %>%
  filter(!is.na(mode_group_2) & trip_weight > 0) %>%
  srvyr::as_survey_design(w = trip_weight, strata = sample_segment) %>%
  group_by(trip_o_city, mode_group_2) %>%
  dplyr::summarize(
    pct = survey_prop(),
    ct = survey_total(),
    n = n()
  ) %>%
  mutate(year = "2021") %>%
  as.data.table()

linked_trips_19_mpo[
  factor(mode_share_origin, levels = mode_share_origin[mode_group_2 == "Drive"][order(pct), trip_o_city], ordered = T)
]

plot_ly(data = mode_share_origin
        , x=~trip_o_city
        , y=~pct
        , color=~ mode_group_2
        , colors = 'Spectral'
        , type = 'bar'
) %>%
  layout(
    barmode = "stack"
    , xaxis = list(categoryorder = "total decending")
  )

# Trip purpose x Trip Origin --------

# Origins of transit trips ----

# Average trip length ----
# Auto ownership by community designation ----
# Telecommuting, other trip replacement behaviors ----
# Congestion - time in car delay ----
# Congestion - fuel costs ----

# Distance vs congestion- What portion of congestion (by community type) is "distance" vs "delay" ----
# Who is experiencing congestion? how is it distributed?
# congestion per mile. Is it a long trip that exposes them to congestion, or
# is there congestion where they live?

# Who is experiencing delay?




# __ Parking Lot -------
# align mode_type_detailed levels
# tbi19$trip <- tbi19 %>%
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













