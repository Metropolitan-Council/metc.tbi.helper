
# packages-------------------------------------------
packages <- list("bit64", "dplyr", "ggplot2", "plotly", "srvyr", "councilR", "sysfonts", "showtext", "purrr")
invisible(lapply(packages, library, character.only = TRUE))
rm(packages)

# load data-------------------------------------------
# source("data-raw/_data-compile.R") -- if you need to compile/refresh the dataset, start here.
# load("data/tbi_extract.RData") # start here if you have the RData object compiled and ready to go.


# data prep -------------------------------------------
##### 1. our universe: days lived by adults in the region -------------------------------------------
adults <- tbi_tables$per %>%
  filter(!age %in% c("Under 5", "5-15", "16-17")) %>%
  pluck("person_id")

adult_days <- tbi_tables$day %>%
  filter(day_weight > 0) %>%
  filter(person_id %in% adults)


##### 1. weekday trips by drivers -------------------------------------------
driver_trips <-
  tbi_tables$trip %>%
  filter(person_id %in% adults) %>%
  filter(trip_weight > 0) %>%
  filter(vehicle_driver == "Driver")



##### 2. calculate gas consumed by drivers -------------------------------
# median mpg for all cars in TBI
median_mpg <- tbi_tables$veh %>%
  left_join(tbi_tables$hh %>% select(hh_id, hh_weight)) %>%
  as_survey_design(w = hh_weight) %>%
  summarize(
    median_mpg_city = survey_median(mpg_city, na.rm = T),
    median_mpg_highway = survey_median(mpg_highway, na.rm = T),
    median_co2_gpm = survey_median(co2_gpm, na.rm = T)
  )

# how many missing mpg?
driver_trips %>%
  left_join(tbi_tables$veh, by = c("hh_id", "veh_id")) %>%
  filter(is.na(mpg_city)) %>%
  nrow()

nrow(driver_trips)

gal_per_trip <-
  driver_trips %>%
  left_join(tbi_tables$veh, by = c("hh_id", "veh_id")) %>%
  # apply median co2 emissions value to vehicles with missing information (probably an under-estimate).
  # only do this for non-electric cars
  mutate(
    mpg_highway = case_when(
      is.na(mpg_highway) ~ median_mpg$median_mpg_highway,
      TRUE ~ mpg_highway
    ),
    mpg_city = case_when(
      is.na(mpg_city) ~ median_mpg$median_mpg_city,
      TRUE ~ mpg_city
    )
  ) %>%
  # apply Highway MPG when speeds > 45mph; city MPG when speeds < 45 mph
  mutate(mpg = case_when(
    speed_mph_imputed >= 45 ~ mpg_highway,
    speed_mph_imputed < 45 ~ mpg_city,
    is.na(speed_mph_imputed) ~ mpg_city
  )) %>%
  mutate(gal_consumed = (1 / mpg) * distance) %>%
  mutate(gal_consumed = case_when(
    epa_fuel_type == "Electricity" ~ 0,
    TRUE ~ gal_consumed
  )) %>%
  filter(gal_consumed > 0)


##### 5. #, % adults who burned gas, traveled by other modes -------------------------------------------
gal_per_person_day <-
  adult_days %>%
  left_join(gal_per_trip) %>%
  # those that don't drive or travel that day --
  group_by(person_id, day_num, day_weight, num_trips) %>%
  summarize(
    gal_consumed_day = sum(gal_consumed, na.rm = T),
    driving_distance = sum(distance, na.rm = T)
  ) %>%
  ungroup()


gal_per_hh_day <-
  tbi_tables$day %>%
  filter(day_weight > 0) %>%
  left_join(gal_per_trip) %>%
  group_by(hh_id, day_num) %>%
  summarize(
    gal_consumed_day = sum(gal_consumed, na.rm = T),
    driving_distance = sum(distance, na.rm = T),
    # will find head of hh's day weight
    head_of_hh = min(person_id),
    num_trips = max(num_trips, na.rm = T),
  ) %>%
  ungroup() %>%
  left_join(tbi_tables$day %>% select(person_id, day_num, day_weight),
    by = c("head_of_hh" = "person_id", "day_num" = "day_num")
  )


# a global (numeric) average of gallons used -----------------
gal_per_person_average <-
  gal_per_person_day %>%
  as_survey_design(w = day_weight) %>%
  summarize(mn_gal = survey_mean(gal_consumed_day))

gal_per_hh_average <-
  gal_per_hh_day %>%
  as_survey_design(w = day_weight) %>%
  summarize(mn_gal = survey_mean(gal_consumed_day))


##### 4. #, % adults who rode as passengers -------------------------------------------
# rode as passenger
passenger_trips <-
  tbi_tables$trip %>%
  filter(person_id %in% adults) %>%
  filter(mode_type %in% c("Household vehicle", "For-hire vehicle", "Other vehicle", "Smartphone ridehailing service")) %>%
  filter(vehicle_driver == "Passenger" | is.na(vehicle_driver))

passenger_days <-
  passenger_trips %>%
  select(person_id, day_num) %>%
  unique() %>%
  mutate(rode_as_psgr = "rode as passenger")

passenger_hh_days <-
  passenger_trips %>%
  select(hh_id, day_num) %>%
  unique() %>%
  mutate(rode_as_psgr = "rode as passenger")

##### 4. #, % adults who used other modes -------------------------------------------
source("R/df-lump-mode-types.R")
other_mode_trips <-
  tbi_tables$trip %>%
  filter(!is.na(mode_type_cond)) %>%
  filter(!mode_type_cond == "Drive") %>%
  filter(trip_weight > 0) %>%
  filter(person_id %in% adults)

other_mode_days <-
  other_mode_trips %>%
  select(person_id, day_num) %>%
  unique() %>%
  mutate(used_other_modes = "used other modes")

other_mode_hh_days <-
  other_mode_trips %>%
  select(hh_id, day_num) %>%
  unique() %>%
  mutate(used_other_modes = "used other modes")

ev_trips <-
  tbi_tables$trip %>%
  left_join(tbi_tables$veh) %>%
  filter(epa_fuel_type == "Electricity") %>%
  filter(trip_weight > 0) %>%
  filter(person_id %in% adults)

ev_days <-
  ev_trips %>%
  select(person_id, day_num) %>%
  unique() %>%
  mutate(used_ev = "used EV")

ev_hh_days <-
  ev_trips %>%
  select(hh_id, day_num) %>%
  unique() %>%
  mutate(used_ev = "used EV")

# Bin gallon/person values ---------------------
gal_per_person_bins <-
  gal_per_person_day %>%
  left_join(passenger_days) %>%
  left_join(other_mode_days) %>%
  left_join(ev_days) %>%
  # filter(driving_distance < 720) %>%
  mutate(gal_bins = cut(
    gal_consumed_day,
    breaks = c(seq(
      from = -0.5, to = 3, by = 0.5
    ), Inf),
    labels = c(
      "0",
      "up to\n0.5",
      "0.5-1",
      "1-1.5",
      "1.5-2",
      "2-2.5",
      "2.5-3",
      "more than\n3"
    )
  )) %>%
  mutate(gal_bins = as.character(gal_bins)) %>%
  # add extra factor levels for zero gallons used --------------
  mutate(
    gal_bins = case_when(
      gal_bins == "0" & num_trips == 0 ~ "0\nStayed\nhome",
      gal_bins == "0" & rode_as_psgr == "rode as passenger" ~ "0\nRode as\npassgr.",
      gal_bins == "0" & used_other_modes == "used other modes" ~ "0\nUsed\nother\nmodes",
      gal_bins == "0" & used_ev == "used EV" ~ "0\nDrove\nan EV",
      TRUE ~ gal_bins
    )
  ) %>%
  # just a handful (7) of spurious records here, resulting from incomplete trip info:
  filter(!gal_bins == "0")

gal_per_hh_bins <-
  gal_per_hh_day %>%
  left_join(passenger_hh_days) %>%
  left_join(other_mode_hh_days) %>%
  left_join(ev_hh_days) %>%
  # filter(driving_distance < 720) %>%
  mutate(gal_bins = cut(
    gal_consumed_day,
    breaks = c(seq(
      from = -1, to = 5, by = 1
    ), Inf),
    labels = c(
      "0",
      "up to\n1",
      "1-2",
      "2-3",
      "3-4",
      "4-5",
      "more than\n5"
    )
  )) %>%
  mutate(gal_bins = as.character(gal_bins)) %>%
  # add extra factor levels for zero gallons used --------------
  mutate(
    gal_bins = case_when(
      gal_bins == "0" & num_trips == 0 ~ "0\nStayed\nhome",
      gal_bins == "0" & rode_as_psgr == "rode as passenger" ~ "0\nRode as\npassgr.",
      gal_bins == "0" & used_other_modes == "used other modes" ~ "0\nUsed\nother\nmodes",
      gal_bins == "0" & used_ev == "used EV" ~ "0\nDrove\nan EV",
      TRUE ~ gal_bins
    )
  ) %>%
  # just a handful (7) of spurious records here, resulting from incomplete trip info:
  filter(!gal_bins == "0")

# survey totals ----------------
gal_bins_summary <-
  gal_per_person_bins %>%
  # filter(num_trips > 0) %>%
  as_survey_design(weights = day_weight) %>%
  group_by(gal_bins) %>%
  summarize(
    n_adults = survey_total(),
    pct_adults = 100 * survey_prop()
  ) %>%
  ungroup() %>%
  mutate(consumed_gas = ifelse(gal_bins %in%
    c(
      "0\nStayed\nhome",
      "0\nRode as\npassgr.",
      "0\nUsed\nother\nmodes"
    ),
  "Those who did not drive:",
  "Those who drove that day:"
  ))


gal_bins_hh_summary <-
  gal_per_hh_bins %>%
  # filter(num_trips > 0) %>%
  as_survey_design(weights = day_weight) %>%
  group_by(gal_bins) %>%
  summarize(
    n_hh = survey_total(),
    pct_hh = 100 * survey_prop()
  ) %>%
  ungroup() %>%
  mutate(consumed_gas = ifelse(gal_bins %in%
    c(
      "0\nStayed\nhome",
      "0\nRode as\npassgr.",
      "0\nUsed\nother\nmodes"
    ),
  "Those who did not drive:",
  "Those who drove that day:"
  ))


# write to xlsx

datlist <- list(
  "gal_per_hh_day" = gal_per_hh_bins %>% mutate(took_trips = ifelse(num_trips > 0, 1, 0)) %>% select(-num_trips),
  "gal_per_hh_summary" = gal_bins_hh_summary,
  "gal_per_hh_avg" = gal_per_hh_average,
  "gal_per_person_day" = gal_per_person_bins,
  "gal_per_person_avg" = gal_per_person_average,
  "gal_per_person_summary" = gal_bins_summary,
  "vehicle_stats" = veh_stats
)

metadata <- data.frame(column_name = lapply(datlist, names) %>% unlist() %>% unique())

metadata <- metadata %>%
  mutate(
    description =
      recode_factor(column_name,
        "hh_id" = "Household ID",
        "day_num" = "Ordered day number of survey, for merging trips with the day table.",
        "gal_consumed_day" = "Gallons of gas consumed per day, based on trip data and EPA fuel efficiency data.",
        "driving_distance" = "Miles driven (as driver) per day",
        "head_of_hh" = "ID for the 'Head of the Household' -- person with the lowest person_id -- for household-day analyses",
        "day_weight" = "Day weight (at person level)",
        "rode_as_psgr" = "Did the person/Did anyone in the household ride in a car as a passenger on that day?",
        "used_other_modes" = "Did the person/Did anyone in the household use other non-driving modes on that day?",
        "used_ev" = "Did the person/Did anyone in the household use an Electric Vehicle on that day?",
        "gal_bins" = "Binned values of gallons per day",
        "took_trips" = "Did anyone in the household make trips on that day?",
        "consumed_gas" = "Did the person/household consume gas on that day?",
        "n_hh" = "Estimated number of households, based on survey weights",
        "n_hh_se" = "Standard error of the estimate",
        "pct_hh" = "Estimated percent of households, based on survey weights",
        "pct_hh_se" = "Standard error of the estimate",
        "mn_gal" = "Weighted average gallons consumed per day",
        "mn_gal_se" = "Standard error of the estimate",
        "person_id" = "Person ID",
        "num_trips" = "Number of trips made that day",
        "n_adults" = "Number of adults 18+ in this category, based on survey weights",
        "n_adults_se" = "Standard error of the estimate",
        "pct_adults" = "Percent of adults 18+ in this category, based on survey weights",
        "pct_adults_se" = "Standard error of the estimate",
        "hh_weight" = "Household weight",
        "income_broad" = "2019 Household Income, in broad categories",
        "thriveCatBroader" = "Broad geography categories for the household (urban, rural, suburban)",
        "weight_unladen" = "Empty weight of the vehicle, in pounds (Source: Minnesota DPS; matched to TBI vehicle make/model/year)",
        "veh_age" = "Age of vehicle in years",
        "mpg_highway" = "Miles per gallon (highway) for the vehicle (Source: EPA; matched to TBI vehicle make/model/year)"
      )
  )
