source("presentation/_load_libraries.R")
if(!exists("tbi")) source("presentation/_load_data.R")

# tbi$linked_trip[
#   , survey_year %>% unique() %>% sort %>% paste0(collapse = "_")
#   , keyby = .(mode_type_detailed)
# ] %>%
#   .[, paste0('"', mode_type_detailed, '" = , #', V1) %>% unique() %>% paste(collapse = '\n') %>% cat]

mapping <- c(
  "Other" = "Other", #2019_2021_2023
  "Other boat (e.g., kayak)" = "Other", #2021_2023
  "Other bus" = "Other", #2019_2021_2023
  "Other private shuttle/bus (e.g., Bellair Charters, Airporter Shuttle)" = "Other", #2019_2021_2023
  "Other rail" = "Other", #2019_2021_2023
  "Paratransit/Dial-A-Ride" = "Other", #2019_2021
  "Skateboard or rollerblade" = "Other", #2019_2021_2023
  "SouthWest Prime or MVTA Connect" = "Other", #2021_2023
  "University/college shuttle/bus" = "Other", #2019_2021_2023
  "Vanpool" = "Other", #2019_2021_2023
  "Vehicle ferry (took vehicle on board)" = "Other", #2021_2023
  "ATV or snowmobile" = "Other", #2019_2021_2023
  "Boat/ferry" = "Other", #2019_2021_2023
  "Employer-provided shuttle/bus" = "Other", #2019_2021_2023
  "Golf cart" = "Other", #2019_2021_2023
  "Medical transportation service" = "Other", #2019_2021_2023
  "Metro Mobility" = "Other", #2019_2021_2023
  "Airplane/helicopter" = "Other", #2019_2021_2023

  "School bus" = 'School Bus', #2019_2021_2023

  "Bike-share - electric bicycle" = "Bike/Scooter", #2019_2021_2023
  "Other rented bicycle" = "Bike/Scooter", #2019_2021_2023
  "Other scooter or moped" = "Bike/Scooter", #2019_2021
  "Personal scooter or moped (not shared)" = "Bike/Scooter", #2019_2021_2023
  "Scooter-share (e.g., Bird, Lime)" = "Bike/Scooter", #2019_2021_2023
  "Standard bicycle (my household's)" = "Bike/Scooter", #2019_2021
  "Bike-share - standard bicycle" = "Bike/Scooter", #2019_2021_2023
  "Borrowed bicycle (e.g., a friend's)" = "Bike/Scooter", #2019_2021_2023
  "Electric bicycle (my household's)" = "Bike/Scooter", #2021_2023
  "Moped-share (e.g., Scoot)" = "Bike/Scooter", #2019_2021

  "Bus rapid transit (e.g., A Line, C Line, Red Line)" = "Transit", #2019_2021_2023
  "Intercity bus (e.g., BoltBus, Greyhound)" = "Transit", #2019_2021
  "Intercity rail (e.g., Amtrak)" = "Transit", #2019_2021_2023
  "Light rail" = "Transit", #2019_2021_2023
  "Local fixed-route bus" = "Transit", #2019_2021_2023
  "Northstar" = "Transit", #2019_2021_2023

  "Car rental" = "Vehicle", #2019_2021_2023
  "Carpool match (e.g., Waze Carpool)" = "Vehicle", #2019_2021_2023
  "Carshare service (e.g., Zipcar)" = "Vehicle", #2019_2021_2023
  "Electric vehicle carshare (e.g., Evie)" = "Vehicle", #2023
  "Express/commuter bus" = "Transit", #2019_2021_2023
  "Friend's vehicle" = "Vehicle", #2019_2021_2023
  "Household vehicle 1" = "Vehicle", #2019_2021_2023
  "Household vehicle 2" = "Vehicle", #2019_2021_2023
  "Household vehicle 3" = "Vehicle", #2019_2021_2023
  "Household vehicle 4" = "Vehicle", #2019_2021_2023
  "Household vehicle 5" = "Vehicle", #2019_2021_2023
  "Household vehicle 6" = "Vehicle", #2019_2021_2023
  "Household vehicle 7" = "Vehicle", #2019_2023
  "Household vehicle 8" = "Vehicle", #2019_2021_2023
  "Other car" = "Vehicle", #2019_2021_2023
  "Other hired car service (e.g., black car, limo)" = "Vehicle", #2019_2021_2023
  "Other motorcycle" = "Vehicle", #2019_2021_2023
  "Other vehicle in household" = "Vehicle", #2019_2021_2023
  "Peer-to-peer car rental (e.g., Turo)" = "Vehicle", #2019
  "Regular taxi (e.g., Yellow Cab)" = "Vehicle", #2019_2021_2023
  "Work vehicle" = "Vehicle", #2019_2021_2023

  "Lyft Line, Uberpool, or other shared-ride" = "TNC", #2019
  "Uber, Lyft, or other smartphone-app ride service" = "TNC", #2019_2021_2023

  "Missing" = "Missing", #2019_2021_2023

  "Walk (or jog/wheelchair)" = "Walk" #2019_2021_2023
)

tbi$linked_trip[, mode_recat := mapping[as.character(mode_type_detailed)]]
tbi$linked_trip <- tbi$linked_trip[mode_recat != "Missing"]
tbi$linked_trip[, mode_recat_2 := fifelse(mode_recat == "Vehicle", "Vehicle", "Other Modes")]

trips1 <-
  tbi$linked_trip %>%
  left_join(select(tbi$hh, hh_id, sample_segment, hh_in_mpo), by = "hh_id") %>%
  filter(!is.na(trip_weight) & trip_weight > 0) %>%
  filter(hh_in_mpo == T)

# mode x year w/o veh--------------
mode_year <- trips1 %>%
  as_survey_design(
    ids = linked_trip_id,
    weights = trip_weight,
    strata = sample_segment
  ) %>%
  group_by(survey_year, mode_recat) %>%
  summarize(
    N = n(),
    wtd_N = sum(trip_weight) %>% round,
    wtd_prop = survey_prop(vartype = "ci", proportion = TRUE)
  ) %>%
  as.data.table() %>%
  .[mode_recat != "Vehicle"] %>%
  print

mode_year[mode_recat == "School Bus"]


mode_year %>%
  .[, survey_year := fct_rev(survey_year)] %>%
plot_ly() %>%
  add_bars(
    y =~ wtd_prop,
    color =~ survey_year,
    x =~ fct_reorder(mode_recat, wtd_prop)
    # , error_y =~ list(symmetric = T, value = 5)
  ) %>%
  councilR::plotly_layout(
    main_title = "Title",
    x_title = "Mode",
    y_title = "Proportion of Trips"
  ) %>%
  layout(
    barmode = 'group'
    , yaxis = list(tickformat = ".0%")
    , font = list(size = 16)
    , legend = list(traceorder = "reverse")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/mode_share_wo_veh.svg", width = 700, height = 400)

# mode x year w/ veh--------------
mode_year_veh <-
  trips1 %>%
  as_survey_design(
    ids = linked_trip_id,
    weights = trip_weight,
    strata = sample_segment
  ) %>%
  group_by(survey_year, mode_recat_2) %>%
  summarize(
    N = n(),
    wtd_N = sum(trip_weight),
    wtd_prop = survey_prop(vartype = "se", proportion = TRUE)
  ) %>%
  as.data.table() %>%
  print

plot_ly(data = mode_year_veh) %>%
  add_bars(
    y =~ wtd_prop,
    color =~ survey_year,
    x =~ fct_reorder(mode_recat_2, wtd_prop)
  ) %>%
  councilR::plotly_layout(
    main_title = "Title",
    x_title = "Mode",
    y_title = "Proportion of Trips"
  ) %>%
  layout(
         yaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/mode_share_veh.svg", width = 500, height = 400)

# mode x income x 2023 --------------
mode_income <-
  trips1 %>%
  left_join(select(tbi$hh, hh_id, income_broad)) %>%
  filter(survey_year == 2023) %>%
  as_survey_design(
    ids = linked_trip_id,
    weights = trip_weight,
    strata = sample_segment
  ) %>%
  group_by(mode_recat, income_broad) %>%
  summarize(
    N = n(),
    wtd_N = survey_total() %>% round,
    wtd_prop = survey_prop(vartype = "ci", proportion = TRUE) %>% round(3)
  ) %>%
  as.data.table() %>%
  print

catorder <- mode_income[income_broad == "<$25K"][order(wtd_prop), mode_recat]
mode_income %>%
  # filter(mode_recat != "School Bus") %>%
  plot_ly() %>%
  add_bars(
    x =~ wtd_prop,
    color =~ income_broad %>% fct_rev(),
    y =~ mode_recat %>% factor(levels = catorder, ordered = T)
  ) %>%
  councilR::plotly_layout(
    main_title = "Title",
    x_title = "Mode",
    y_title = "Proportion of Trips",
    legend_title = "Household Income"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
         ) %>%
  print %>%
  save_image("output/mode_share_income.svg", width = 800, height = 400)

# mode x race x 2023 --------------
mode_race <-
  trips1 %>%
  left_join(select(tbi$person, person_id, race_ethnicity)) %>%
  filter(survey_year == 2023) %>%
  as_survey_design(
    ids = linked_trip_id,
    weights = trip_weight,
    strata = sample_segment
  ) %>%
  group_by(mode_recat, race_ethnicity) %>%
  summarize(
    N = n(),
    wtd_N = survey_total() %>% round,
    wtd_prop = survey_prop(vartype = "ci", proportion = TRUE) %>% round(3)
  ) %>%
  as.data.table() %>%
  print

# catorder <- mode_race[income_broad == "<$25K"][order(wtd_prop), mode_recat]
mode_race %>%
  plot_ly() %>%
  add_bars(
    x =~ wtd_prop,
    color =~ race_ethnicity, # %>% fct_rev(),
    y =~ mode_recat %>% factor(levels = catorder, ordered = T)
  ) %>%
  councilR::plotly_layout(
    main_title = "Title",
    x_title = "Mode",
    y_title = "Proportion of Trips"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  )

# mode x age x 2023 --------------


# mode x hhsize x 2023 --------------













