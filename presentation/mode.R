source("presentation/_load_libraries.R")
source("presentation/_load_data.R")


trip %>% names() %>% str_subset("mode")

trip[
  , survey_year %>% unique() %>% sort %>% paste0(collapse = "_")
  , keyby = .(mode_type_detailed)
] %>%
  .[, paste0('"', mode_type_detailed, '" = , #', V1) %>% unique() %>% paste(collapse = '\n') %>% cat]

mapping <- c(
  "ATV or snowmobile" = "Other", #2019_2021_2023
  "Airplane/helicopter" = "Air Travel", #2019_2021_2023
  "Bike-share - electric bicycle" = "Bike/Scooter", #2019_2021_2023
  "Bike-share - standard bicycle" = "Bike/Scooter", #2019_2021_2023
  "Boat/ferry" = "Other", #2019_2021_2023
  "Borrowed bicycle (e.g., a friend's)" = "Bike/Scooter", #2019_2021_2023
  "Bus rapid transit (e.g., A Line, C Line, Red Line)" = "Transit", #2019_2021_2023
  "Car rental" = "Vehicle", #2019_2021_2023
  "Carpool match (e.g., Waze Carpool)" = "Vehicle", #2019_2021_2023
  "Carshare service (e.g., Zipcar)" = "Vehicle", #2019_2021_2023
  "Electric bicycle (my household's)" = "Bike/Scooter", #2021_2023
  "Electric vehicle carshare (e.g., Evie)" = "Vehicle", #2023
  "Employer-provided shuttle/bus" = "Other", #2019_2021_2023
  "Express/commuter bus" = "Transit", #2019_2021_2023
  "Friend's vehicle" = "Vehicle", #2019_2021_2023
  "Golf cart" = "Other", #2019_2021_2023
  "Household vehicle 1" = "Vehicle", #2019_2021_2023
  "Household vehicle 2" = "Vehicle", #2019_2021_2023
  "Household vehicle 3" = "Vehicle", #2019_2021_2023
  "Household vehicle 4" = "Vehicle", #2019_2021_2023
  "Household vehicle 5" = "Vehicle", #2019_2021_2023
  "Household vehicle 6" = "Vehicle", #2019_2021_2023
  "Household vehicle 7" = "Vehicle", #2019_2023
  "Household vehicle 8" = "Vehicle", #2019_2021_2023
  "Intercity bus (e.g., BoltBus, Greyhound)" = "Transit", #2019_2021
  "Intercity rail (e.g., Amtrak)" = "Transit", #2019_2021_2023
  "Light rail" = "Transit", #2019_2021_2023
  "Local fixed-route bus" = "Transit", #2019_2021_2023
  "Lyft Line, Uberpool, or other shared-ride" = "Vehicle", #2019
  "Medical transportation service" = "Other", #2019_2021_2023
  "Metro Mobility" = "Other", #2019_2021_2023
  "Missing" = "Missing", #2019_2021_2023
  "Moped-share (e.g., Scoot)" = "Bike/Scooter", #2019_2021
  "Northstar" = "Transit", #2019_2021_2023
  "Other" = "Other", #2019_2021_2023
  "Other boat (e.g., kayak)" = "Other", #2021_2023
  "Other bus" = "Other", #2019_2021_2023
  "Other car" = "Vehicle", #2019_2021_2023
  "Other hired car service (e.g., black car, limo)" = "Vehicle", #2019_2021_2023
  "Other motorcycle" = "Vehicle", #2019_2021_2023
  "Other private shuttle/bus (e.g., Bellair Charters, Airporter Shuttle)" = "Other", #2019_2021_2023
  "Other rail" = "Other", #2019_2021_2023
  "Other rented bicycle" = "Bike/Scooter", #2019_2021_2023
  "Other scooter or moped" = "Bike/Scooter", #2019_2021
  "Other vehicle in household" = "Vehicle", #2019_2021_2023
  "Paratransit/Dial-A-Ride" = "Other", #2019_2021
  "Peer-to-peer car rental (e.g., Turo)" = "Vehicle", #2019
  "Personal scooter or moped (not shared)" = "Bike/Scooter", #2019_2021_2023
  "Regular taxi (e.g., Yellow Cab)" = "Vehicle", #2019_2021_2023
  "School bus" = 'School Bus', #2019_2021_2023
  "Scooter-share (e.g., Bird, Lime)" = "Bike/Scooter", #2019_2021_2023
  "Skateboard or rollerblade" = "Other", #2019_2021_2023
  "SouthWest Prime or MVTA Connect" = "Other", #2021_2023
  "Standard bicycle (my household's)" = "Bike/Scooter", #2019_2021
  "Uber, Lyft, or other smartphone-app ride service" = "Vehicle", #2019_2021_2023
  "University/college shuttle/bus" = "Other", #2019_2021_2023
  "Vanpool" = "Other", #2019_2021_2023
  "Vehicle ferry (took vehicle on board)" = "Other", #2021_2023
  "Walk (or jog/wheelchair)" = "Walk", #2019_2021_2023
  "Work vehicle" = "Vehicle" #2019_2021_2023
)
trip[, mode_recat := mapping[mode_type_detailed]]

trip[
  , .(Trips = sum(trip_weight, na.rm = TRUE), .N)
  , .(survey_year, mode_recat)
] %>%
  .[, pct := Trips/sum(Trips), survey_year] %>%
  .[mode_recat != 'Vehicle'] %>%
  print() %>%
  plot_ly() %>%
  add_bars(
    x = ~ reorder(mode_recat, -pct)
    , y = ~pct
    , color = ~survey_year
    , colors = c(colors$councilBlue, colors$esBlue)
  ) %>%
  layout(
    title = "Other Mode",
    # yaxis = list(title = 'Survey/Max(Surveys)'),
    yaxis = list(title = "Percent of Trips", tickformat = ".0%"),
    xaxis = list(title = "")
    , font = list(size = 16)
    , margin = list(t = 60)
  ) %>%
  print %>%
  save_image("output/mode_nonVeh.svg", width =700, height = 500)

trip[
  , .(Trips = sum(trip_weight, na.rm = TRUE), .N)
  , .(survey_year
      , mode_recat = fifelse(mode_recat == "Vehicle", "Vehicle", "Other Mode"))
] %>%
  .[, pct := Trips/sum(Trips), survey_year] %>%
  print() %>%
  plot_ly() %>%
  add_bars(
    x=~ reorder(mode_recat, -pct)
    , y=~pct
    , color=~survey_year
    , colors = c(colors$councilBlue, colors$esBlue)
  ) %>%
  layout(
    # yaxis = list(title = 'Survey/Max(Surveys)'),
    yaxis = list(title = "Percent of Trips", tickformat = ".0%"),
    xaxis = list(title = "")
    , font = list(size = 16)
  ) %>%
  print %>%
  save_image("output/mode_veh.svg", width = 500, height = 500)


# speed --------

trip[
  # mode_type %>% str_detect("Vehicle")
  !mode_type %in% c("Long distance passenger mode", "Missing")
  , .(speed = median(speed_mph, na.rm = TRUE), .N)
  , .(survey_year, mode_type)
] %>%
  .[, N := sum(N), mode_type] %>%
  .[N > 2000] %>%
  print() %>%
  plot_ly() %>%
  add_bars(
    x=~ reorder(mode_type, -speed)
    , y=~speed
    , color=~survey_year
    , colors = c(colors$councilBlue, colors$esBlue)
  ) %>%
  layout(
    # yaxis = list(title = 'Survey/Max(Surveys)'),
    yaxis = list(title = 'Miles per Hour'),
    xaxis = list(title = "")
    , font = list(size = 16)
  ) %>%
  print %>%
  save_image("output/speed.svg", width = 800, height = 500)
