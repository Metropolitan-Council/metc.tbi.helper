### Add vehicle ID -------
veh <-
  veh %>% mutate(veh_id = paste(hh_id, "_", vehicle_num, sep = ""))

trip <- trip %>%
  mutate(veh_id = case_when(
    grepl(pattern = "Household vehicle", x = mode_type_detailed) ~ mode_type_detailed
  )) %>%
  mutate(veh_id = str_replace(
    veh_id,
    pattern = "Household vehicle ",
    replacement = paste(hh_id, "_", sep = "")
  )) %>%
  mutate(veh_id = case_when(mode_type_detailed %in%
    c(
      "Other vehicle in household",
      "Other motorcycle",
      "Car from work",
      "Friend/relative/colleague's car",
      "Rental car",
      "Carpool match (e.g., Waze Carpool)",
      "Carshare service (e.g., HOURCAR, Car2Go, Zipcar, Maven)",
      "Peer-to-peer car rental (e.g., Turo, Getaround)",
      "Other vehicle"
    ) ~
    "Other Vehicle"))
