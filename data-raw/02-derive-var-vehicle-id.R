# 2019 -------
veh19 <-
  veh19 %>% mutate(veh_id = paste(hh_id, "_", vehicle_num, sep = ""))

trip19 <- trip19 %>%
  mutate(veh_id = case_when(
    grepl(pattern = "Household vehicle", x = mode_type_detailed) ~
      paste(hh_id, str_extract(mode_type_detailed, "[0-9]+"), sep = "_")
  )) %>%
  mutate(veh_id = case_when(
    grepl(pattern = "Household vehicle", x = mode_type_detailed) ~ veh_id,
    mode_type_detailed %in%
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
      "Other Vehicle",
    TRUE ~ veh_id
  ))

# 2021 ----
## Mode type detailed for 2021 dataset ----
veh21 <-
  veh21 %>% mutate(veh_id = paste(hh_id, "_", str_extract(vehicle_num, "[0-9]+"), sep = ""))

mode_type_hierarchy <- read.csv("data-raw/mode_type_hierarchy.csv")

mode_type_detailed21 <-
  trip21 %>%
  select(trip_id, mode_1, mode_2, mode_3) %>%
  pivot_longer(-trip_id, values_to = "mode_type_detailed") %>%
  select(-name) %>%
  filter(!is.na(mode_type_detailed)) %>%
  unique() %>%
  left_join(mode_type_hierarchy, by = "mode_type_detailed") %>%
  group_by(trip_id) %>%
  summarize(mode_type_value = max(mode_type_value)) %>%
  ungroup() %>%
  left_join(mode_type_hierarchy, by = "mode_type_value") %>%
  select(-mode_type_value)

trip21 <- trip21 %>%
  left_join(mode_type_detailed21, by = "trip_id") %>%
  mutate(veh_id = case_when(
    grepl(pattern = "Household vehicle", x = mode_type_detailed) ~
      paste(hh_id, str_extract(mode_type_detailed, "[0-9]+"), sep = "_")
  )) %>%
  mutate(veh_id = case_when(
    grepl(pattern = "Household vehicle", x = mode_type_detailed) ~ veh_id,
    mode_type_detailed %in%
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
      "Other Vehicle",
    TRUE ~ veh_id
  ))
