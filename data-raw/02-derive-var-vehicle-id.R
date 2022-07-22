# 2019 -------
veh19 <-
  veh19 %>% mutate(veh_id = paste0(hh_id, "_", vehicle_num))

trip19 <- trip19 %>%
  mutate(veh_id = case_when(
    grepl(pattern = "Household vehicle", x = mode_type_detailed) ~
      paste(hh_id, str_extract(mode_type_detailed, "[0-9]+"), sep = "_")
  ))

# 2021 ----
# veh21 <-
#   veh21 %>% mutate(veh_id = paste0(hh_id, "_",
#                                    str_extract(vehicle_num, "[0-9]+")))

## Mode type detailed for 2021 dataset ----
# mode_type_hierarchy <- read.csv("data-raw/mode_type_hierarchy.csv") %>%
#   mutate(mode_type_detailed = recode(mode_type_detailed,
#                                      "Standard bicycle" = "Standard bicycle (my household's)",
#                                      "Friend’s vehicle" = "Friend's vehicle",
#                                      "Bus rapid transit" = "Bus rapid transit (e.g., A Line, C Line, Red Line)",
#                                      "Electric bicycle" = "Electric bicycle (my household's)",
#                                      "Intercity bus" = "Intercity bus (e.g., BoltBus, Greyhound)",
#                                      "Other private shuttle/bus" = "Other private shuttle/bus (e.g., Bellair Charters, Airporter Shuttle)",
#                                      "Carshare service" = "Carshare service (e.g., Zipcar)",
#                                      "Borrowed bicycle" = "Borrowed bicycle (e.g., a friend's)",
#                                      "Paratransit/Dial-a-Ride" = "Paratransit/Dial-A-Ride",
#                                      "Intercity rail" = "Intercity rail (e.g., Amtrak)",
#                                      "Peer-to-peer car rental" = "Peer-to-peer car rental (e.g., Turo)",
#                                      "Other motorcycle (not my household’s)" = "Other motorcycle (not my household's)"))
#
# mode_type_detailed21 <-
#   trip21 %>%
#   select(trip_id, mode_1, mode_2, mode_3) %>%
#   pivot_longer(-trip_id, values_to = "mode_type_detailed") %>%
#   select(-name) %>%
#   filter(!is.na(mode_type_detailed)) %>%
#   unique() %>%
#   left_join(mode_type_hierarchy, by = "mode_type_detailed") %>%
#   group_by(trip_id) %>%
#   summarize(mode_type_value = max(mode_type_value)) %>%
#   ungroup() %>%
#   left_join(mode_type_hierarchy, by = "mode_type_value") %>%
#   select(-mode_type_value)
#
# trip21 <- trip21 %>%
#   left_join(mode_type_detailed21, by = "trip_id") %>%
#   mutate(veh_id = case_when(
#     grepl(pattern = "Household vehicle", x = mode_type_detailed) ~
#       paste(hh_id, str_extract(mode_type_detailed, "[0-9]+"), sep = "_")
#   ))
