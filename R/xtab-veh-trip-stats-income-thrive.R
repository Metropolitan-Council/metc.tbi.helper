

veh_stats <-
  tbi_tables$veh %>%
  left_join(tbi_tables$hh) %>%
  select(hh_id, hh_weight, income_broad, thriveCategory, thriveCatBroad, thriveCatBroader, weight_unladen, veh_age, mpg_highway, mpg_city)


veh_income_thrive <-
  veh_stats %>%
  filter(!income_broad == "Prefer not to answer" & !is.na(thriveCatBroader)) %>%
  # use survey weights to get average + SE vehicle occupancy:
  srvyr::as_survey_design(id = 1, weights = hh_weight) %>%
  group_by(thriveCatBroader, income_broad) %>%
  summarize(across(
    c(weight_unladen, veh_age, mpg_highway, mpg_city),
    ~ srvyr::survey_mean(., na.rm = T)
  ))


veh_thrive <-
  veh_stats %>%
  filter(!income_broad == "Prefer not to answer" & !is.na(thriveCategory)) %>%
  # use survey weights to get average + SE vehicle occupancy:
  srvyr::as_survey_design(id = 1, weights = hh_weight) %>%
  group_by(thriveCategory) %>%
  summarize(across(
    c(weight_unladen, veh_age, mpg_highway, mpg_city),
    ~ srvyr::survey_mean(., na.rm = T)
  ))

veh_thrive_trip <-
  tbi_tables$trip %>%
  filter(vehicle_driver == "Driver") %>%
  filter(grepl("vehicle|car", x = mode_type_detailed)) %>%
  select(trip_id, hh_id, trip_weight, veh_id) %>%
  left_join(tbi_tables$veh) %>%
  left_join(tbi_tables$hh) %>%
  select(
    trip_id,
    hh_id,
    trip_weight,
    income_broad,
    thriveCategory,
    thriveCatBroad,
    thriveCatBroader,
    weight_unladen,
    veh_age,
    mpg_highway,
    mpg_city
  )

veh_thrive_trip_summary <-
  veh_thrive_trip %>%
  filter(!is.na(thriveCatBroad)) %>%
  # use survey weights to get average + SE vehicle occupancy:
  srvyr::as_survey_design(id = 1, weights = trip_weight) %>%
  group_by(thriveCatBroad) %>%
  summarize(
    n = length(trip_id),
    n_hh = length(unique(hh_id)),
    across(
      c(weight_unladen, veh_age, mpg_highway, mpg_city),
      ~ srvyr::survey_mean(., na.rm = T)
    )
  )


message("new table: veh_income_thrive, vehicle stats weighted at hh level x income broad x thrive broader")
