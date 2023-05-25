#### (1) Find trips that were completed ONLY by car ----------
n_modes_pertrip <-
  trip %>%
  # count the number of modes per trip
  # first, "Missing Skip logic" is NA
  mutate(across(
    c(mode_1, mode_2, mode_3),
    ~ ifelse(. == "Missing: Skip logic", NA, as.character(.))
  )) %>%
  select(trip_id, mode_1, mode_2, mode_3) %>%
  pivot_longer(
    cols = c(mode_1, mode_2, mode_3),
    names_to = "mode_num",
    values_to = "modes"
  ) %>%
  # get rid of trips where mode is "NA" (it switched to character at some point, so it's not NA without quotes...weird)
  filter(!modes == "NA") %>%
  # here is where the counting happens:
  group_by(trip_id) %>%
  # great function! adds a column called "n"
  add_tally() %>%
  # renaming tally column
  rename(n_modes = n)


#### (3) Vehicle Stops ----------
veh_stops <-
  trip %>%
  filter(vehicle_driver == "Driver") %>%
  # only want the single-mode trips
  left_join(n_modes_pertrip) %>%
  filter(n_modes == 1) %>%
  # filter to vehicle trips in a household vehicle only:
  filter(grepl(pattern = "Household vehicle", x = mode_type_detailed)) %>%
  # create new vehicle ID:
  mutate(veh_id = str_replace(
    mode_type_detailed,
    pattern = "Household vehicle ",
    replacement = paste(hh_id, "_", sep = "")
  )) %>%
  # select relevant columns:
  select(
    veh_id, trip_id, trip_weight,
    depart_date_imputed, depart_time_imputed,
    arrive_date, arrive_time,
    o_purpose_category_imputed,
    d_purpose_category_imputed, d_purpose_imputed
  ) %>%
  # Create a DateTime column from separate date and time columns:
  mutate(
    depart_datetime_imputed = as.POSIXct(depart_date_imputed, time = depart_time_imputed),
    arrive_datetime = as.POSIXct(arrive_date, time = arrive_time)
  ) %>%
  select(-depart_date_imputed, -depart_time_imputed, -arrive_date, -arrive_time) %>%
  # sometimes multiple people reported themselves as the driver...
  arrange(veh_id, depart_datetime_imputed, trip_id) %>%
  group_by(veh_id, depart_datetime_imputed) %>%
  slice_head(n = 1) %>% # should select the lowest TRIP ID number (usually the adult in the household)
  unique() %>%
  ungroup() %>%
  # find instances where vehicles have no stop and remove them
  group_by(veh_id) %>%
  add_tally() %>%
  ungroup() %>%
  # needs to have at least two rows to calculate a stop time
  filter(n >= 2) %>%
  # now calculate the idle time - group by vehicle
  arrange(veh_id, depart_datetime_imputed) %>%
  group_by(veh_id) %>%
  mutate(idleTime = lead(depart_datetime_imputed) - arrive_datetime) %>%
  ungroup() %>%
  # get rid of the last stop for each vehicle (NA idletime)
  filter(!is.na(idleTime)) %>%
  # difftime is automatically in seconds - transform to minutes:
  mutate(idleTime = as.numeric(idleTime, units = "mins")) %>%
  # select relevant columns:
  select(veh_id, trip_id, trip_weight, d_purpose_category_imputed, d_purpose_imputed, idleTime)

# vehicle with negative idletimes: 18115411_1
rm(n_modes_pertrip)
