# check for any PII and remove
# for vehicle table, remove make, model, year and name, and then round the numbers from DPS/EPA
veh <- veh %>%
  select(-make, -model, -vehicle_name, -class_vehicle) %>%
  select(-epa_tbi_veh_match_notes, -dps_tbi_veh_match_notes) %>%
  mutate(veh_age = 2019 - year) %>%
  select(-year) %>%
  mutate(
    co2_gpm = round(co2_gpm, -1),
    mpg_city = round(mpg_city, 0),
    mpg_highway = round(mpg_highway, 0),
    weight_unladen = round(weight_unladen, -2)
  ) %>%
  left_join(hh %>% select(hh_id, hh_weight))

hh <-
  hh %>%
  select(-home_lat, -home_lon)

trip <-
  trip %>%
  select(-o_lat, -o_lon, -d_lat, -d_lon)


per <- per %>%
  select(-ethnicity_other_specify)
