# check for any PII and remove
# for vehicle table, remove make, model, year and name, and then round the numbers from DPS/EPA
veh19 <- veh19 %>%
  select(-make, -model, -vehicle_name, -class_vehicle) %>%
  select(-epa_tbi_veh_match_notes, -dps_tbi_veh_match_notes) %>%
  mutate(veh_age = case_when(!year == "1980 or earlier" ~ 2019 - as.numeric(year))) %>%
  mutate(
    co2_gpm = round(co2_gpm, -1),
    mpg_city = round(mpg_city, 0),
    mpg_highway = round(mpg_highway, 0),
    weight_unladen = round(weight_unladen, -2)
  ) %>%
  left_join(hh19 %>% select(hh_id, hh_weight), by = "hh_id")

hh19 <-
  hh19 %>%
  select(-home_lat, -home_lon)

trip19 <-
  trip19 %>%
  select(-o_lat, -o_lon, -d_lat, -d_lon)


per19 <- per19 %>%
  select(-ethnicity_other_specify)

# 2021 -----
# check for any PII and remove
# for vehicle table, remove make, model, year and name, and then round the numbers from DPS/EPA
veh21 <- veh21 %>%
  select(-make, -model, -class_vehicle) %>%
  select(-epa_tbi_veh_match_notes, -dps_tbi_veh_match_notes) %>%
  mutate(veh_age = case_when(!year == "1980 or earlier" ~ 2021 - as.numeric(year))) %>%
  mutate(
    co2_gpm = round(co2_gpm, -1),
    mpg_city = round(mpg_city, 0),
    mpg_highway = round(mpg_highway, 0),
    weight_unladen = round(weight_unladen, -2)
  ) %>%
  left_join(hh21 %>% select(hh_id, hh_weight), by = "hh_id")

hh21 <-
  hh21 %>%
  select(-home_lat, -home_lon)

trip21 <-
  trip21 %>%
  select(-o_lat, -o_lon, -d_lat, -d_lon) %>%
  select(-mode_other_comment) %>%
  select(-d_purpose_other)

per21 <- per21 %>%
  select(-ethnicity_other_specify) %>%
  select(-race_black_african_other, -race_asian_other, -race_hispanic_other, -language_at_home_other) %>%
  select(-ev_typical_charge_other)
