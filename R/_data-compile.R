# Get TBI survey data from database ---------
source("data-raw/get-survey-data.R")

# Trim columns down for manageability ----------
source("data-raw/slim-survey-data-columns.R")

# Trim survey data to MPO region -----------
source("data-raw/trim-survey-data-to-mpo.R")

# Get EPA and Vehicle Weight Data:
source("data-raw/get-epa-vehicle-efficiency-data.R")

source("data-raw/get-dps-vehicle-weight-data.R")


# Append Thrive Category (can aggregate to other geographies here)
source("data-raw/add-thrive-to-hh-trip.R")

# Append MPO boundary to trips
source("data-raw/add-mpo-boundary-to-trips.R")


# check for any PII and remove
# for vehicle table, remove make, model, year and name, and then round the numbers from DPS/EPA


veh_wt_tally <- veh %>%
  mutate(veh_age = 2019 - year) %>%
  left_join(hh %>% select(hh_id, hh_weight)) %>%
  group_by(make, model) %>%
  summarize(raw_n = length(veh_id), ave_year = round(mean(year)), weighted_n = sum(hh_weight)) %>%
  filter(!make == "Other") %>%
  arrange(desc(weighted_n)) %>%
  left_join(veh %>% select(make, model, year, co2_gpm, mpg_city, mpg_highway, weight_unladen) %>% unique(),
            by = c('make' = 'make', 'model' = 'model', 'ave_year' = 'year'))

veh_wt_tally_thrive <- veh %>%
  mutate(veh_age = 2019 - year) %>%
  left_join(hh %>% select(hh_id, hh_weight, thriveCatBroader)) %>%
  group_by(thriveCatBroader, make, model) %>%
  summarize(raw_n_xthrive = length(veh_id), ave_year_xthrive = round(mean(year)), weighted_n_xthrive = sum(hh_weight)) %>%
  filter(!make == "Other") %>%
  filter(!make == "Motorcycle") %>%
  arrange(thriveCatBroader, desc(weighted_n_xthrive)) %>%
  left_join(veh %>% select(make, model, year, co2_gpm, mpg_city, mpg_highway, weight_unladen) %>% unique(),
            by = c('make' = 'make', 'model' = 'model', 'ave_year_xthrive' = 'year'))

write.csv(veh_wt_tally, 'data/vehicle_tally_weighted.csv', row.names = F)
write.csv(veh_wt_tally_thrive, 'data/vehicle_tally_weighted_bythrive.csv', row.names = F)

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
  )
hh <-
  hh %>%
  select(-home_lat, -home_lon)

trip <-
  trip %>%
  select(-o_lat, -o_lon, -d_lat, -d_lon)


per <- per %>%
  select(-ethnicity_other_specify)


# Write data -------------------
tbi <- list(
  "day" = day,
  "per" = per,
  "hh" = hh,
  "veh" = veh,
  "trip" = trip
)

saveRDS(tbi, "data/tbi_extract.RData")
write.csv(dictionary, "data/metadata_table.csv", row.names = F)

rm(hh, per, trip, veh, day)
