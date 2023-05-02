veh_occ_income_thrive <-
  tbi19$trip %>%
  # driving trip only:
  filter(mode_type %in% c("Household vehicle", "Other vehicle")) %>%
  # get number of people traveling (character column, dangit):
  mutate(num_travelers = gsub(" people| person", "", num_travelers)) %>%
  mutate(num_travelers = ifelse(num_travelers %in% c("Missing: Non-response", "Missing: Skip logic"), NA, num_travelers)) %>%
  mutate(num_travelers = as.numeric(as.character(num_travelers))) %>%
  filter(trip_weight > 0) %>%
  left_join(tbi19$hh) %>%
  # use survey weights to get average + SE vehicle occupancy:
  srvyr::as_survey_design(id = 1, weights = trip_weight) %>%
  group_by(thriveCatBroader, income_broad) %>%
  summarize(num_travelers = srvyr::survey_mean(num_travelers, na.rm = T))

message("new table: veh_occ_income_thrive, vehicle occupancy x income broad x thrive broader")
