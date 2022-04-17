### Linked Trip Table -----------
# Links trips created by more than one mode (e.g., bus and walk)

dictionary <- read.csv("data/metadata_table.csv")

missing_codes <- dictionary %>%
  filter(grepl("missing", value_label, ignore.case = T)) %>%
  select(value) %>%
  unique() %>%
  pull()



## Create a mode hierarchy -----------
get_hierarchy <- function(myvar) {
  value_col_name <- paste0(myvar, "_id")

  dictionary %>%
    filter(variable == !!myvar & !value %in% missing_codes) %>%
    select(value, value_label) %>%
    mutate(value = as.numeric(value)) %>%
    arrange(abs(value)) %>%
    mutate(value = 1:length(unique(value_label))) %>%
    rename(
      !!value_col_name := value,
      !!myvar := value_label
    )
}

mode_type_hierarchy <-
  get_hierarchy("mode_type") %>%
  # add bicycle as a mode separate from micro-mobility (scooters).
  rbind(cbind(mode_type_id = 9.5, mode_type = "Bicycle")) %>%
  mutate(mode_type_id = as.numeric(mode_type_id)) %>%
  arrange(abs(mode_type_id)) %>%
  mutate(mode_type_id = 1:length(unique(mode_type)))

mode_type_detailed_hierarchy <-
  get_hierarchy("mode_type_detailed")


#### destination purpose --------

d_purpose_hierarchy <-
  get_hierarchy("d_purpose_imputed") %>%
  # put change mode last:
  mutate(
    d_purpose_imputed_id = ifelse(
      d_purpose_imputed == "Change/transfer mode (e.g., wait for bus, change planes)", 100,
      d_purpose_imputed_id
    )
  ) %>%
  arrange(abs(d_purpose_imputed_id)) %>%
  mutate(d_purpose_imputed_id = 1:length(unique(d_purpose_imputed)))

o_purpose_hierarchy <-
  get_hierarchy("o_purpose_imputed") %>%
  # put change mode last:
  mutate(
    o_purpose_imputed_id = ifelse(
      o_purpose_imputed == "Change/transfer mode (e.g., wait for bus, change planes)", 100,
      o_purpose_imputed_id
    )
  ) %>%
  arrange(abs(o_purpose_imputed_id)) %>%
  mutate(o_purpose_imputed_id = 1:length(unique(o_purpose_imputed)))

d_purpose_category_hierarchy <-
  get_hierarchy("d_purpose_category_imputed") %>%
  # put change mode last:
  mutate(
    d_purpose_category_imputed_id = ifelse(
      d_purpose_category_imputed == "Change mode", 100,
      d_purpose_category_imputed_id
    )
  ) %>%
  arrange(abs(d_purpose_category_imputed_id)) %>%
  mutate(d_purpose_category_imputed_id = 1:length(unique(d_purpose_category_imputed)))


d_purpose_detailed_hierarchy <-
  get_hierarchy("d_purpose_category_imputed") %>%
  # put change mode last:
  mutate(
    d_purpose_category_imputed_id = ifelse(
      d_purpose_category_imputed == "Change mode", 100,
      d_purpose_category_imputed_id
    )
  ) %>%
  arrange(abs(d_purpose_category_imputed_id)) %>%
  mutate(d_purpose_category_imputed_id = 1:length(unique(d_purpose_category_imputed)))

#### origin purpose --------
o_purpose_category_hierarchy <-
  get_hierarchy("o_purpose_category_imputed") %>%
  # put change mode last:
  mutate(
    o_purpose_category_imputed_id = ifelse(
      o_purpose_category_imputed == "Change mode", 100,
      o_purpose_category_imputed_id
    )
  ) %>%
  arrange(abs(o_purpose_category_imputed_id)) %>%
  mutate(o_purpose_category_imputed_id = 1:length(unique(o_purpose_category_imputed)))







#### Link Trips, Select Highest Mode/Purpose ----------
trip_linked <- tbi$trip %>%
  arrange(trip_id) %>%
  # join to the hierarchies for mode and purpose:
  left_join(mode_type_hierarchy) %>%
  left_join(o_purpose_hierarchy) %>%
  left_join(d_purpose_hierarchy) %>%
  left_join(mode_type_detailed_hierarchy) %>%
  left_join(o_purpose_category_hierarchy) %>%
  left_join(d_purpose_category_hierarchy) %>%
  # new column: a trip chain (originpurpose -> destination purpose):
  mutate(
    trip_chain = paste0(o_purpose_category_imputed, "->", d_purpose_category_imputed),
    trip_chain_detail = paste0(o_purpose_imputed, "->", d_purpose_imputed)
  ) %>%
  mutate(leg_dur_hr = (1 / speed_mph_imputed) * distance) %>%
  # key for aggregation of linked trips -- person and trip_linked #
  group_by(person_id, linked_trip_num) %>%
  # summarize for each linked trip:
  summarize(
    n_links = length(trip_id),
    across(c(trip_weight), ~ max(., na.rm = T)),
    across(
      c(
        mode_type_id,
        mode_type_detailed_id,
        o_purpose_category_imputed_id,
        d_purpose_category_imputed_id,
        o_purpose_imputed_id,
        d_purpose_imputed_id
      ),
      ~ min(., na.rm = T)
    ),
    across(c(distance, duration_imputed), ~ sum(., na.rm = T)),
    speed_mph_imputed = sum(distance) / sum(leg_dur_hr),
    trip_id = paste0(trip_id, leg_num),
    across(
      c(
        num_travelers,
        trip_o_in_mpo,
        vehicle_driver,
        veh_id,
        hh_in_mpo,
        trip_in_mpo,
        o_thriveCategory,
        o_thriveCatBroad,
        o_thriveCatBroader,
        day_num,
        travel_date,
        depart_time_imputed
      ),
      ~ first(na.omit(.))
    ),
    across(
      c(
        trip_d_in_mpo,
        d_thriveCategory,
        d_thriveCatBroad,
        d_thriveCatBroader,
        arrive_time
      ),
      ~ last(na.omit(.))
    ),
  ) %>%
  ungroup() %>%
  # retrieve the mode types again:
  left_join(mode_type_hierarchy) %>%
  left_join(o_purpose_hierarchy) %>%
  left_join(d_purpose_hierarchy) %>%
  left_join(mode_type_detailed_hierarchy) %>%
  left_join(o_purpose_category_hierarchy) %>%
  left_join(d_purpose_category_hierarchy) %>%
  select(
    -o_purpose_category_imputed_id, -o_purpose_imputed_id,
    -d_purpose_category_imputed_id, -d_purpose_imputed_id,
    -mode_type_id, -mode_type_detailed_id
  )

# missing any trip attributes?
setdiff(names(tbi$trip), names(trip_linked))

# set order of columns same as in trip table
original_names <- names(tbi$trip)[names(tbi$trip) %in% names(trip_linked)]
new_names <- setdiff(names(trip_linked), names(tbi$trip))
all_names <- c(original_names, new_names)

tbi$trip_linked <-
  trip_linked %>%
  select(one_of(all_names))

rm(mode_type_hierarchy)
rm(mode_type_detailed_hierarchy)
rm(o_purpose_hierarchy)
rm(d_purpose_hierarchy)
rm(d_purpose_category_hierarchy)
rm(missing_codes)
rm(dictionary)
rm(all_names, new_names, original_names)
rm(get_hierarchy)
rm(trip_linked)
