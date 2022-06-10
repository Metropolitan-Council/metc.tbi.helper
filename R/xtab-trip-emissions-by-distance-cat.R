source('R/df-trip-emissions.R')

co2_x_trip_distance <-
  drive_trips %>%
  dplyr::mutate(
    cuts = cut(
      distance,
      breaks = histogram_breaks$distance$breaks,
      labels =  histogram_breaks$distance$labels,
      order_result = TRUE
    )
  ) %>%
  dplyr::rename(distance_category := cuts) %>%
  filter(!is.na(distance)) %>%
  srvyr::as_survey_design(weights = "trip_weight") %>%
  dplyr::group_by(mode_type_cond, distance_category) %>%
  dplyr::summarize(
    group_N = length(hh_id),
    # raw sample size - number of people, trips, households, days (by group)
    group_N_hh = length(unique(hh_id)),
    # number of households in sample (by group)
    total_co2 = srvyr::survey_total(co2)
  ) %>%
  dplyr::ungroup() %>%
  group_by(mode_type_cond) %>%
  mutate(pct_co2 = 100 * total_co2/sum(total_co2)) 


message("New table: co2_x_trip_distance")

thrive_population <- tbi_tables$hh %>% 
  select(hh_id, thriveCatBroad) %>%
  left_join(tbi_tables$per %>% select(hh_id, person_id, person_weight)) %>%
  group_by(thriveCatBroad) %>%
  summarize(thrivePop = sum(person_weight))
  
co2_x_trip_dist_thrive <-
  drive_trips %>%
  left_join(tbi_tables$hh %>% select(hh_id, thriveCatBroad)) %>%
  dplyr::mutate(
    cuts = cut(
      distance,
      breaks = c(-10, 5, Inf),
      labels =  c("less than 5", "5 or more"
      ),
      order_result = TRUE
    )
  ) %>%
  dplyr::rename(distance_category := cuts) %>%
  filter(!is.na(distance)) %>%
  srvyr::as_survey_design(weights = "trip_weight") %>%
  dplyr::group_by(mode_type_cond, distance_category, thriveCatBroad) %>%
  dplyr::summarize(
    group_N = length(hh_id),
    # raw sample size - number of people, trips, households, days (by group)
    group_N_hh = length(unique(hh_id)),
    # number of households in sample (by group)
    total_co2 = srvyr::survey_total(co2)
  ) %>%
  dplyr::ungroup() %>%
  group_by(mode_type_cond) %>%
  mutate(pct_co2 = 100 * total_co2/sum(total_co2)) %>%
  left_join(thrive_population) %>%
  mutate(co2_per_capita = total_co2/thrivePop)

message("New table: co2_x_trip_dist_thrive")
