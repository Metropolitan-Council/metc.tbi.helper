trips_x_distance_x_mode <-
  tbi19$trip %>%
  dplyr::mutate(
    cuts = cut(
      distance,
      breaks = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, Inf),
      labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "More than 10"),
      order_result = TRUE
    )
  ) %>%
  dplyr::rename(distance_category := cuts) %>%
  filter(mode_group %in% c("Drive", "Transit", "Bicycle", "Walk")) %>%
  filter(!is.na(distance)) %>%
  srvyr::as_survey_design(weights = "trip_weight") %>%
  dplyr::group_by(mode_group, distance_category) %>%
  dplyr::summarize(
    group_N = length(hh_id),
    # raw sample size - number of people, trips, households, days (by group)
    group_N_hh = length(unique(hh_id)),
    # number of households in sample (by group)
    n_trips = srvyr::survey_total(),
    pct_trips = 100 * srvyr::survey_prop()
  ) %>%
  dplyr::ungroup()


message("New table: trips_x_distance_x_mode, trips by distance category and mode group")
