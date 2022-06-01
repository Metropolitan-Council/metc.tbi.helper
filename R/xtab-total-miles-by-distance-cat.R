source('~/MetC_Locals/MTS/metc.tbi.helper/R/df-lump-mode-types.R')

vmt_x_trip_distance <-
  tbi_tables$trip %>%
  dplyr::mutate(
    cuts = cut(
      distance,
      breaks = histogram_breaks$distance$breaks,
      labels =  histogram_breaks$distance$labels,
      order_result = TRUE
    )
  ) %>%
  dplyr::rename(distance_category := cuts) %>%
  filter(mode_type_cond %in% c("Drive", "Transit", "Bicycle",  "Walk")) %>%
  filter(!is.na(distance)) %>%
  srvyr::as_survey_design(weights = "trip_weight") %>%
  dplyr::group_by(mode_type_cond, distance_category) %>%
  dplyr::summarize(
    group_N = length(hh_id),
    # raw sample size - number of people, trips, households, days (by group)
    group_N_hh = length(unique(hh_id)),
    # number of households in sample (by group)
    total_vmt = srvyr::survey_total(distance)
  ) %>%
  dplyr::ungroup()


message("New table: trips distance x mode type (condensed)")
