# mode share
ms19 <-
tbi19 %>%
  pluck("trip") %>%
  left_join(tbi19$hh %>% select(hh_id, sample_segment)) %>%
  as_survey_design(w = trip_weight, strata = sample_segment) %>%
  filter(!is.na(mode_type)) %>%
  group_by(mode_type) %>%
  summarize(n_trip_raw = n(),
            prop_est = srvyr::survey_prop(),
            n_trip_est = survey_total()) %>%
  mutate(year = "2018-2019")


ms21 <-
tbi21 %>%
  pluck("trip") %>%
  left_join(tbi21$hh %>% select(hh_id, sample_segment)) %>%
  as_survey_design(w = trip_weight, strata = sample_segment) %>%
  filter(!is.na(mode_type)) %>%
  group_by(mode_type) %>%
  summarize(n_trip_raw = n(),
            prop_est = srvyr::survey_prop(),
            n_trip_est = survey_total()) %>%
  mutate(year = "2021")

modeshare <-
  bind_rows(ms19, ms21)



ggplot(modeshare %>% filter(!mode_type == "Other"),
       aes(x = year, y = n_trip_raw, fill = year)) +
  geom_col(position = position_dodge(width = 0.3)) +
  facet_wrap(~mode_type, scales = "free_y", nrow = 3)

