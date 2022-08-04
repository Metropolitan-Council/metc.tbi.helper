trip19 <-
  trip19 %>%
  mutate(distance_adj =
           ifelse(
      participation_group=="Online or call center recruit, online or call center diary",
      distance*1.35,
      distance
    ))


trip21 <-
  trip21 %>%
  left_join(hh21 %>% select(hh_id, participation_group), by=c("hh_id")) %>%
  mutate(survey_group = word(participation_group,-1)) %>%
  mutate(distance_adj =
           ifelse(
             survey_group %in% c("Center", "Web"),
             distance*1.35,
             distance
           )) %>%
  select(-survey_group, -participation_group)
