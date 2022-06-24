trip19 <-
  trip19 %>%
  mutate(mode_type = as.character(mode_type)) %>%
  mutate(mode_type = ifelse(grepl("bicy", mode_type_detailed), "Bicycle", mode_type)) %>%
  mutate(
    mode_group =
      recode_factor(mode_type,
        `Household vehicle` = "Drive",
        `School bus` = "Other",
        `Other vehicle` = "Drive",
        `Public bus` = "Transit",
        `Walk` = "Walk",
        `Bicycle` = "Bicycle",
        `Rail` = "Transit",
        `Other bus` = "Other",
        Other = "Other",
        `Smartphone ridehailing service` = "Drive",
        `For-hire vehicle` = "Drive",
        `Micromobility` = "Other",
        `Long distance passenger mode` = "Other"
      )
  )

# 2021 data missing mode_type_detailed
mode_type_hierarchy <- read.csv('data-raw/mode_type_hierarchy.csv')
trip21_detailed <- trip21 %>%
  select(trip_id, mode_1, mode_2, mode_3) %>%
  pivot_longer(-trip_id, values_to = "mode_type_detailed") %>%
  select(-name) %>%
  filter(!is.na(mode_type_detailed)) %>%
  left_join(mode_type_hierarchy, by = "mode_type_detailed") %>%
  group_by(trip_id) %>%
  summarize(mode_type_value = max(mode_type_value)) %>%
  left_join(mode_type_hierarchy, by = "mode_type_value")



trip21 <-
  trip21 %>%
  left_join(trip21_detailed, by = "trip_id")
  mutate(mode_type = as.character(mode_type)) %>%
  mutate(mode_type = ifelse(grepl("bicy", mode_type_detailed), "Bicycle", mode_type)) %>%
  mutate(
    mode_group =
      recode_factor(mode_type,
                    `Walk` = "Walk",
                    `Bicycle or e-bicycle` = "Bicycle",
                    `Bike-share` = "Bicycle",
                    `Scooter-share` = "Other",
                    `Taxi` = "Drive",
                    `Smartphone-app ride-hailing service` = "Drive",
                    Other = "Other",
                    `Vehicle` = "Drive",
                    `Carshare` = "Drive",
                    `School bus` = "Other",
                    `Shuttle` = "Transit",
                    `Ferry` = "Other",
                    `Transit` = "Transit",
                    `Long distance passenger mode` = "Other"
      )
  )


message("New variable in trip table: mode_group, that condenses mode_type_detailed (but keeps bicycles separate)")
