trip19 <-
  trip19 %>%
  mutate(mode_type_chr = as.character(mode_type)) %>%
  mutate(mode_type_chr = ifelse(grepl("bicy|bike", mode_type_detailed, ignore.case = T),
                                "Bicycle", mode_type_chr)) %>%
  mutate(
    mode_group =
      recode_factor(mode_type_chr,
        `Household vehicle` = "Drive",
        `Other vehicle` = "Drive",
        `For-hire vehicle` = "Drive",
        `Smartphone ridehailing service` = "Drive",
        `Public bus` = "Transit",
        `Rail` = "Transit",
        `Bicycle` = "Bicycle",
        `Walk` = "Walk",
        `Other bus` = "Other",
        Other = "Other",
        `Micromobility` = "Other",
        `Long distance passenger mode` = "Other",
        `School bus` = "Other"
      )
  ) %>%
  select(-mode_type_chr)


trip21 <-
  trip21 %>%
  mutate(mode_type_chr = as.character(mode_type)) %>%
  mutate(mode_type_chr = ifelse(grepl("bicy", mode_type_detailed), "Bicycle", mode_type_chr)) %>%
  mutate(
    mode_group =
      recode_factor(mode_type_chr,
        `Vehicle` = "Drive",
        `Carshare` = "Drive",
        `Taxi` = "Drive",
        `Smartphone-app ride-hailing service` = "Drive",
        `Transit` = "Transit",
        `Bicycle or e-bicycle` = "Bicycle",
        `Bike-share` = "Bicycle",
        `Scooter-share` = "Other",
        `Walk` = "Walk",
        Other = "Other",
        `School bus` = "Other",
        `Ferry` = "Other",
        `Shuttle` = "Other",
        `Long distance passenger mode` = "Other"
      )
  ) %>%
  select(-mode_type_chr)


message("New variable in trip table: mode_group, that condenses mode_type_detailed (but keeps bicycles separate)")
