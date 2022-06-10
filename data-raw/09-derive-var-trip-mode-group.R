trip <-
  trip %>%
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
