# Mode group = Drive, Transit, Bicycle, Walk, Other
# Mode group 2 = Drive alone, Drive with others, Transit, Bicycle, Walk, Other
trip19 <-
  trip19 %>%
  mutate(mode_type_chr = as.character(mode_type)) %>%
  mutate(mode_type_chr = ifelse(grepl("bicy|bike", mode_type_detailed, ignore.case = T),
                                "Bicycle/Scooter", mode_type_chr)) %>%
  mutate(
    mode_group =
      recode_factor(mode_type_chr,
        `Household vehicle` = "Drive",
        `Other vehicle` = "Drive",
        `For-hire vehicle` = "Drive",
        `Smartphone ridehailing service` = "Drive",
        `Public bus` = "Transit",
        `Rail` = "Transit",
        `Bicycle` = "Bicycle/Scooter",
        `Walk` = "Walk",
        `Other bus` = "Other",
        Other = "Other",
        `Micromobility` = "Bicycle/Scooter",
        `Long distance passenger mode` = "Other",
        `School bus` = "Other"
      )
  ) %>%
  select(-mode_type_chr)


# trip19 %>%
#   select(mode_group, mode_type, mode_type_detailed) %>%
#   filter(!is.na(mode_type) & !is.na(mode_type_detailed)) %>%
#   unique()


trip21 <-
  trip21 %>%
  mutate(
    mode_group =
      recode_factor(mode_type ,
        `Vehicle` = "Drive",
        `Carshare` = "Drive",
        `Taxi` = "Drive",
        `Smartphone-app ride-hailing service` = "Drive",
        `Transit` = "Transit",
        `Bike-share` = "Bicycle/Scooter",
        `Bicycle or e-bicycle` = "Bicycle/Scooter",
        `Scooter-share` = "Bicycle/Scooter",
        `Walk` = "Walk",
        Other = "Other",
        `School bus` = "Other",
        `Ferry` = "Other",
        `Shuttle` = "Other",
        `Long distance passenger mode` = "Other"
      )
  )

trip21 %>%
  select(mode_group, mode_type, mode_type_detailed) %>%
  filter(!is.na(mode_type) & !is.na(mode_type_detailed)) %>%
  unique()

message("New variable in trip table: mode_group, that condenses mode_type_detailed (but keeps bicycles separate)")
