# 2019 -----
### Trip table (purpose on origin & destination ends) --------------
trip19 <- trip19 %>%
  mutate(
    d_purpose_category_broad =
      recode_factor(d_purpose_category_imputed,
        `Home` = "Home",
        `Spent the night at non-home location` = "Home",
        Work = "Work",
        `Work-related` = "Work",
        School = "School",
        `School-related` = "School",
        `Escort` = "Maintenance",
        `Errand/Other` = "Maintenance",
        `Shop` = "Maintenance",
        Meal = "Social/Recreational",
        `Social/Recreation` = "Social/Recreational"
      )
  ) %>%
  mutate(
    o_purpose_category_broad =
      recode_factor(
        o_purpose_category_imputed,
        `Home` = "Home",
        `Spent the night at non-home location` = "Home",
        Work = "Work",
        `Work-related` = "Work",
        School = "School",
        `School-related` = "School",
        `Escort` = "Maintenance",
        `Errand/Other` = "Maintenance",
        `Shop` = "Maintenance",
        Meal = "Social/Recreational",
        `Social/Recreation` = "Social/Recreational"
      )
  )

### Trip purpose table (overall purpose of trip) --------------
trip_purpose19 <- trip_purpose19 %>%
  mutate(
    purpose_category_broad =
      recode_factor(purpose_category,
        `Home` = "Home",
        `Spent the night at non-home location` = "Home",
        Work = "Work",
        `Work-related` = "Work",
        School = "School",
        `School-related` = "School",
        `Escort` = "Maintenance",
        `Errand/Other` = "Maintenance",
        `Shop` = "Maintenance",
        Meal = "Social/Recreational",
        `Social/Recreation` = "Social/Recreational"
      )
  )

# 2021 -----
### Trip table (purpose on origin & destination ends) --------------
trip21 <- trip21 %>%
  mutate(
    d_purpose_category_broad =
      recode_factor(
        d_purpose_category_imputed,
        `Overnight` = "Home",
        `Home` = "Home",
        Work = "Work",
        `Work related` = "Work",
        School = "School",
        `School related` = "School",
        `Escort` = "Maintenance",
        `Errand` = "Maintenance",
        `Shopping` = "Maintenance",
        Meal = "Social/Recreational",
        `Social/Recreation` = "Social/Recreational",
        `Other` = "Other",
        `Change mode` = "Other",
        `Not imputable` = "Not imputable"
      )
  ) %>%
  mutate(
    o_purpose_category_broad =
      recode_factor(o_purpose_category_imputed,
        `Overnight` = "Home",
        `Home` = "Home",
        Work = "Work",
        `Work related` = "Work",
        School = "School",
        `School related` = "School",
        `Escort` = "Maintenance",
        `Errand` = "Maintenance",
        `Shopping` = "Maintenance",
        Meal = "Social/Recreational",
        `Social/Recreation` = "Social/Recreational",
        `Other` = "Other",
        `Change mode` = "Other",
        `Not imputable` = "Not imputable"
      )
  )

### Trip purpose table (overall purpose of trip) --------------
trip_purpose21 <- trip_purpose21 %>%
  mutate(
    purpose_category_broad =
      recode_factor(purpose_category,
        `Overnight` = "Home",
        `Home` = "Home",
        Work = "Work",
        `Work related` = "Work",
        School = "School",
        `School related` = "School",
        `Escort` = "Maintenance",
        `Errand` = "Maintenance",
        `Shopping` = "Maintenance",
        Meal = "Social/Recreational",
        `Social/Recreation` = "Social/Recreational",
        `Other` = "Other",
        `Change mode` = "Other",
        `Not imputable` = "Not imputable"
      )
  )
