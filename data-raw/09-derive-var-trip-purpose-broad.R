### Trip table (purpose on origin & destination ends) --------------
trip <- trip %>%
  mutate(
    d_purpose_category_broad =
      recode_factor(d_purpose_category_imputed,
        School = "School",
        `School-related` = "School",
        Work = "Work",
        `Work-related` = "Work",
        `Escort` = "Maintenance",
        `Errand/Other` = "Maintenance",
        `Shop` = "Maintenance",
        Meal = "Social/Recreational",
        `Social/Recreation` = "Social/Recreational",
        `Spent the night at non-home location` = "Home",
        `Home` = "Home"
      )
  ) %>%
  mutate(
    o_purpose_category_broad =
      recode_factor(o_purpose_category_imputed,
        School = "School",
        `School-related` = "School",
        Work = "Work",
        `Work-related` = "Work",
        `Escort` = "Maintenance",
        `Errand/Other` = "Maintenance",
        `Shop` = "Maintenance",
        Meal = "Social/Recreational",
        `Social/Recreation` = "Social/Recreational",
        `Spent the night at non-home location` = "Home",
        `Home` = "Home"
      )
  )

### Trip purpose table (overall purpose of trip) --------------
trip_purpose <- trip_purpose %>%
  mutate(
    purpose_category_broad =
      recode_factor(purpose_category,
                    School = "School",
                    `School-related` = "School",
                    Work = "Work",
                    `Work-related` = "Work",
                    `Escort` = "Maintenance",
                    `Errand/Other` = "Maintenance",
                    `Shop` = "Maintenance",
                    Meal = "Social/Recreational",
                    `Social/Recreation` = "Social/Recreational",
                    `Spent the night at non-home location` = "Home",
                    `Home` = "Home"
      )
  )
