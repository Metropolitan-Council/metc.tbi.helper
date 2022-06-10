### Lump Purpose Categories --------------
trip <- trip %>%
  mutate(
    d_purpose_category_new =
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
    o_purpose_category_new =
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
