selectall_pivot <-
  function(surveyobj, table, variable_prefix) {
    labels <- surveyobj %>%
      pluck("dictionary") %>%
      filter(grepl(pattern = variable_prefix, x = variable)) %>%
      select(variable, variable_label) %>%
      unique()


    surveyobj %>%
      pluck(table) %>%
      select(
        contains("_id"), contains("_num"), contains("weight"),
        starts_with(variable_prefix)
      ) %>%
      pivot_longer(
        cols = starts_with(variable_prefix),
        # names_prefix = paste0(variable_prefix, "_"),
        names_to = "variable"
      ) %>%
      left_join(labels)
  }

# selectall_pivot(tbi19, "day", "no_travel")
