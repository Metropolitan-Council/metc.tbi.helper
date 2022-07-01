selectall_pivot <-
function(surveyobj, table, variable_prefix) {
  surveyobj %>%
    pluck(table) %>%
    select(contains("_id"), contains("weight"), starts_with(variable_prefix)) %>%
    pivot_longer(
      cols = starts_with(variable_prefix), names_prefix = paste0(variable_prefix, "_")
    )
}

pivotlong_selectall(tbi19, "day", "no_travel")
