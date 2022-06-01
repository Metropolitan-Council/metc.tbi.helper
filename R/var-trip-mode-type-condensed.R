tbi_tables$trip <-
  tbi_tables$trip %>%
  mutate(mode_type = as.character(mode_type)) %>%
  mutate(mode_type = ifelse(grepl("bicy", mode_type_detailed), "Bicycle", mode_type)) %>%
  mutate(
    mode_type_cond =
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

# Append to dictionary
new_entry <- data.frame(which_table = "trip", variable = "mode_type_cond", wt_field = "trip_weight", category = "Trips", variable_label = "Mode Type Category",
                        value = unique(tbi_tables$trip$mode_type_cond))
tbi_dict <- bind_rows(tbi_dict, new_entry)
message("New column created in trip table: mode_type_cond")
