# This script is writen to run after
# 14-make-data-objects.R


# 2019 ------------------------------
missing_vars <-
  lapply(
    tbi19[c("person", "trip", "household", "day", "vehicle", "trip_purpose")],
    \(x) setdiff(names(x), tbi19$dictionary$variable) %>%
      as.data.table()
  ) %>%
  rbindlist(idcol = ".id") %>%
  setnames(c(".id", "."), c("which_table", "variable")) %>%
  .[
    , wt_field := case_when(
      which_table == "person" ~ "person_weight",
      which_table == "household" ~ "hh_weight",
      which_table == "trip" ~ "trip_weight",
      which_table == "day" ~ "day_weight",
      which_table == "vehicle" ~ "hh_weight",
      which_table == "trip_purpose" ~ "trip_purpose_weight"
    )
  ]


tbi19$dictionary <- tbi19$dictionary %>%
  # we'll probably want to play with the ORDERING of this case-when command
  # to assign variables to tables when they appear in multiple tables.
  mutate(which_table = case_when(
    variable %in% names(tbi19$person) ~ "person",
    variable %in% names(tbi19$household) ~ "household",
    variable %in% names(tbi19$trip) ~ "trip",
    variable %in% names(tbi19$day) ~ "day",
    variable %in% names(tbi19$vehicle) ~ "vehicle",
    variable %in% names(tbi19$trip_purpose) ~ "trip_purpose"
  )) %>%
  # find the weighting field for each table:
  mutate(wt_field = case_when(
    which_table == "person" ~ "person_weight",
    which_table == "household" ~ "hh_weight",
    which_table == "trip" ~ "trip_weight",
    which_table == "day" ~ "day_weight",
    which_table == "vehicle" ~ "hh_weight",
    which_table == "trip_purpose" ~ "trip_purpose_weight"
  )) %>%
  # join to our missing variables:
  full_join(missing_vars)




# 2021 ------------------------------
missing_vars <-
  lapply(
    tbi21[c("person", "trip", "household", "day", "vehicle", "trip_purpose")],
    \(x) setdiff(names(x), tbi21$dictionary$variable) %>%
      as.data.table()
  ) %>%
  rbindlist(idcol = ".id") %>%
  setnames(c(".id", "."), c("which_table", "variable")) %>%
  .[
    , wt_field := case_when(
      which_table == "person" ~ "person_weight",
      which_table == "household" ~ "hh_weight",
      which_table == "trip" ~ "trip_weight",
      which_table == "day" ~ "day_weight",
      which_table == "vehicle" ~ "hh_weight",
      which_table == "trip_purpose" ~ "trip_purpose_weight"
    )
  ]


tbi21$dictionary <- tbi21$dictionary %>%
  # we'll probably want to play with the ORDERING of this case-when command
  # to assign variables to tables when they appear in multiple tables.
  mutate(which_table = case_when(
    variable %in% names(tbi21$person) ~ "person",
    variable %in% names(tbi21$household) ~ "household",
    variable %in% names(tbi21$trip) ~ "trip",
    variable %in% names(tbi21$day) ~ "day",
    variable %in% names(tbi21$vehicle) ~ "vehicle",
    variable %in% names(tbi21$trip_purpose) ~ "trip_purpose"
  )) %>%
  # find the weighting field for each table:
  mutate(wt_field = case_when(
    which_table == "person" ~ "person_weight",
    which_table == "household" ~ "hh_weight",
    which_table == "trip" ~ "trip_weight",
    which_table == "day" ~ "day_weight",
    which_table == "vehicle" ~ "hh_weight",
    which_table == "trip_purpose" ~ "trip_purpose_weight"
  )) %>%
  # join to our missing variables:
  full_join(missing_vars)
