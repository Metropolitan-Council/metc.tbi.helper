# # Packages -------------
# source("data-raw/00-load-pkgs.R")
#
# # Set wd-------------
# here::here()
#
# # Get data -----------
# # Configure database time zone
# Sys.setenv(TZ = "America/Chicago")
# Sys.setenv(ORA_SDTZ = "America/Chicago")
#
# ## connect to database ------------
# tbidb <- ROracle::dbConnect(
#   dbDriver("Oracle"),
#   dbname = keyring::key_get("mts_planning_database_string"),
#   username = "mts_planning_data",
#   password = keyring::key_get("mts_planning_data_pw"))
#
# db_dictionary <-
#   ROracle::dbReadTable(tbidb, "TBI19_DICTIONARY") %>%
#   select(-table) %>%
#   unique() %>%
#   as.data.table()
#
# Appending missing variables to tbi_dict
missing_vars <-
  lapply(tbi21[c("per", "trip", "hh", "day", "veh")], function(x) setdiff(names(x), tbi21$dictionary$variable) %>% as.data.frame()) %>%
  bind_rows(.id = "which_table") %>%
  rename(variable = ".") %>%
  mutate(wt_field = case_when(
    which_table == "per" ~ "person_weight",
    which_table == "hh" ~ "hh_weight",
    which_table == "trip" ~ "trip_weight",
    which_table == "day" ~ "day_weight",
    which_table == "veh" ~ "hh_weight",
    which_table == "trip_purpose" ~ "trip_purpose_weight"
  ))

full_dictionary <- tbi21$dictionary %>%
  # we'll probably want to play with the ORDERING of this case-when command
  # to assign variables to tables when they appear in multiple tables.
  mutate(which_table = case_when(
    variable %in% names(tbi21$per) ~ "per",
    variable %in% names(tbi21$hh) ~ "hh",
    variable %in% names(tbi21$trip) ~ "trip",
    variable %in% names(tbi21$day) ~ "day",
    variable %in% names(tbi21$veh) ~ "veh",
    variable %in% names(tbi21$trip_purpose) ~ "trip_purpose"
  )) %>%
  # find the weighting field for each table:
  mutate(wt_field = case_when(
    which_table == "per" ~ "person_weight",
    which_table == "hh" ~ "hh_weight",
    which_table == "trip" ~ "trip_weight",
    which_table == "day" ~ "day_weight",
    which_table == "veh" ~ "hh_weight",
    which_table == "trip_purpose" ~ "trip_purpose_weight"
  )) %>%
  # join to our missing variables:
  full_join(missing_vars) %>%
  mutate(category = "")
# remove survey metadata fields:
# filter(!category %in% c("Survey metadata"))tbi21$

write.csv(full_dictionary, "data-raw/full_dictionary_to_fill_2021.csv")


# some work by hand occurred:
dictionary21 <- read.csv("data-raw/full_dictionary_filled.csv")

dictionary21 <- dictionary21 %>%
  filter(!category %in% c("Survey metadata", "PII")) %>%
  rename( 'which_table' = 'ï..which_table')

## Clean up---------------
# rm(db_dictionary, full_dictionary)
