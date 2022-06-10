# # Packages -------------
# packages <- list("bit64", "tidyverse", "data.table", "DBI", "ROracle", "keyring", "here")
# invisible(lapply(packages, library, character.only = TRUE))
# rm(packages)
#
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
# # Appending missing variables to tbi_dict
# missing_vars <-
#   lapply(tbi_tables, function(x) setdiff(names(x), db_dictionary$variable) %>% as.data.frame()) %>%
#   bind_rows(.id = "which_table") %>%
#   rename(variable = ".") %>%
#   mutate(wt_field = case_when(
#     which_table == "per" ~ "person_weight",
#     which_table == "hh" ~ "hh_weight",
#     which_table == "trip" ~ "trip_weight",
#     which_table == "day" ~ "day_weight",
#     which_table == "veh" ~ "hh_weight",
#     which_table == "trip_purpose" ~ "trip_purpose_weight"
#   ))
#
# full_dictionary <- db_dictionary %>%
#   # we'll probably want to play with the ORDERING of this case-when command
#   # to assign variables to tables when they appear in multiple tables.
#   mutate(which_table = case_when(
#     variable %in% names(per) ~ "per",
#     variable %in% names(hh) ~ "hh",
#     variable %in% names(trip) ~ "trip",
#     variable %in% names(day) ~ "day",
#     variable %in% names(veh) ~ "veh",
#     variable %in% names(trip_purpose) ~ "trip_purpose"
#   )) %>%
#   # find the weighting field for each table:
#   mutate(wt_field = case_when(
#     which_table == "per" ~ "person_weight",
#     which_table == "hh" ~ "hh_weight",
#     which_table == "trip" ~ "trip_weight",
#     which_table == "day" ~ "day_weight",
#     which_table == "veh" ~ "hh_weight",
#     which_table == "trip_purpose" ~ "trip_purpose_weight"
#   )) %>%
#   # join to our missing variables:
#   full_join(missing_vars) %>%
#   # remove survey metadata fields:
#   filter(!category %in% c("Survey metadata"))
#
# write.csv(full_dictionary, "data-raw/full_dictionary_to_fill.csv")


# some work by hand occurred:
tbi_dict <- read.csv('data-raw/full_dictionary_filled.csv')

tbi_dict <- tbi_dict %>%
  filter(!category %in% c("Survey metadata", "PII"))


usethis::use_data(tbi_dict,
  overwrite = TRUE,
  compress = "xz",
  internal = FALSE
)

## Clean up---------------
# rm(db_dictionary, full_dictionary)
