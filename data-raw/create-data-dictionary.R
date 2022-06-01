# Packages -------------
packages <- list("bit64", "tidyverse", "data.table", "DBI", "ROracle", "keyring", "here")
invisible(lapply(packages, library, character.only = TRUE))
rm(packages)


# Set wd-------------
here::here()

# Get data -----------
# Configure database time zone
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")

# read connection string (git-ignored)
source("data-raw/database-connect-string.R")

## connect to database ------------
tbidb <- ROracle::dbConnect(
  dbDriver("Oracle"),
  dbname = connect_string,
  username = "mts_planning_data",
  password = keyring::key_get("mts_planning_data_pw"))

dictionary <-
  ROracle::dbReadTable(tbidb, "TBI19_DICTIONARY") %>%
  select(-table) %>%
  unique() %>%
  as.data.table()
# code to compile dictionary, and add numeric column descriptors

tbi_dict <- dictionary %>%
  filter(category %in% c(
    "Demographics",
    "Attitudes toward autonomous vehicles",
    "Shared mobility",
    "Commute",
    "Trips",
    "Days without travel",
    "Delivery & online shopping",
    "Vehicle"
  )) %>%
  # we'll probably want to play with the ORDERING of this case-when command
  # to assign variables to tables when they appear in multiple tables.
  mutate(which_table = case_when(
    variable %in% names(per) ~ "per",
    variable %in% names(hh) ~ "hh",
    variable %in% names(trip) ~ "trip",
    variable %in% names(day) ~ "day",
    variable %in% names(veh) ~ "veh"
  )) %>%
  # find the weighting field for each table:
  mutate(wt_field = case_when(
    which_table == "per" ~ "person_weight",
    which_table == "hh" ~ "hh_weight",
    which_table == "trip" ~ "trip_weight",
    which_table == "day" ~ "day_weight",
    which_table == "veh" ~ "hh_weight"
  ))


# # Appending missing variables to tbi_dict
# missing_vars <-
# lapply(tbi_tables, function(x) setdiff(names(x), dictionary$variable) %>% as.data.frame()) %>%
#   rbindlist(idcol = "which_table")
#
# names(missing_vars) <- c("which_table", "variable")
#
# # trim out ID's and weights
# missing_vars <-
# missing_vars %>%
#   filter(!grepl("_id", variable)) %>%
#   filter(!grepl("_weight", variable)) %>%
#   filter(!grepl("_num", variable)) %>%
#   filter(!grepl("_date", variable)) %>%
#   mutate(wt_field = case_when(
#     which_table == "per" ~ "person_weight",
#     which_table == "hh" ~ "hh_weight",
#     which_table == "trip" ~ "trip_weight",
#     which_table == "day" ~ "day_weight",
#     which_table == "veh" ~ "hh_weight"
#   ))
#
# write.csv(full_join(missing_vars, dictionary), "data-raw/full_dictionary.csv")


# some work by hand occurred:
tbi_dict_numeric <- read.csv('data-raw/dictionary_numeric_cols.csv')

tbi_dict <- bind_rows(tbi_dict_numeric, tbi_dict)


usethis::use_data(tbi_dict,
                  overwrite = TRUE,
                  compress = "xz",
                  internal = FALSE
)

## Clean up---------------
rm(connect_string, tbi_dict_numeric, tbidb)

