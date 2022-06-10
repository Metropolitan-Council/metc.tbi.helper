###########################################################.
### TOOLBOX ----
###########################################################.
suppressMessages(library(data.table, quietly = T))
suppressMessages(library(bit64, quietly = T))
suppressMessages(library(openxlsx, quietly = T))
suppressMessages(library(dplyr, quietly = T))
suppressMessages(library(keyring, quietly = T))

#Database toolbox-------------------------------
library(DBI)
library(rstudioapi) # this package allows us to type in a password when connecting to the database.
library(ROracle) # Moment of truth...does it load?
#Configure database time zone -------------------------------
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")


# #Connecting to the database: the other 25% of the battle -------------------------------
tbidb = ROracle::dbConnect(
  dbDriver("Oracle"),
  dbname = keyring::key_get("mts_planning_database_string"),
  username = 'mts_planning_data',
  # mts_planning_view for viewing data only, no read/write priviliges. mts_planning_data is the username for read/write privlieges.
  password = keyring::key_get("mts_planning_data_pw")
)

###########################################################.
### CODEBOOKS ####
###########################################################.

# codebooks:
vars <- fread('data_shell/variable_description.csv')
vals <- fread('data_shell/value_labels.csv')

# check for duplicated entries:
vals[duplicated(vals)]
vals <- unique(vals)

vars[duplicated(vars)]
# none

###########################################################.
### BAD CODEBOOK ENTRIES ----
###########################################################.
# whitespace:
vals <- vals %>%
  mutate(across(c(variable, label), ~trimws(.)))

vars <- vars %>%
  mutate(across(c(variable, description), ~trimws(.)))

all_codes <- vals %>%
  left_join(vars) %>%
  select(table, variable, description, var_logic, value, label) %>%
  rename(variable_logic = var_logic, variable_label = description)

ROracle::dbWriteTable(
  tbidb,
  value = all_codes,
  name = "TBI21_DICTIONARY",
  # replace the  file:
  overwrite = TRUE,
  row.names = FALSE
)


###########################################################.
### COMPARE TO 2019 ----
###########################################################.
tbi19dict <- ROracle::dbReadTable(tbidb, "TBI19_DICTIONARY") %>%
  mutate(value = as.numeric(value))

# Variables that are different:
setdiff(tbi19dict$variable, all_codes$variable)
setdiff(all_codes$variable, tbi19dict$variable)


both_years_dictionary <- tbi19dict %>%
  full_join(all_codes,
            by = c("variable"),
            suffix = c("_19", "_21")) %>%
  select(
    variable,
    variable_logic_19,
    variable_label_19,
    variable_label_21,
    variable_logic_21
  ) %>%
  unique() %>%
  mutate(across(
    c(
      variable,
      variable_logic_19,
      variable_label_19,
      variable_label_21,
      variable_logic_21
    ),
    ~ trimws(.)
  )) %>%
  # differing labels?
  mutate(label_diff = ifelse(variable_label_19 == variable_label_21, F, T)) %>%
  mutate(logic_diff = ifelse(variable_logic_19 == variable_logic_21, F, T))


both_years_var %>%
  filter(label_diff == T) %>%
  select(variable, variable_label_19, variable_label_21)

both_years_var %>%
  filter(label_diff == F) %>%
  select(variable, variable_label_19, variable_label_21)

# For these, assign var label 21 to var label 19 -- I like them better
both_years_var <- both_years_var %>%
  mutate(variable_label_21 = case_when(label_diff = T ~ variable_label_19))

# Now find where survey logic has changed
both_years_var %>%
  filter(logic_diff == T) %>%
  select(variable, variable_logic_19, variable_logic_21)


###########################################################.
### READ IN DATA ----
###########################################################.
setwd(choose.dir()) # navigate to N: drive where raw files are stored.

# each csv:
day  <- fread('Data/raw-Data/TBI Wave 1 Dataset 20200630/day.csv')
hh   <-
  fread('Data/raw-Data/TBI Wave 1 Dataset 20200630/household.csv')
loc  <-
  fread('Data/raw-Data/TBI Wave 1 Dataset 20200630/location.csv')
veh  <-
  fread('Data/raw-Data/TBI Wave 1 Dataset 20200630/vehicle.csv')
per  <-
  fread('Data/raw-Data/TBI Wave 1 Dataset 20200630/person.csv')
trip <- fread('Data/raw-Data/TBI Wave 1 Dataset 20200630/trip.csv')


###########################################################.
### SET IDS as INT64 ----
###########################################################.
hh[, hh_id := as.integer64(hh_id)]
veh[, hh_id := as.integer64(hh_id)]
day[, c('hh_id', 'person_id') := lapply(.SD, as.integer64),
    .SDcols = c('hh_id', 'person_id')]
loc[, c('hh_id', 'person_id', 'trip_id') := lapply(.SD, as.integer64),
    .SDcols = c('hh_id', 'person_id', 'trip_id')]
trip[, c('hh_id', 'person_id', 'trip_id') := lapply(.SD, as.integer64),
     .SDcols = c('hh_id', 'person_id', 'trip_id')]
per[, c('hh_id', 'person_id') := lapply(.SD, as.integer64),
    .SDcols = c('hh_id', 'person_id')]


# # Write to Oracle Database

# # Update Database files ---------------------------------------------
# # Write over the database files
ROracle::dbWriteTable(
  tbidb,
  value = hh,
  name = "TBI19_HOUSEHOLD_RAW",
  # replace the  file:
  overwrite = TRUE,
  row.names = FALSE
)

ROracle::dbWriteTable(
  tbidb,
  value = per,
  name = "TBI19_PERSON_RAW",
  # replace the  file:
  overwrite = TRUE,
  row.names = FALSE
)

ROracle::dbWriteTable(
  tbidb,
  value = veh,
  name = "TBI19_VEHICLE_RAW",
  # replace the configuration file:
  overwrite = TRUE,
  row.names = FALSE
)

ROracle::dbWriteTable(
  tbidb,
  value = day,
  name = "TBI19_DAY_RAW",
  # replace the  file:
  overwrite = TRUE,
  row.names = FALSE
)

ROracle::dbWriteTable(
  tbidb,
  value = loc,
  name = "TBI19_LOCATION_RAW",
  # replace the  file:
  overwrite = TRUE,
  row.names = FALSE
)

ROracle::dbWriteTable(
  tbidb,
  value = trip,
  name = "TBI19_TRIP_RAW",
  # replace the  file:
  overwrite = TRUE,
  row.names = FALSE
)





