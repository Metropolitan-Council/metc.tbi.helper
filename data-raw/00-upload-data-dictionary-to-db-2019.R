########################################################### .
### TOOLBOX ----
########################################################### .
suppressMessages(library(data.table, quietly = T))
suppressMessages(library(bit64, quietly = T))
suppressMessages(library(openxlsx, quietly = T))
suppressMessages(library(dplyr, quietly = T))
suppressMessages(library(keyring, quietly = T))

########################################################### .
### READ IN DATA ----
########################################################### .
# each csv:
day <- fread(paste0(
  keyring::key_get("tbirawdirectory"),
  "day.csv"
))

hh <-
  day <- fread(paste0(
    keyring::key_get("tbirawdirectory"),
    "hh.csv"
  ))

loc <-
  day <- fread(paste0(
    keyring::key_get("tbirawdirectory"),
    "loc.csv"
  ))

veh <-
  day <- fread(paste0(
    keyring::key_get("tbirawdirectory"),
    "veh.csv"
  ))

per <-
  fread(paste0(
    keyring::key_get("tbirawdirectory"),
    "per.csv"
  ))

trip <-  fread(paste0(
  keyring::key_get("tbirawdirectory"),
  "trip.csv"
))

########################################################### .
### SET IDS as INT64 ----
########################################################### .
hh[, hh_id := as.integer64(hh_id)]
veh[, hh_id := as.integer64(hh_id)]
day[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]
loc[, c("hh_id", "person_id", "trip_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id", "trip_id")
]
trip[, c("hh_id", "person_id", "trip_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id", "trip_id")
]
per[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]


# # Write to Oracle Database
# Opening the toolbox-------------------------------
library(DBI)
library(rstudioapi) # this package allows us to type in a password when connecting to the database.
library(ROracle) # Moment of truth...does it load?
# Configure database time zone -------------------------------
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")


# #Connecting to the database: the other 25% of the battle -------------------------------
connect.string <- "(DESCRIPTION=(ADDRESS_LIST = (ADDRESS = (PROTOCOL = TCP)(HOST = fth-exa-scan.mc.local  )(PORT = 1521)))(CONNECT_DATA = (SERVER = DEDICATED)(SERVICE_NAME =  com4te.mc.local)))"
tbidb <- ROracle::dbConnect(
  dbDriver("Oracle"),
  dbname = keyring::key_get("mts_planning_database_string"),
  username = "mts_planning_data",
  # mts_planning_view for viewing data only, no read/write priviliges. mts_planning_data is the username for read/write privlieges.
  password = rstudioapi::askForPassword("database password")
)

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





########################################################### .
### CODEBOOKS ####
########################################################### .

# codebooks:
hh_codes <-
  data.table(read.xlsx(
    paste0(keyring::key_get("tbirawdirectory"),
           "TBI Wave 1 Dataset Codebook.xlsx"), sheet = "hh"))
veh_codes <-
  data.table(read.xlsx(paste0(keyring::key_get("tbirawdirectory"),
                              "TBI Wave 1 Dataset Codebook.xlsx"), sheet = "vehicle"))
per_codes <-
  data.table(read.xlsx(paste0(keyring::key_get("tbirawdirectory"),
                              "TBI Wave 1 Dataset Codebook.xlsx"), sheet = "person"))
day_codes <-
  data.table(read.xlsx(paste0(keyring::key_get("tbirawdirectory"),
                              "TBI Wave 1 Dataset Codebook.xlsx"), sheet = "day"))
trip_codes <-
  data.table(read.xlsx(paste0(keyring::key_get("tbirawdirectory"),
                              "TBI Wave 1 Dataset Codebook.xlsx"), sheet = "trip"))
# no codebook for location

variable_names <-
  data.table(read.xlsx(paste0(keyring::key_get("tbirawdirectory"),
                              "TBI Wave 1 Dataset Codebook.xlsx"), sheet = "summary"))


lapply(
  list(hh_codes, per_codes, trip_codes, veh_codes),
  FUN = function(x) {
    x[duplicated(x)]
  }
)
# only one duplicated entry  - hhcodes, participation group and residence months
hh_codes <- unique(hh_codes)

########################################################### .
### BAD CODEBOOK ENTRIES ----
########################################################### .
# whitespace in "d_purpose":
trip_codes$variable <- trimws(trip_codes$variable)

# already translated:
hh_codes <-
  hh_codes[!variable %in% c("sample_home_state", "sample_segment", "study_design")]


per_codes$table <- "Person"
trip_codes$table <- "Trip"
veh_codes$table <- "Vehicle"
hh_codes$table <- "Household"
day_codes$table <- "Day"


codebooks <-
  list(per_codes, trip_codes, veh_codes, hh_codes, day_codes)

all_codes <- rbindlist(codebooks) %>%
  left_join(variable_names %>% select(variable, label, category, survey_question, logic)) %>%
  select(table, category, variable, survey_question, logic, label, value, value_label, value_logic) %>%
  rename(variable_logic = logic, variable_label = label)

ROracle::dbWriteTable(
  tbidb,
  value = all_codes,
  name = "TBI19_DICTIONARY",
  # replace the  file:
  overwrite = TRUE,
  row.names = FALSE
)
