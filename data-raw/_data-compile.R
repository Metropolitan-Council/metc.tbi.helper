# Get TBI survey data from database ---------
source("data-raw/get-survey-data.R")

# Trim columns down for manageability ----------
source("data-raw/slim-survey-data-columns.R")

# Trim survey data to MPO region -----------
source("data-raw/trim-survey-data-to-mpo.R")

# Get EPA fuel efficiency Data ----------
source("data-raw/get-epa-vehicle-efficiency-data.R")

# Get Vehicle Weight data ----------
source("data-raw/get-dps-vehicle-weight-data.R")

# Append Thrive Category where lat/lons exist ----------
source("data-raw/add-thrive-to-hh-trip.R")

# Append MPO boundary to trips origin/destination ----------
source("data-raw/add-mpo-boundary-to-trips.R")

# Remove PII ----------
source("data-raw/remove-pii.R")

# Create Data Dictionary ----------
source("data-raw/create-data-dictionary.R")

# Write data as RData Obj-------------------
tbi_tables <- list(
  "day" = day,
  "per" = per,
  "hh" = hh,
  "veh" = veh,
  "trip" = trip
)


usethis::use_data(tbi_tables,
                  overwrite = TRUE,
                  compress = "xz",
                  internal = FALSE
)

# Write data to Database
tbidb <- ROracle::dbConnect(
  dbDriver("Oracle"),
  dbname = keyring::key_get("mts_planning_database_string"),
  username = "mts_planning_data",
  password = keyring::key_get("mts_planning_data_pw")
)

ROracle::dbWriteTable(tbidb, "tbi_19_day_public", tbi_tables$day, append = FALSE, overwrite = T)
ROracle::dbWriteTable(tbidb, "tbi_19_trip_public", tbi_tables$trip, append = FALSE, overwrite = T)
ROracle::dbWriteTable(tbidb, "tbi_19_hh_public", tbi_tables$hh, append = FALSE, overwrite = T)
ROracle::dbWriteTable(tbidb, "tbi_19_veh_public", tbi_tables$veh, append = FALSE, overwrite = T)
ROracle::dbWriteTable(tbidb, "tbi_19_per_public", tbi_tables$per, append = FALSE, overwrite = T)

# code to compile dictionary, and add numeric column descriptors


rm(hh, per, trip, veh, day, dictionary)

