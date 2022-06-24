# Load necessary packages ------
source("data-raw/00-load-pkgs.R")

# Get TBI survey data from database ---------
source("data-raw/01-get-survey-data.R")

# Append geographic boundaries to  household, work, school, and trip -----------
source("data-raw/02-derive-var-vehicle-id.R")

# Append geographic boundaries to  household, work, school, and trip -----------
source("data-raw/03-get-geographic-boundaries.R")

# Get EPA Efficiency Data -----------
source("data-raw/04-get-epa-vehicle-efficiency-data.R")

# Get DPS Vehicle Weight Data -----------
source("data-raw/05-get-dps-vehicle-weight-data.R")

# Extra variables ------
source("data-raw/06-derive-var-person-race.R")
source("data-raw/07-derive-var-hh-with-poc.R")
source("data-raw/08-derive-var-hh-income-easyread.R")
source("data-raw/09-derive-var-trip-mode-group.R")
source("data-raw/10-derive-table-trip-purpose.R")
source("data-raw/11-derive-var-trip-purpose-broad.R")
source("data-raw/12-derive-var-trip-seasons.R")



# Re-format time
trip <- trip %>%
  mutate(
    depart_time_imputed = as.ITime(depart_time_imputed),
    arrive_time = as.ITime(arrive_time)
  )

# Trim columns down for manageability ----------
# source("data-raw/13-slim-survey-data-columns.R")

# Remove PII ------------------
source("data-raw/14-remove-pii.R")

# Work on the dictionary ------------------
source("data-raw/15-create-dictionary.R")

# Write Data -------------------------
tbi_tables <- list(
  "day" = day,
  "per" = per,
  "hh" = hh,
  "veh" = veh,
  "trip" = trip,
  "trip_purpose" = trip_purpose,
  "dictionary" = dictionary
)

#### To RData object: -----
# usethis::use_data(tbi_tables,
#   overwrite = TRUE,
#   compress = "xz",
#   internal = FALSE
# )

save(tbi_tables,
     file = "data/tbi_tables.rda",
     compress = "xz"
)


