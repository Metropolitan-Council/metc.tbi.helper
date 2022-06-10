# Get TBI survey data from database ---------
source("data-raw/01-get-survey-data.R")

# Append geographic boundaries to  household, work, school, and trip -----------
source("data-raw/02-add-geographic-boundaries.R")

# Get EPA Efficiency Data -----------
source("data-raw/03-get-epa-vehicle-efficiency-data.R")

# Get DPS Vehicle Weight Data -----------
source("data-raw/04-get-dps-vehicle-weight-data.R")

# Extra variables ------
source("data-raw/05-add-var-person-race.R")
source("data-raw/06-add-var-hh-income-easyread.R")
source("data-raw/07-add-var-trip-purpose.R")
source("data-raw/08-add-var-trip-mode-group.R")
source("data-raw/09-add-var-trip-purpose-broad.R")
source("data-raw/10-add-var-trip-seasons.R")

# Re-format time
trip <- trip %>%
  mutate(
    depart_time_imputed = as.ITime(depart_time_imputed),
    arrive_time = as.ITime(arrive_time)
  )

# Remove PII ------------------
source("data-raw/11-remove-pii.R")

# Trim columns down for manageability ----------
# source("data-raw/slim-survey-data-columns.R")

# Work on the dictionary ------------------
source("data-raw/create-dictionary.R")

# Create additional outputs for app --------------
source("data-raw/histogram_breaks.R")
source("data-raw/input_list.R")
source("data-raw/missing_codes.R")

# Write Data -------------------------
tbi_tables <- list(
  "day" = day,
  "per" = per,
  "hh" = hh,
  "veh" = veh,
  "trip" = trip,
  "trip_purpose" = trip_purpose
)

#### To RData object: -----
usethis::use_data(tbi_tables,
                  overwrite = TRUE,
                  compress = "xz",
                  internal = FALSE
)

#### To Oracle Database: -----


#### To CSV: -----

