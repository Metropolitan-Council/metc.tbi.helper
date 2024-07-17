# Load necessary packages ------
source("data-raw/00-load-pkgs.R")
source("data-raw/_db_connect.R")

# Get TBI survey data from database ---------
source("data-raw/01-get-survey-data.R")

# Append geographic boundaries to household, work, school, and trip -----------
source("data-raw/02-derive-var-vehicle-id.R")

# Append geographic boundaries to  household, work, school, and trip -----------
source("data-raw/03-get-geographic-boundaries.R")

# Get EPA Efficiency Data -----------
# script under construction
# source("data-raw/04-get-epa-vehicle-efficiency-data.R")

# Get DPS Vehicle Weight Data -----------
# script under construction
# source("data-raw/05-get-dps-vehicle-weight-data.R")

# Extra variables ------
source("data-raw/04-derive-var-person-race.R")
source("data-raw/05-derive-var-hh-income-easyread.R")


# source("data-raw/08-derive-var-trip-mode-group.R") # deprecated
# source("data-raw/09-derive-var-trip-distance-adjusted.R") # Analysis needed
# source("data-raw/10-derive-table-trip-purpose.R")
source("data-raw/06-derive-var-trip-purpose-broad.R")
source("data-raw/07-derive-var-trip-seasons.R")
source("data-raw/08-remove-pii.R")
# source("data-raw/14-make-data-objects.R")
source("data-raw/99-distribute-data.R")
