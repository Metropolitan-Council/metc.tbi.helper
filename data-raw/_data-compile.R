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
source("data-raw/07-derive-var-hh-income-easyread.R")
source("data-raw/08-derive-var-trip-mode-group.R")
source("data-raw/09-derive-var-trip-distance-adjusted.R")
source("data-raw/10-derive-table-trip-purpose.R")
source("data-raw/11-derive-var-trip-purpose-broad.R")
source("data-raw/12-derive-var-trip-seasons.R")

# Re-format time
trip19 <- trip19 %>%
  mutate(
    depart_time_imputed = as.ITime(depart_time_imputed),
    arrive_time = as.ITime(arrive_time)
  )

trip21 <- trip21 %>%
  mutate(
    depart_time = as.ITime(depart_time),
    arrive_time = as.ITime(arrive_time)
  )

# Remove PII ------------------
source("data-raw/14-remove-pii.R")

# Work on the dictionary ------------------
# source("data-raw/15-create-dictionary.R")
dictionary19 <- read.csv("data-raw/final_dictionary_2019.csv")
dictionary21 <- read.csv("data-raw/final_dictionary_2021.csv")
dictionary21 <- dictionary21 %>% rename(variable_label = description)

# Put data in lists -------------------------
tbi19 <- list(
  "day" = day19,
  "per" = per19,
  "hh" = hh19,
  "veh" = veh19,
  "trip" = trip19,
  "trip_purpose" = trip_purpose19,
  "dictionary" = dictionary19
)

tbi21 <- list(
  "day" = day21,
  "per" = per21,
  "hh" = hh21,
  "veh" = veh21,
  "trip" = trip21,
  "trip_purpose" = trip_purpose21,
  "dictionary" = dictionary21
)

# Remove non-ASCII characters -----
remove_nonascii <-
  function(dat) {
    dat %>%
      mutate(across(where(is.character), function(x) stringi::stri_trans_general(x, "latin-ascii")))
  }

tbi21 <- tbi21 %>%
  lapply(remove_nonascii)

tbi19 <- tbi19 %>%
  lapply(remove_nonascii)

# Write data as rda -------------------------
save(tbi19,
  file = "data/tbi19.rda",
  compress = T
)

save(tbi21,
  file = "data/tbi21.rda",
  compress = T
)
