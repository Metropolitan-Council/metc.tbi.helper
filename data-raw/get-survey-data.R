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
  password = keyring::key_get("mts_planning_data_pw")
)

## Load tables ---------
hh <- ROracle::dbReadTable(tbidb, "TBI19_HOUSEHOLD_RAW") %>% as.data.table()
per <- ROracle::dbReadTable(tbidb, "TBI19_PERSON_RAW") %>% as.data.table()
trip <- ROracle::dbReadTable(tbidb, "TBI19_TRIP_RAW") %>% as.data.table()
veh <- ROracle::dbReadTable(tbidb, "TBI19_VEHICLE_RAW") %>% as.data.table()
day <- ROracle::dbReadTable(tbidb, "TBI19_DAY_RAW") %>% as.data.table()

## Translate tables using dictionary -----------
dictionary <-
  ROracle::dbReadTable(tbidb, "TBI19_DICTIONARY") %>%
  select(-table) %>%
  unique() %>%
  as.data.table()

# note: this part uses data.table syntax and functions.
translate_using_dictionary <- function(dat, dictionary) {
  # select the names of columns that do not need to be translated -
  # (anything column that's not in the codebook):
  dat_id_vars <-
    names(dat[, !colnames(dat) %in% unique(dictionary$variable), with = FALSE])

  # melt the dataset:
  dat_long <-
    # suppressing warning about column types being coerced to character.
    suppressWarnings(
      melt(
        dat,
        var = "variable",
        val = "value",
        id.vars = dat_id_vars
      ),
      classes = c("message", "warning")
    )

  # convert var/value pairs to character, for both dictionary and data:
  dat_long[, c("variable", "value") := list(as.character(variable), as.character(value))]
  dictionary[, c("variable", "value") := list(as.character(variable), as.character(value))]

  # merge data to dictionary:
  dat_long <- merge(
    dat_long,
    dictionary,
    on = c("variable", "value"),
    # keep all data
    all.x = T,
    # don't keep all the extraneous dictionary
    all.y = F
  )

  # cast back to wide:
  dat_cast_formula <-
    as.formula(paste(paste(dat_id_vars, collapse = " + "), "~ variable"))
  newdat <-
    dcast(dat_long, dat_cast_formula, value.var = "value_label")

  # fix factor variables - relevel according to the order in the codebook (to start)
  namevec <- names(newdat)
  for (i in namevec) {
    if (i %in% unique(dictionary$variable)) {
      col_levels <- dictionary$value_label[dictionary$variable == i]
      newdat[, (i) := factor(get(i), levels = col_levels)]
    }
  }
  newdat <- droplevels(newdat)
  return(newdat)
}

per <- translate_using_dictionary(per, dictionary)
hh <- translate_using_dictionary(hh, dictionary)
veh <- translate_using_dictionary(veh, dictionary)
day <- translate_using_dictionary(day, dictionary)
trip <- translate_using_dictionary(trip, dictionary)

# Replace missing with NA -----------
# all the numeric codes for missing:
all_missing_codes <-
  dictionary[grep("Missing", value_label), "value", with = F]
all_missing_codes <- unique(all_missing_codes$value)

# all the value labels that include missing:
all_missing_labels <-
  dictionary[grep("Missing", value_label), "value_label", with = F]

# both as character:
all_missing_vector <- unique(rbind(all_missing_codes, all_missing_labels, use.names = F))
all_missing_vector <- all_missing_vector$x


# function to replace missing values with NA:
replace_survey_missing <- function(dat) {
  na_dat <- dat %>%
    mutate(across(
      where(is.numeric),
      ~ ifelse(. %in% all_missing_codes, NA, .)
    )) %>%
    # replace factor entries with NA:
    mutate(across(
      where(is.factor),
      ~ factor(., exclude = all_missing_vector)
    ))

  return(na_dat)
}

day <- replace_survey_missing(day)
trip <- replace_survey_missing(trip)
hh <- replace_survey_missing(hh)
per <- replace_survey_missing(per)
veh <- replace_survey_missing(veh)

rm(all_missing_labels, all_missing_vector, all_missing_codes)

# Set IDs as Integer64 -----------
hh[, hh_id := as.integer64(hh_id)]
veh[, hh_id := as.integer64(hh_id)]
day[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]
trip[, c("hh_id", "person_id", "trip_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id", "trip_id")
]
per[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]


### Add vehicle ID -------
veh <-
  veh %>% mutate(veh_id = paste(hh_id, "_", vehicle_num, sep = ""))

trip <- trip %>%
  mutate(veh_id = str_replace(
    mode_type_detailed,
    pattern = "Household vehicle ",
    replacement = paste(hh_id, "_", sep = "")
  ))

## Clean up---------------
rm(connect_string, replace_survey_missing, translate_using_dictionary, tbidb)
