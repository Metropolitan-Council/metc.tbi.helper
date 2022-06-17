# Packages -------------
source("data-raw/00-load-pkgs.R")
# Set wd-------------
here::here()

# Get data -----------
# Configure database time zone
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")


## connect to database ------------
tbidb <- ROracle::dbConnect(
  DBI::dbDriver("Oracle"),
  dbname = keyring::key_get("mts_planning_database_string"),
  username = "mts_planning_data",
  password = keyring::key_get("mts_planning_data_pw")
)

## Load tables ---------
message("Loading raw data from Oracle database")
hh19 <- ROracle::dbReadTable(tbidb, "TBI19_HOUSEHOLD_RAW") %>% as.data.table()
per19 <- ROracle::dbReadTable(tbidb, "TBI19_PERSON_RAW") %>% as.data.table()
trip19 <- ROracle::dbReadTable(tbidb, "TBI19_TRIP_RAW") %>% as.data.table()
veh19 <- ROracle::dbReadTable(tbidb, "TBI19_VEHICLE_RAW") %>% as.data.table()
day19 <- ROracle::dbReadTable(tbidb, "TBI19_DAY_RAW") %>% as.data.table()

hh21 <- ROracle::dbReadTable(tbidb, "TBI21_HOUSEHOLD_RAW") %>% as.data.table()
per21 <- ROracle::dbReadTable(tbidb, "TBI21_PERSON_RAW") %>% as.data.table()
trip21 <- ROracle::dbReadTable(tbidb, "TBI21_TRIP_RAW") %>% as.data.table()
veh21 <- ROracle::dbReadTable(tbidb, "TBI21_VEHICLE_RAW") %>% as.data.table()
day21 <- ROracle::dbReadTable(tbidb, "TBI21_DAY_RAW") %>% as.data.table()

## Translate tables using dictionary -----------
message("Translating tables using dictionary")

dictionary19 <-
  ROracle::dbReadTable(tbidb, "TBI19_DICTIONARY") %>%
  select(-table) %>%
  unique() %>%
  as.data.table()

dictionary21 <-
  ROracle::dbReadTable(tbidb, "TBI21_DICTIONARY") %>%
  select(-table) %>%
  unique() %>%
  as.data.table()

dictionary21 <- rename(dictionary21, "value_label" = label)

# note: this part uses data.table syntax and functions.
translate_tbi <- function(dat, dictionary) {
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
      col_levels <- unique(dictionary$value_label[dictionary$variable == i])
      newdat[, (i) := factor(get(i), levels = col_levels)]
    }
  }
  newdat <- droplevels(newdat)
  return(newdat)
}

message("...person...")
per19 <- translate_tbi(per19, dictionary19)
per21 <- translate_tbi(per21, dictionary21)

message("...hh...")
hh19 <- translate_tbi(hh19, dictionary19)
hh21 <- translate_tbi(hh21, dictionary21)

message("...veh...")
veh19 <- translate_tbi(veh19, dictionary19)
veh21 <- translate_tbi(veh21, dictionary21)

message("...day...")
day19 <- translate_tbi(day19, dictionary19)
day21 <- translate_tbi(day21, dictionary21)

message("...trip...")
trip19 <- translate_tbi(trip19, dictionary19)
trip21 <- translate_tbi(trip21, dictionary21)

# Replace missing with NA -----------
message("Replacing missing values with NA")
# all the numeric codes for missing:
all_missing_codes <-
  rbind(
    dictionary19[grep("Missing", value_label), "value", with = F],
    dictionary21[grep("Missing", value_label), "value", with = F],
    use.names = T)

all_missing_codes <- unique(all_missing_codes$value)

# all the value labels that include missing:
all_missing_labels <-
  rbind(
    dictionary19[grep("Missing", value_label), "value_label", with = F],
    dictionary21[grep("Missing", value_label), "value_label", with = F],
    use.names = T)


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

day19 <- replace_survey_missing(day19)
trip19 <- replace_survey_missing(trip19)
hh19 <- replace_survey_missing(hh19)
per19 <- replace_survey_missing(per19)
veh19 <- replace_survey_missing(veh19)

day21 <- replace_survey_missing(day21)
trip21 <- replace_survey_missing(trip21)
hh21 <- replace_survey_missing(hh21)
per21 <- replace_survey_missing(per21)
veh21 <- replace_survey_missing(veh21)

rm(all_missing_labels, all_missing_vector, all_missing_codes)

# Set IDs as Integer64 -----------
message("IDs as Int64")
hh19[, hh_id := as.integer64(hh_id)]
hh21[, hh_id := as.integer64(hh_id)]

veh19[, hh_id := as.integer64(hh_id)]
veh21[, hh_id := as.integer64(hh_id)]

day19[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]
day21[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
    .SDcols = c("hh_id", "person_id")
]

trip19[, c("hh_id", "person_id", "trip_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id", "trip_id")
]
trip21[, c("hh_id", "person_id", "trip_id") := lapply(.SD, as.integer64),
     .SDcols = c("hh_id", "person_id", "trip_id")
]

per19[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]
per21[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
      .SDcols = c("hh_id", "person_id")
]

## Clean up---------------
rm(replace_survey_missing, translate_tbi, tbidb)
