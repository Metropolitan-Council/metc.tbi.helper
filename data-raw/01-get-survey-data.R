# This script is writen to run after
# 00-load-pkgs.R

# Get data -----------
# Configure database time zone
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")

## connect to database ------------
# Raw data lives in the Met Council databases and this code
# will only work for internal employees.
tbidb <- db_connect()

# 2019 tables -------------
tables2019 <- c(
  "TBI19_DAY_RAW",
  "TBI19_HOUSEHOLD_RAW",
  "TBI19_LOCATION_RAW",
  "TBI19_PERSON_RAW",
  "TBI19_TRIP_RAW",
  "TBI19_VEHICLE_RAW",
  "TBI19_DICTIONARY_RAW",
  "TBI_MODE_CONFLATION"
)

lapply(tables2019, \(table_){
  message(table_)
  data_download <- dbReadTable(tbidb, table_) %>% as.data.table()
  assign(
    table_ %>%
      str_to_lower() %>%
      str_replace("tbi19_", "") %>%
      str_replace("_raw", "") %>%
      str_c("19"),
    data_download,
    envir = .GlobalEnv
  )
})

tables2019 <- c("day19", "household19", "location19", "person19", "trip19", "vehicle19")
dictionary19[, table := table %>% str_to_lower()]
dictionary19[, value := as.integer(value)]
dictionary19[str_detect(value_label, "Missing"), value_label := NA]

dictionary19[, survey_question := survey_question %>% str_replace_all("â€™", "'")]

# FIXME: These columns are missing. Emailed RSG to get a copy of them.
dictionary19 <- dictionary19[!variable %in% c("home_park_pass_period", "provided_text_name")]

# for variables in the dictionary, replace the coded level with the
# human readable level.
dictionary19[, unique(table)] %>%
  lapply(\(table_){
    dictionary19[table == table_, unique(variable)] %>%
      lapply(\(var_){
        tempLookup <- dictionary19[table == table_ & variable == var_]
        setnames(tempLookup, "value", var_)

        table_ %>%
          str_c(19) %>%
          get() %>%
          .[tempLookup, on = var_, temp := value_label] %>%
          .[, (var_) := NULL] %>%
          setnames("temp", var_)
      })
  })

# 2021 tables ------------------
tables2021 <- c(
  "TBI21_DAY_RAW",
  "TBI21_HOUSEHOLD_RAW",
  "TBI21_LOCATION_RAW",
  "TBI21_PERSON_RAW",
  "TBI21_TRIP_RAW",
  "TBI21_VEHICLE_RAW",
  "TBI21_DICTIONARY_RAW",
  "TBI_MODE_CONFLATION"
)

lapply(tables2021, \(table_){
  message(table_)
  data_download <- dbReadTable(tbidb, table_) %>% as.data.table()
  assign(
    table_ %>%
      str_to_lower() %>%
      str_replace("tbi21_", "") %>%
      str_replace("_raw", "") %>%
      str_c("21"),
    data_download,
    envir = .GlobalEnv
  )
})

tables2021 <- c("day21", "household21", "location21", "person21", "trip21", "vehicle21")
dictionary21[, table := table %>% str_to_lower()]
dictionary21[, value := as.integer(value)]
dictionary21 <- dictionary21[!is.na(value)]
dictionary21[str_detect(label, "Missing"), label := NA]

# for variables in the dictionary, replace the coded level with the
# human readable level.
dictionary21[, unique(table)] %>%
  lapply(\(table_){
    dictionary21[table == table_, unique(variable)] %>%
      lapply(\(var_){
        tempLookup <- dictionary21[table == table_ & variable == var_]
        setnames(tempLookup, "value", var_)

        table_ %>%
          str_c(21) %>%
          get() %>%
          .[tempLookup, on = var_, temp := label] %>%
          .[, (var_) := NULL] %>%
          setnames("temp", var_)
      })
  })


# Set IDs as Integer64 -----------
household19[, hh_id := as.integer64(hh_id)]
household21[, hh_id := as.integer64(hh_id)]

vehicle19[, hh_id := as.integer64(hh_id)]
vehicle21[, hh_id := as.integer64(hh_id)]

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

person19[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]
person21[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]

## Clean up---------------
dbDisconnect(tbidb)
rm(tbidb, tables2019, tables2021)
