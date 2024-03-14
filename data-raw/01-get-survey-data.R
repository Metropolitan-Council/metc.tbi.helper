# Get data -----------
# Configure database time zone
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")

## connect to database ------------
# Raw data lives in the Met Council databases and this code
# will only work for internal employees.
tbidb <- db_connect()

# data dictionary --------
var_list <-
  dbReadTable(tbidb, "TBI19.21.23_VARIABLE_LIST") %>%
  as.data.table()
value_list <-
  dbReadTable(tbidb, "TBI19.21.23_VALUE_LIST") %>%
  as.data.table()
value_list[, (names(value_list)) := lapply(.SD, as.character)]
value_list[, (names(value_list)) :=
             lapply(.SD, \(str) fifelse(str == 'Missing', NA, str))]


# 2019 tables -------------
dbListTables(tbidb) %>%
  str_subset("TBI19_RAW") %>%
  str_subset("LOCATION", negate = T) %>%
  lapply(\(table_){
  message(table_)
  data_download <- dbReadTable(tbidb, table_) %>% as.data.table()
  assign(
    table_ %>%
      str_to_lower() %>%
      str_replace("tbi19_raw_", "") %>%
      str_c("19"),
    data_download,
    envir = .GlobalEnv
  )
}) %>%
  invisible()

tables2019 <- c("day19", "hh19", "person19", "trip19", "vehicle19")


# * change to unified column names --------------
lapply(tables2019, \(table_){
  var_list_2019 <- var_list[!is.na(variable_2019)]
    get(table_) %>%
    setnames(var_list_2019$variable_2019,
             var_list_2019$variable_unified,
             skip_absent = T)
  message(table_)
}) %>%
  invisible()

# * decode variables -----------------
# for variables in the value_list, replace the coded level with the
# human readable level.

# table <- "person19"
  lapply(tables2019, \(table){
    message(table)
    get(table) %>%
      names %>%
      lapply(\(name){
        var_mapping <- value_list[variable_unified == name]
        # if(name == "income_detailed") browser()
        if(var_mapping[, .N > 0]){
          message(table, " - ", name)
          setnames(var_mapping, "value_2019", name)
          get(table)[var_mapping, on=name, (name) := i.label_upcoded]
          # %>%
                  # .[, (name) := NULL] %>%
                  # setnames("temp", name %>% paste0())
        }
      })
  }) %>%
  invisible()

# 2021 tables ------------------
dbListTables(tbidb) %>%
  str_subset("TBI21_RAW") %>%
  str_subset("LOCATION", negate = T) %>%
  lapply(\(table_){
    message(table_)
    data_download <- dbReadTable(tbidb, table_) %>% as.data.table()
    assign(
      table_ %>%
        str_to_lower() %>%
        str_replace("tbi21_raw_", "") %>%
        str_c("21"),
      data_download,
      envir = .GlobalEnv
    )
  }) %>%
  invisible()

tables2021 <- c("day21", "hh21", "person21", "trip21", "vehicle21")


# * change to unified column names --------------
lapply(tables2021, \(table_){
  var_list_2021 <- var_list[!is.na(variable_2021)]
  get(table_) %>%
    setnames(var_list_2021$variable_2021,
             var_list_2021$variable_unified,
             skip_absent = T)
  message(table_)
}) %>%
  invisible()

# * decode variables -----------------
# for variables in the value_list, replace the coded level with the
# human readable level.

# table <- "person21"
  lapply(tables2021, \(table){
    message(table)
    get(table) %>%
      names %>%
      lapply(\(name){
        var_mapping <- value_list[variable_unified == name]
        # if(name == "income_detailed") browser()
        if(var_mapping[, .N > 0]){
          message(table, " - ", name)
          setnames(var_mapping, "value_2021", name)
          get(table)[var_mapping, on=name, (name) := i.label_upcoded]
          # %>%
          # .[, (name) := NULL] %>%
          # setnames("temp", name %>% paste0())
        }
      })
  }) %>%
  invisible()

# Set IDs as Integer64 -----------
hh19[, hh_id := as.integer64(hh_id)]
hh21[, hh_id := as.integer64(hh_id)]

hh19[, hh_weight := as.numeric(hh_weight)]
hh21[, hh_weight := as.numeric(hh_weight)]

vehicle19[, hh_id := as.integer64(hh_id)]
vehicle21[, hh_id := as.integer64(hh_id)]

day19[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]
day21[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]

day19[, day_weight := as.numeric(day_weight)]
day21[, day_weight := as.numeric(day_weight)]

trip19[, c("hh_id", "person_id", "trip_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id", "trip_id")
]
trip21[, c("hh_id", "person_id", "trip_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id", "trip_id")
]

trip19[, trip_weight := as.numeric(trip_weight)]
trip21[, trip_weight := as.numeric(trip_weight)]
trip19[, distance_miles := distance_miles %>% as.numeric()]
trip21[, distance_miles := distance_miles %>% as.numeric()]
trip19[, trip_num := trip_num %>% as.numeric()]
trip21[, trip_num := trip_num %>% as.numeric()]

person19[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]
person21[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]

person19[, person_weight := as.numeric(person_weight)]
person21[, person_weight := as.numeric(person_weight)]


## Clean up---------------
dbDisconnect(tbidb)
rm(tbidb, tables2019, tables2021)

