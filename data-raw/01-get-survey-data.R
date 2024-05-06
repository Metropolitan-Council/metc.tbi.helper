# Load data from local file
# MetC staff only
qs::qload("/Users/whitedba/Desktop/tbi_hh_data/CombinedDataset_2024-04-19/dat_all_upcoded_labeled.qs")
tbi <- dat_all_upcoded_labeled
rm(dat_all_upcoded_labeled)

var_list <- fread("/Users/whitedba/Desktop/tbi_hh_data/CombinedDataset_2024-04-19/variables.csv")
val_list <- fread("/Users/whitedba/Desktop/tbi_hh_data/CombinedDataset_2024-04-19/values.csv")

# # Get data -----------
# # Configure database time zone
# Sys.setenv(TZ = "America/Chicago")
# Sys.setenv(ORA_SDTZ = "America/Chicago")

# RSG sent pre-decoded variables deprecating this code:
# ## connect to database ------------
# # Raw data lives in the Met Council databases and this code
# # will only work for internal employees.
# tbidb <- db_connect()
# # data dictionary --------
# var_list <-
#   dbReadTable(tbidb, "TBI19.21.23_VARIABLE_LIST") %>%
#   as.data.table()
# value_list <-
#   dbReadTable(tbidb, "TBI19.21.23_VALUE_LIST") %>%
#   as.data.table()
# value_list[, (names(value_list)) := lapply(.SD, as.character)]
# value_list[, (names(value_list)) :=
#              lapply(.SD, \(str) fifelse(str == 'Missing', NA, str))]
#
#
# # 2019 tables -------------
# dbListTables(tbidb) %>%
#   str_subset("TBI19_RAW") %>%
#   str_subset("LOCATION", negate = T) %>%
#   lapply(\(table_){
#   message(table_)
#   data_download <- dbReadTable(tbidb, table_) %>% as.data.table()
#   assign(
#     table_ %>%
#       str_to_lower() %>%
#       str_replace("tbi19_raw_", "") %>%
#       str_c("19"),
#     data_download,
#     envir = .GlobalEnv
#   )
# }) %>%
#   invisible()
#
# tables2019 <- c("day19", "hh19", "person19", "trip19", "vehicle19")
#
#
# # * change to unified column names --------------
# lapply(tables2019, \(table_){
#   var_list_2019 <- var_list[!is.na(variable_2019)]
#     get(table_) %>%
#     setnames(var_list_2019$variable_2019,
#              var_list_2019$variable_unified,
#              skip_absent = T)
#   message(table_)
# }) %>%
#   invisible()
#
# # * decode variables -----------------
# # for variables in the value_list, replace the coded level with the
# # human readable level.
#
# # table <- "person19"
#   lapply(tables2019, \(table){
#     message(table)
#     get(table) %>%
#       names %>%
#       lapply(\(name){
#         var_mapping <- value_list[variable_unified == name]
#         # if(name == "income_detailed") browser()
#         if(var_mapping[, .N > 0]){
#           message(table, " - ", name)
#           setnames(var_mapping, "value_2019", name)
#           get(table)[var_mapping, on=name, (name) := i.label_upcoded]
#           # %>%
#                   # .[, (name) := NULL] %>%
#                   # setnames("temp", name %>% paste0())
#         }
#       })
#   }) %>%
#   invisible()
#
# # 2021 tables ------------------
# dbListTables(tbidb) %>%
#   str_subset("TBI21_RAW") %>%
#   str_subset("LOCATION", negate = T) %>%
#   lapply(\(table_){
#     message(table_)
#     data_download <- dbReadTable(tbidb, table_) %>% as.data.table()
#     assign(
#       table_ %>%
#         str_to_lower() %>%
#         str_replace("tbi21_raw_", "") %>%
#         str_c("21"),
#       data_download,
#       envir = .GlobalEnv
#     )
#   }) %>%
#   invisible()
#
# tables2021 <- c("day21", "hh21", "person21", "trip21", "vehicle21")
#
#
# # * change to unified column names --------------
# lapply(tables2021, \(table_){
#   var_list_2021 <- var_list[!is.na(variable_2021)]
#   get(table_) %>%
#     setnames(var_list_2021$variable_2021,
#              var_list_2021$variable_unified,
#              skip_absent = T)
#   message(table_)
# }) %>%
#   invisible()
#
# # * decode variables -----------------
# # for variables in the value_list, replace the coded level with the
# # human readable level.
#
# # table <- "person21"
#   lapply(tables2021, \(table){
#     message(table)
#     get(table) %>%
#       names %>%
#       lapply(\(name){
#         var_mapping <- value_list[variable_unified == name]
#         # if(name == "income_detailed") browser()
#         if(var_mapping[, .N > 0]){
#           message(table, " - ", name)
#           setnames(var_mapping, "value_2021", name)
#           get(table)[var_mapping, on=name, (name) := i.label_upcoded]
#           # %>%
#           # .[, (name) := NULL] %>%
#           # setnames("temp", name %>% paste0())
#         }
#       })
#   }) %>%
#   invisible()
#
# # Set Data types -----------
# # * day ----
# # note: integer64 gets cohersed to char when databasing. So using
# # numeric instead.
# numeric_cols <- c("hh_id", "person_id", 'day_id', 'num_trips', 'telework_time', 'day_weight')
# day19[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
# day21[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
# rm(numeric_cols)
#
# day19[, travel_date := as.Date(travel_date)]
# day21[, travel_date := as.Date(travel_date)]
#
# # * hh ------------
# numeric_cols <- c("hh_id",
#                   "sample_home_lat",
#                   'sample_home_lon',
#                   'sample_home_bg',
#                   'home_lat',
#                   'home_lon',
#                   'home_bg_2010',
#                   'hh_weight'
#                   )
# hh19[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
# hh21[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
# rm(numeric_cols)
#
# date_cols <- c(
#   "first_travel_date",
#   "last_travel_date"
#   )
# hh19[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]
# hh21[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]
# rm(date_cols)
#
# # * vehicle --------
# numeric_cols <- c("hh_id", "year", "home_park_pass_cost")
# vehicle19[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
# numeric_cols <- c("hh_id", "year", "vehicle_id")
# vehicle21[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
# rm(numeric_cols)
#
# # * trip ----
# numeric_cols_19 <- c(
#   "person_id",
#   "trip_num",
#   'trip_id',
#   'hh_id',
#   'leg_num',
#   'linked_trip_num',
#   'o_lat',
#   'o_lon',
#   'o_bg',
#   'd_lat',
#   'd_lon',
#   'd_bg_2010',
#   'duration_minutes',
#   'duration_imputed',
#   'distance_miles',
#   'speed_mph',
#   'vehicle_park_cost',
#   'taxi_cost',
#   'mode_type_p_1',
#   'mode_type_p_2',
#   'mode_type_p_3',
#   'mode_type_p_4',
#   'predicted_mode_probability',
#   'trip_weight',
#   'synthetic_trip'
# )
# trip19[, (numeric_cols_19) := lapply(.SD, as.numeric), .SDcols = numeric_cols_19]
# numeric_cols_21 <- c(
#   "hh_id",
#   "trip_id",
#   "person_id",
#   "day_id",
#   "trip_num",
#   "trip_weight",
#   "depart_minute",
#   "duration_minutes",
#   "arrive_minute",
#   "distance_miles",
#   "speed_mph",
#   "o_lat",
#   "o_lon",
#   "o_bg_2020",
#   "o_puma_2012",
#   "d_lat",
#   "d_lon",
#   "d_bg_2010",
#   "d_bg_2020",
#   "d_puma_2012",
#   "leg_num",
#   "taxi_cost",
#   "linked_trip_id",
#   "leg_id",
#   "vehicle_id"
# )
# trip21[, (numeric_cols_21) := lapply(.SD, as.numeric), .SDcols = numeric_cols_21]
# rm(numeric_cols_19, numeric_cols_21)
#
# trip19[, travel_date := as.Date(travel_date)]
# trip21[, travel_date := as.Date(travel_date)]
#
# # TODO: resume here ---------
#
# trip19[sample(.N, 10), .(paste0(depart_time), depart_time_test)]
# posix_cols <- c(
#   "depart_time",
#   "arrive_time",
#   "depart_time_imputed"
# )
# trip19[, (paste0(posix_cols, "_test")) := lapply(.SD, as.POSIXct), .SDcols = posix_cols]
# trip19[sample(.N, 1), depart_time]
#
#
# # * person -------------
# person19[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
#   .SDcols = c("hh_id", "person_id")
# ]
# person21[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
#   .SDcols = c("hh_id", "person_id")
# ]
#
# person19[, person_weight := as.numeric(person_weight)]
# person21[, person_weight := as.numeric(person_weight)]
#
#
# ## Clean up---------------
# dbDisconnect(tbidb)
# rm(tbidb, tables2019, tables2021)

