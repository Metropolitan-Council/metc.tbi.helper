# This script is writen to run after
# 15-create-dictionary.R

# GitHub --------------------------------------
save(tbi19, file = "data/tbi19.rda")
save(tbi21, file = "data/tbi21.rda")

# Geospatial commons --------------------------------------
# Contact GIS
tbi_desktop_path <- file.path(key_get("desktop"), "TBI_data")
dir.create(tbi_desktop_path)

tbi_geospatialCommons_path <- file.path(tbi_desktop_path, "geospatial_commons")
dir.create(tbi_geospatialCommons_path)

# 2019
tbi19_path <- file.path(tbi_geospatialCommons_path, "tbi19")
dir.create(tbi19_path)

tbi19 %>%
  names() %>%
  lapply(\(table_){
    fwrite(tbi19[[table_]],
      file = file.path(tbi19_path
                       , paste0("TravelBehaviorInventory2019"
                                , str_to_title(table_)
                                , '.csv')))
  })

# 2021
tbi21_path <- file.path(tbi_geospatialCommons_path, "tbi21")
dir.create(tbi21_path)

tbi21 %>%
  names() %>%
  lapply(\(table_){
    fwrite(tbi19[[table_]],
      file = file.path(tbi19_path
                       , paste0("TravelBehaviorInventory2019"
                                , str_to_title(table_)
                                , '.csv')))
  })

# Geospatial metadata ----------
# this'll just give a nice list of field to be copy and pasted in to the metadata tool.
tbi19$dictionary[
  variable_label %>%
    unique() %>%
    paste(collapse = '

          ') %>%
    cat]
tbi21$dictionary[
  description %>%
    unique() %>%
    paste(collapse = '\n') %>%
    cat]

# MTS_Planning DB --------------------------------------

db_con <- db_connect()
tbi19_PII %>%
  names() %>%
  setdiff("location") %>%
  lapply(\(table_){
    table_name <- paste0("TBI19_", str_to_upper(table_))
    if (!dbExistsTable(db_con, table_name)) {
      cli::progress_message(table_)
      dbWriteTable(db_con, name = table_name, value = tbi19_PII[[table_]])
    }
  })

# since the location table is so big, it works better
# to up load it in chunks.
hh_i <- 1
tbi19_PII$location[, unique(hh_id)] %>%
  lapply(\(hh_){
    if (!dbExistsTable(db_con, "TBI19_LOCATION")) {
      cat("\014")
      cli::progress_message(hh_i, " of ", tbi19_PII$location[, uniqueN(hh_id)])
      dbWriteTable(
        db_con,
        name = "TBI19_LOCATION",
        value = tbi19_PII$location[hh_id == hh_],
        append = T
      )
    }
    hh_i <<- hh_i + 1
  })


# 2021
tbi21_PII %>%
  names() %>%
  setdiff("location") %>%
  lapply(\(table_){
    table_name <- paste0("TBI21_", str_to_upper(table_))
    if (!dbExistsTable(db_con, table_name)) {
      cli::progress_message(table_)
      dbWriteTable(db_con, name = table_name, value = tbi21_PII[[table_]])
    }
  })

# since the location table is so big, it works better
# to up load it in chunks.
hh_i <- 1
tbi21_PII$location[, unique(ind)] %>%
  lapply(\(i){
    if (!dbExistsTable(db_con, "TBI21_LOCATION")) {
      cli::progress_message(hh_i, " of ", tbi21_PII$location[, uniqueN(ind)])
      dbWriteTable(
        db_con,
        name = "TBI21_LOCATION",
        value = tbi21_PII$location[ind == i, -c("ind")],
        append = T
      )
    }
    hh_i <<- hh_i + 1
  })



# National Renewable Energy Laboratory -----
# Work with Cemal.akcicek@nrel.gov and joseph.fish@nrel.gov
tbi_database_NREL <- file.path(tbi_desktop_path, "database_NREL")
dir.create(tbi_database_NREL)

# 2019
tbi19_path <- file.path(tbi_database_NREL, "tbi19_PII")
dir.create(tbi19_path)

tbi19_PII %>%
  names() %>%
  lapply(\(table_){
    fwrite(tbi19_PII[[table_]],
           file = file.path(tbi19_path
                            , paste0("TravelBehaviorInventory2019"
                                     , str_to_title(table_)
                                     , '.csv')))
  })

# 2021
tbi21_path <- file.path(tbi_database_NREL, "tbi21_PII")
dir.create(tbi21_path)

tbi21_PII %>%
  names() %>%
  lapply(\(table_){
    fwrite(tbi21_PII[[table_]],
           file = file.path(tbi21_path
                            , paste0("TravelBehaviorInventory2019"
                                     , str_to_title(table_)
                                     , '.csv')))
  })

