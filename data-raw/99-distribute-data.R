# This script is writen to run after
# 15-create-dictionary.R

# GitHub --------------------------------------
save(tbi19, file = "data/tbi19.rda")
save(tbi21, file = "data/tbi21.rda")

# Geospatial commons --------------------------------------
# Contact GIS
tbi_desktop_path <- file.path("~/Desktop", "TBI_data_out")
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
    fwrite(tbi21[[table_]],
      file = file.path(tbi21_path
                       , paste0("TravelBehaviorInventory2021"
                                , str_to_title(table_)
                                , '.csv')))
  })


# MTS_Planning DB --------------------------------------
# location file is not modified in this process and in the
# data base as TBI19_RAW_LOCATION
lapply(tbi21_PII, nrow)

db_con <- db_connect()
tbi19_PII %>%
  names() %>%
  setdiff(c("var_list", 'value_list')) %>%
  lapply(\(table_){
    table_name <- paste0("TBI19_FINAL_", str_to_upper(table_))
    if (!dbExistsTable(db_con, table_name)) {
      message(table_)
      n <- 5000
      nr <- nrow(tbi19_PII[[table_]])
      table_subsets <- split(tbi19_PII[[table_]], rep(1:ceiling(nr/n), each=n, length.out=nr))

      i <- 1
      N <- length(table_subsets)
      lapply(table_subsets, \(table_ss_){
        message(table_, ' ', i, " of ", N)
        dbWriteTable(db_con, name = table_name, value = table_ss_, append = T)
        i <<- i + 1
      })
    }
  })


# 2021
# location file is not modified in this process and in the
# data base as TBI21_RAW_LOCATION
tbi21_PII %>%
  names() %>%
  setdiff(c("var_list", 'value_list')) %>%
  lapply(\(table_){
    table_name <- paste0("TBI21_FINAL_", str_to_upper(table_))
    if (!dbExistsTable(db_con, table_name)) {
      message(table_)
      n <- 5000
      nr <- nrow(tbi21_PII[[table_]])
      table_subsets <- split(tbi21_PII[[table_]], rep(1:ceiling(nr/n), each=n, length.out=nr))

      i <- 1
      N <- length(table_subsets)
      lapply(table_subsets, \(table_ss_){
        message(table_, ' ', i, " of ", N)
        dbWriteTable(db_con, name = table_name, value = table_ss_, append = T)
        i <<- i + 1
      })
    }
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




dbListTables(db_con) %>%
  str_subset("TBI") %>%
  str_subset("OBS", T) %>%
  str_subset("v_", T) %>%
  str_subset("TOUR", T) %>%
  paste0(collapse = '\n') %>% cat
