# This script is writen to run after
# 15-create-dictionary.R

# Git Hub --------------------------------------
save(tbi19_rmPII, file = "data/tbi19.rda")
save(tbi21_rmPII, file = "data/tbi21.rda")


# Geo-spatial commons --------------------------------------
# Contact GIS
tbi_desktop_path <- file.path(key_get('desktop'), 'TBI_data')
dir.create(tbi_desktop_path)

tbi_geospatialCommons_path <- file.path(tbi_desktop_path, "geospatial_commons")
dir.create(tbi_geospatialCommons_path)

# 2019
tbi19_path <- file.path(tbi_geospatialCommons_path, "tbi19")
dir.create(tbi19_path)

tbi19_rmPII %>%
  names %>%
  lapply(\(table_){
    fwrite(tbi19_rmPII[[table_]]
           , file = file.path(tbi19_path, table_) %>% str_c('.csv')
           )
  })

# 2021
tbi21_path <- file.path(tbi_geospatialCommons_path, "tbi21")
dir.create(tbi21_path)

tbi21_rmPII %>%
  names %>%
  lapply(\(table_){
    fwrite(tbi21_rmPII[[table_]]
           , file = file.path(tbi21_path, table_) %>% str_c('.csv')
    )
  })



# MTS_Planning DB --------------------------------------

db_con <- db_connect()
tbi19 %>%
  names %>%
  setdiff('location') %>%
  lapply(\(table_){
    message(table_)
    tictoc::tic()
    dbWriteTable(
      db_con,
      name = paste0('TBI19_', str_to_upper(table_)),
      value = tbi19[[table_]]
    )
    tictoc::toc()
  })

hh_i <- 1
tictoc::tic()
tbi19$location[, unique(hh_id)] %>%
  lapply(\(hh_){
    cat('\014')
    message(hh_i, ' of ', tbi19$location[, uniqueN(hh_id)])
    dbWriteTable(
      db_con,
      name = 'TBI19_LOCATION',
      value = tbi19$location[hh_id == hh_],
      append = T
    )
    hh_i <<- hh_i+1
  })
tictoc::toc()

# 2021
tbi21 %>%
  names %>%
  setdiff('location') %>%
  lapply(\(table_){
    message(table_)
    tictoc::tic()
    dbWriteTable(
      db_con,
      name = paste0('TBI21_', str_to_upper(table_)),
      value = tbi21[[table_]]
    )
    tictoc::toc()
  })


dbDisconnect(db_con)
tictoc::tic()
db_con <- db_connect()
hh_i <- 1
tbi21$location[, ind := rep(1:4426, each = 760)]
tbi21$location[, unique(ind)] %>%
  lapply(\(i){
    message(hh_i, ' of ', tbi21$location[, uniqueN(ind)] )
    dbWriteTable(
      db_con,
      name = 'TBI21_LOCATION',
      value = tbi21$location[ind == i, -c('ind')],
      append = T
    )
    hh_i <<- hh_i+1
  })
tictoc::toc()

tbi21$location[, .N] %>%prettyNum(',')


# National Renewable Energy Laboratory -----
# Work with Cemal.akcicek@nrel.gov and joseph.fish@nrel.gov
tbi_database_NREL <- file.path(tbi_desktop_path, "database_NREL")
dir.create(tbi_database_NREL)

# 2019
tbi19_path <- file.path(tbi_database_NREL, "tbi19")
dir.create(tbi19_path)

tbi19 %>%
  names %>%
  lapply(\(table_){
    fwrite(tbi19[[table_]]
           , file = file.path(tbi19_path, table_) %>% str_c('.csv')
    )
  })

# 2021
tbi21_path <- file.path(tbi_database_NREL, "tbi21")
dir.create(tbi21_path)

tbi21 %>%
  names %>%
  lapply(\(table_){
    fwrite(tbi21[[table_]]
           , file = file.path(tbi21_path, table_) %>% str_c('.csv')
    )
  })


