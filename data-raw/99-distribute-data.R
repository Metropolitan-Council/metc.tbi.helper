# This script is writen to run after
# 15-create-dictionary.R

tbi_desktop_path <- file.path(key_get('desktop'), 'TBI_data')
dir.create(tbi_desktop_path)

# repo data --------------------------------------
save(tbi19_rmPII, file = "data/tbi19.rda")
save(tbi21_rmPII, file = "data/tbi21.rda")

# geo-spatial commons (GIS folks) data --------------------------------------
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


# files for internal SQL database --------------------------------------
con <- db_connect()
tbi19 %>%
  names %>%
  lapply(\(table_){
    dbWriteTable(con, str_c("TBI19_", table_ %>% str_to_upper()), tbi19[[table_]])
  })


tbi21 %>%
  names %>%
  lapply(\(table_){
    dbWriteTable(con, str_c("TBI21_", table_ %>% str_to_upper()), tbi21[[table_]])
  })
dbDisconnect(con)

# files for National Renewable Energy Lab --------------------------------------


# files N drive --------------------------------------
# Hold on this... I dont think we need this
# many duplications of the data.

