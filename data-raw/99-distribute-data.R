# N Drive -
# RSG's raw data delivery is kept in the N drive and on oneDrive
# See Brandon Whited for access and specific location.

# read function
source("data-raw/_helper_function.R")

# GitHub --------------------------------------
# save(tbi_rmPII, file = "data/tbi.rda")
# qs::qsave(tbi_rmPII, "data/tbi.qs") # file too big

# make output file -----------------------------
output_file <- file.path('~/Desktop', "TBI_data_out")
output_file %>% dir.create()

# one drive / local inst---------
tbi_oneDrive_path <- file.path(output_file, "oneDrive")
dir.create(tbi_oneDrive_path)
qsave(tbi, file.path(tbi_oneDrive_path, "tbi_upcoded_2019_2021_2023.qs"))

tbi_local_path <- file.path(output_file, "local")
dir.create(tbi_local_path)
qsave(tbi, file.path(tbi_local_path, "tbi_upcoded_2019_2021_2023.qs"))

# Geospatial commons --------------------------------------
# Contact GIS
# tbi_desktop_path <- file.path("Desktop", "TBI_data_out")

tbi_geospatialCommons_path <- file.path(output_file, "geospatial_commons")
dir.create(tbi_geospatialCommons_path)

# output tables --------
tables <- names(tbi_rmPII) %>% str_subset("meta", negate = T)
years <- c("2019", "2021", "2023")
lapply(tables, \(tab){
  lapply(years, \(yr){
    tbi_yr_path <- file.path(tbi_geospatialCommons_path, paste0("tbi", yr))
    if (!dir.exists(tbi_yr_path)) dir.create(tbi_yr_path)
    fwrite(tbi_rmPII[[tab]][survey_year == yr],
      file = file.path(
        tbi_yr_path,
        sprintf("TravelBehaviorInventory%s_%s.csv", yr, tab %>% str_to_upper())
      )
    )
  })
})

tbi_geospatialCommons_path %>%
  list.files() %>%
  lapply(\(file){
    fwrite(
      tbi$metaData_variables,
      file.path(tbi_geospatialCommons_path, file, "TravelBehaviorInventory_meta_data_variables.csv")
    )
    fwrite(
      tbi$metaData_values,
      file.path(tbi_geospatialCommons_path, file, "TravelBehaviorInventory_meta_data_values.csv")
    )
  })


# MTS_Planning DB --------------------------------------
# save to database. The process is faster if we do it in smaller chuncks
tables <- names(tbi) %>% str_subset("meta", negate = T)
years <- c(2019, 2021, 2023)
db_con <- dbConnect(odbc::odbc(),
  dsn = "MTS_Planning_Data",
  uid = keyring::key_get("councilR.uid"),
  pwd = keyring::key_get("councilR.pwd")
)

# tab <- "trip"
# yr <- 2023
lapply(tables, \(tab){
  lapply(years, \(yr){
    table_name <- sprintf("TBI%s_%s", yr, tab %>% str_to_upper()) %>% print()

    if (!dbExistsTable(db_con, table_name)) {
      message(tab, yr)

      n <- 1000
      nr <- nrow(tbi[[tab]][survey_year == yr])
      table_subsets <- split(
        tbi[[tab]][survey_year == yr],
        rep(1:ceiling(nr / n),
          each = n,
          length.out = nr
        )
      )

      # create an empty table using the function created in script 15
      create_table(db_con, tab, table_name)

      i <- 1
      N <- length(table_subsets)
      lapply(table_subsets, \(table_ss_){
        message(tab, " ", i, " of ", N)
        dbWriteTable(db_con, name = table_name, value = table_ss_, append = T)
        i <<- i + 1
      })
    }
  })
})

dbWriteTable(db_con, "TBI2019_2023_META_DATA_VARIABLES", tbi$metaData_variables)
dbWriteTable(db_con, "TBI2019_2023_META_DATA_VALUES", tbi$metaData_values)
dbDisconnect(db_con)

# NREL -----
# TODO: parse and send data to NREL.
tbi
