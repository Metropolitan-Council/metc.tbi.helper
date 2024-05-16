# This script is writen to run after
# 15-create-dictionary.R

# GitHub --------------------------------------
save(tbi_rmPII, file = "data/tbi.rda")

# Geospatial commons --------------------------------------
# Contact GIS
tbi_desktop_path <- file.path("~/Desktop", "TBI_data_out")
dir.create(tbi_desktop_path)

tbi_geospatialCommons_path <- file.path(tbi_desktop_path, "geospatial_commons")
dir.create(tbi_geospatialCommons_path)

# remove emojis and special charactors from the data.
lapply(tbi_rmPII, \(tab){
  cols <- tab %>%
    sapply(typeof) %>%
    str_detect("character")
  col_names <- names(tab)[cols]
  tab[, c(col_names) :=
    lapply(.SD, str_replace_all, pattern = "[^\x01-\xFF]", replacement = ""),
  .SDcols = cols
  ]
})

# output tables --------
tables <- names(tbi_rmPII)
# years <- tbi_rmPII$hh$survey_year %>% unique()
years <- c(2019, 2021)
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

# MTS_Planning DB --------------------------------------
# remove emojis and special charactors from the data.
# tab <- copy(tbi$trip)
lapply(tbi, \(tab){
  cols <- tab %>%
    sapply(typeof) %>%
    str_detect("character")
  col_names <- names(tab)[cols]
  tab[, c(col_names) :=
    lapply(.SD, str_replace_all, pattern = "[^\x01-\xFF]", replacement = ""),
  .SDcols = cols
  ]
})


# save to database. The process is faster if we do it in smaller chuncks
db_con <- db_connect()
# tab <- "trip"
# yr <- 2023
lapply(tables, \(tab){
  lapply(years, \(yr){
    table_name <- sprintf("TBI%s_%s", yr, tab %>% str_to_upper()) %>% print()

    if (!dbExistsTable(db_con, table_name)) {
      message(tab, yr)

      n <- 5000
      nr <- nrow(tbi[[tab]][survey_year == yr])
      table_subsets <- split(
        tbi[[tab]][survey_year == yr],
        rep(1:ceiling(nr / n),
          each = n,
          length.out = nr
        )
      )

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
dbDisconnect(db_con)

# N Drive -----------------
# put data in the N drive

# fwrite(table_subsets[['2915']], '~/Desktop/test.csv')
# for(i in 1:nrow(table_subsets[['583']])){
#   message(i)
#   DBI::dbWriteTable(db_con, name = table_name, value = table_subsets[['583']][i], append = T)
# }
# DBI::dbWriteTable(db_con, name = table_name, value = table_subsets[['583']], append = T)


# # MTS_Planning DB --------------------------------------
# # location file is not modified in this process and in the
# # data base as TBI19_RAW_LOCATION
# db_con <- db_connect()
# tbi19_PII %>%
#   names() %>%
#   setdiff(c("var_list", 'value_list')) %>%
#   lapply(\(table_){
#     table_name <- paste0("TBI19_FINAL_", str_to_upper(table_))
#     if (!dbExistsTable(db_con, table_name)) {
#       message(table_)
#       n <- 5000
#       nr <- nrow(tbi19_PII[[table_]])
#       table_subsets <- split(tbi19_PII[[table_]], rep(1:ceiling(nr/n), each=n, length.out=nr))
#
#       i <- 1
#       N <- length(table_subsets)
#       lapply(table_subsets, \(table_ss_){
#         message(table_, ' ', i, " of ", N)
#         dbWriteTable(db_con, name = table_name, value = table_ss_, append = T)
#         i <<- i + 1
#       })
#     }
#   })
#
#
# # 2021
# # location file is not modified in this process and in the
# # data base as TBI21_RAW_LOCATION
# tbi21_PII %>%
#   names() %>%
#   setdiff(c("var_list", 'value_list')) %>%
#   lapply(\(table_){
#     table_name <- paste0("TBI21_FINAL_", str_to_upper(table_))
#     if (!dbExistsTable(db_con, table_name)) {
#       message(table_)
#       n <- 5000
#       nr <- nrow(tbi21_PII[[table_]])
#       table_subsets <- split(tbi21_PII[[table_]], rep(1:ceiling(nr/n), each=n, length.out=nr))
#
#       i <- 1
#       N <- length(table_subsets)
#       lapply(table_subsets, \(table_ss_){
#         message(table_, ' ', i, " of ", N)
#         dbWriteTable(db_con, name = table_name, value = table_ss_, append = T)
#         i <<- i + 1
#       })
#     }
#   })



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
      file = file.path(
        tbi19_path,
        paste0(
          "TravelBehaviorInventory2019",
          str_to_title(table_),
          ".csv"
        )
      )
    )
  })

# 2021
tbi21_path <- file.path(tbi_database_NREL, "tbi21_PII")
dir.create(tbi21_path)

tbi21_PII %>%
  names() %>%
  lapply(\(table_){
    fwrite(tbi21_PII[[table_]],
      file = file.path(
        tbi21_path,
        paste0(
          "TravelBehaviorInventory2019",
          str_to_title(table_),
          ".csv"
        )
      )
    )
  })




dbListTables(db_con) %>%
  str_subset("TBI") %>%
  str_subset("OBS", T) %>%
  str_subset("v_", T) %>%
  str_subset("TOUR", T) %>%
  paste0(collapse = "\n") %>%
  cat()


lapply(names(tbi), \(dt_name){
  dt <- copy(tbi[[dt_name]])
  lapply(c(2019, 2021), \(yr){
    message(
      "table: ", dt_name, " yr: ", yr, " nrow: ",
      dt[survey_year == yr, .N %>% prettyNum(",")]
    )
  })
})
