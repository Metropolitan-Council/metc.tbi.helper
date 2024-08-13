library(data.table)
library(DBI)
library(srvyr)
library(stringr)
library(plotly)

# load data ----------
con_mts <- dbConnect(odbc::odbc(),
                     dsn = "MTS_Planning_Data",
                     uid = keyring::key_get("councilR.uid"),
                     pwd = keyring::key_get("councilR.pwd")
)

yrs <- c(2019, 2021, 2023)
tables <- c("day", 'trip', 'hh', 'person')
lapply(tables, \(table_){
  temp_yrs <-
    lapply(yrs, \(yr){
      paste(table_, yr) %>% message()
      dbReadTable(con_mts, sprintf("TBI%s_%s", yr, table_))
    }) %>%
    rbindlist()
  assign(table_, temp_yrs, envir = .GlobalEnv)
})

# single response categorical Variable
hh %>%
  # .[survey_year == 2023] %>%
  .[, .N, income_broad] %>%
  .[, pct := 100 * N/sum(N)] %>%
  print
