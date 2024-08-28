library(DBI)
library(data.table)


con_mts <- dbConnect(odbc::odbc(),
  dsn = "MTS_Planning_Data",
  uid = keyring::key_get("councilR.uid"),
  pwd = keyring::key_get("councilR.pwd")
  )

# TBI onbord data ----------
obs16 <- dbReadTable(con_mts, "TBI_OBS_2016") %>%
  as.data.table
obs16[, .(total_transfers, unlinked_wght_fctr, linked_wght_fctr)] %>% sample_n(10)

obs21 <- dbReadTable(con_mts, "TBI_OBS_2021_PILOT_OD") %>%
  as.data.table
obs21[sample(.N, 10), .(system_transfers, linked_wght_fctr, unlinked_wght_fctr)]

obs22 <- dbReadTable(con_mts, "TBI_OBS_2022") %>%
  as.data.table
obs22[sample(.N, 10), .(system_transfers, linked_weight, unlinked_weight)]


# ridership data ----------------
con_psh <- if (Sys.info()['sysname'] == 'Linux') {
  dbConnect(odbc::odbc(), database = 'serv_hist', server = 'azdbsqlcl07.mc.local,1433', driver = 'ODBC Driver 11 for SQL Server', uid = 'RShiny', pwd = Sys.getenv('ODBC_RSHINY'), timezone_out = "America/Chicago")
} else {
  dbConnect(odbc::odbc(), 'db_prod_serv_hist', uid = Sys.getenv('ODBC_UID'), pwd = Sys.getenv('ODBC_PWD'), timezone_out = "America/Chicago")
}
odbc::odbcSetTransactionIsolationLevel(con_psh, 'read_uncommitted')

ridership <- dbGetQuery(con_psh, "select * from New_DailySummaryWithRidership
                        where nyear > 2018") %>%
  as.data.table()

ridership <- dbReadTable(con_psh, "t_Official_Daily_Ridership") %>%
  as.data.table()
ridership[, date := as.Date(dtDate)]
ridership[, year := year(dtDate)]
ridership[, wday := lubridate::wday(dtDate, label = T, abbr = T)]
ridership[, route %>% unique() %>% sort]

ridership[
  year %in% c(2019, 2021, 2023) & wday %in% c('Mon', 'Tue', 'Wed', 'Thu'),
  .(rides = sum(rides)),
  keyby = .(date, year)
] %>%
  .[, .(avg_wkday = mean(rides)), year] %>%
  fwrite("contractor_data_requests/MT_ridership_noContacted.csv")

ridership[,
  .(rides = sum(rides)),
  keyby = .(year)
] %>%
  fwrite("contractor_data_requests/MT_ridership_totals_noContacted.csv")

obs21[, route %>% unique()]

obs <-
  obs16[, .(
    year = 2016,
    total_unliked_trips = sum(unlinked_wght_fctr) %>% round,
    total_linked_trips = sum(linked_wght_fctr) %>% round,
    routes = route %>% unique() %>% paste(collapse = ',')
  )] %>%
  rbind(
    obs21[, .(
      year = 2021,
      total_unliked_trips = sum(unlinked_wght_fctr) %>% round,
      total_linked_trips = sum(linked_wght_fctr) %>% round,
      routes = route %>% unique() %>% paste(collapse = ',')
    )]
  ) %>%
  rbind(
    obs22[, .(
      year = 2022,
      total_unliked_trips = sum(unlinked_weight) %>% round,
      total_linked_trips = sum(linked_weight) %>% round,
      routes = route %>% unique() %>% paste(collapse = ',')
    )]
  ) %>%
  print
fwrite(obs, "contractor_data_requests/obs.csv")

# transfers ---
obs22[, .N, system_transfers] %>%
  fwrite("contractor_data_requests/transfers.csv")
obs22[, mean(system_transfers)]





