source("presentation/_load_libraries.R")

# con_mtsp <- DBI::dbConnect(odbc::odbc(), Driver = "FreeTDS",
#   Database = "MTS_Planning_Data", Uid = keyring::key_get("councilR.uid"),
#   Pwd = keyring::key_get("councilR.pwd"), Server = "dbsqlcl11t.test.local,65414",
#   timezone_out = "America/Chicago")

con_mtsp <- dbConnect(odbc::odbc(),
                      dsn = "MTS_Planning_Data",
                      uid = keyring::key_get("councilR.uid"),
                      pwd = keyring::key_get("councilR.pwd")
)
odbc::odbcSetTransactionIsolationLevel(con_mtsp, 'read_uncommitted')

# list tables
tables <-
  dbListTables(con_mtsp) %>%
  stringr::str_subset("^TBI") %>%
  stringr::str_subset("OBS", negate = T) %>%
  stringr::str_subset("CONF", negate = T)

tbi <-
  lapply(tables, function(x) {
    print(x)
    dbReadTable(con_mtsp, x) %>% as.data.table()
  })

names(tbi) <- tables

dbDisconnect(con_mtsp)
rm(tables, con_mtsp)

trip <- rbind(tbi$TBI2019_TRIP, tbi$TBI2021_TRIP, tbi$TBI2023_TRIP)
trip[survey_year == 2019, linked_trip_id := paste0(person_id, "_", linked_trip_num)]
household <- rbind(tbi$TBI2019_HH, tbi$TBI2021_HH, tbi$TBI2023_HH)
person <- rbind(tbi$TBI2019_PERSON, tbi$TBI2021_PERSON, tbi$TBI2023_PERSON)
day <- rbind(tbi$TBI2019_DAY, tbi$TBI2021_DAY, tbi$TBI2023_DAY)


# # gtfs 2019
# tempService <- data.table(service_id = serviceOnDate("2019-11-12", gtfs = "data/gtfs_2019.zip"))
# tempTrips <- readGTFS("trips", 'data/gtfs_2019.zip')[tempService, on="service_id"] %>%
#   .[,  route_id := route_id %>% str_replace_all("-112", "")] %>%
#   .[, .N, .(route_id, shape_id)] %>%
#   .[!route_id %in% c(888, 887)] %>% print
# transit_access_2019 <- readGTFS("shapes", 'data/gtfs_2019.zip')[tempTrips, on=.(shape_id)] %>%
#   .[, .(route_id = list(unique(route_id))), .(shape_pt_lat, shape_pt_lon)] %>%
#   na.omit() %>%
#   st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
#   st_transform(26915) %>%
#   st_union() %>%
#   st_buffer(402) %>%
#   st_transform(4326)
# saveRDS(transit_access_2019, 'data/transit_access_2019.RDS')
# rm(tempService, tempTrips)
# transit_access_2019 <- readRDS("data/transit_access_2019.RDS")

# # gtfs 2021
# tempService <- data.table(service_id = serviceOnDate("2021-11-15", gtfs = "data/gtfs_2021.zip"))
# tempTrips <- readGTFS("trips", 'data/gtfs_2021.zip')[tempService, on="service_id"] %>%
#   # .[,  route_id := route_id %>% str_replace_all("-112", "")] %>%
#   .[, .N, .(route_id, shape_id)] %>%
#   .[!route_id %in% c(888, 887)]
# transit_access_2021 <- readGTFS("shapes", 'data/gtfs_2021.zip')[tempTrips, on=.(shape_id)] %>%
#   .[, .(route_id = list(unique(route_id))), .(shape_pt_lat, shape_pt_lon)] %>%
#   na.omit() %>%
#   st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
#   st_transform(26915) %>%
#   st_union() %>%
#   st_buffer(402) %>%
#   st_transform(4326)
# saveRDS(transit_access_2021, 'data/transit_access_2021.RDS')
# rm(tempService, tempTrips)
# transit_access_2021 <- readRDS("data/transit_access_2021.RDS")


# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = transit_access_2019, weight = 1) %>%
#   addPolygons(data = transit_access_2021, weight = 1, color = 'red')









