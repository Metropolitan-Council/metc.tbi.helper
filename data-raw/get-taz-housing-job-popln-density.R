hh19_sf <- hh19 %>%
  select(hh_id, home_lon, home_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("home_lon", "home_lat"),
    crs = 4326
  )

hh21_sf <- hh21 %>%
  select(hh_id, home_lon, home_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("home_lon", "home_lat"),
    crs = 4326
  )

### Get Shapefiles -------------
message("Downloading Shapefiles from GIS Library")

if (grepl("mac", osVersion)) {
  db <- DBI::dbConnect(odbc::odbc(),
                       "GISLibrary",
                       Driver = "FreeTDS",
                       timeout = 10,
                       Uid = keyring::key_get("MetC_uid"),
                       Pwd = keyring::key_get("MetC")
  )
} else {
  db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")
}


taz_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.TAZ2010;"
) %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  st_transform(crs = 4326) %>%
  st_make_valid() %>%
  mutate(taz_pop_per_acre = POPTOTAL/ACRES,
         taz_housing_units_per_acre = HUTOTAL/ACRES,
         taz_jobs_per_acre = TOTAL_EMP/ACRES) %>%
  mutate(taz = as.integer64(CensusTAZ)) %>%
  select(taz, taz_pop_per_acre, taz_housing_units_per_acre, taz_jobs_per_acre)



hh19_taz <-
  st_join(hh19_sf, taz_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_taz = taz)

hh21_taz <-
  st_join(hh21_sf, taz_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_taz = taz)

hh19 <- hh19 %>%
  left_join(hh19_taz, by = "hh_id")

hh21 <- hh21 %>%
  left_join(hh21_sf, by = "hh_id")
