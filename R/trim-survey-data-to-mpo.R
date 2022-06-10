### Toolbox ----------
library(DBI)
library(sf)
library(tidyverse)


db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")

### Get MPO shapefile -------------
mpo_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.MetropolitanPlanningOrganizationArea;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
mpo_sf <- st_transform(mpo_sf, crs = 4326)

### Create spatial features object from hh table ------------
hh_sf <- hh %>%
  select(hh_id, home_lon, home_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("home_lon", "home_lat"),
    crs = 4326
  )

### Trim hh: households in MPO ----------
hh_ids <-
  st_join(hh_sf, mpo_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  filter(OBJECTID == 1) %>%
  select(hh_id)

hh[, hh_in_mpo := ifelse(hh_id %in% hh_ids$hh_id, "in_mpo", "outside_mpo")]

hh <- hh %>%
  filter(hh_in_mpo == "in_mpo")

### Trim veh: Vehicles owned by HHs in MPO----------
veh <- veh %>%
  left_join(hh %>% select(hh_id, hh_in_mpo)) %>%
  filter(hh_in_mpo == "in_mpo") %>%
  select(-hh_in_mpo)

### Trim per: people who live in MPO----------
per <- per %>%
  left_join(hh %>% select(hh_id, hh_in_mpo)) %>%
  filter(hh_in_mpo == "in_mpo") %>%
  select(-hh_in_mpo)

### Trim day: days for people that live in MPO----------
day <- day %>%
  left_join(hh %>% select(hh_id, hh_in_mpo)) %>%
  filter(hh_in_mpo == "in_mpo") %>%
  select(-hh_in_mpo)

### Trim trip: trips made by HHs in MPO ----------
trip <- trip %>%
  left_join(select(per, person_id, hh_id)) %>%
  left_join(select(hh, hh_id, hh_in_mpo)) %>%
  filter(hh_in_mpo == "in_mpo")

rm(hh_sf, db, hh_ids, mpo_sf)
