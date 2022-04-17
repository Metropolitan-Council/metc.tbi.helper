library(DBI)
library(sf)
library(tidyverse)

db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")

# MPO Area (Metropolitan Planning Organization - Twin Cities)
mpo_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.MetropolitanPlanningOrganizationArea;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
mpo_sf <- st_transform(mpo_sf, crs = 4326)

trip_d_sf <- trip %>%
  select(trip_id, d_lon, d_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("d_lon", "d_lat"), crs = 4326)
trip_o_sf <- trip %>%
  select(trip_id, o_lon, o_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("o_lon", "o_lat"), crs = 4326)


# Append MPO information to trip table:
trip_o_ids <-
  st_join(trip_o_sf, mpo_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  filter(OBJECTID == 1) %>%
  select(trip_id)
trip_d_ids <-
  st_join(trip_d_sf, mpo_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  filter(OBJECTID == 1) %>%
  select(trip_id)

# Join back to main tables:
trip[, trip_in_mpo := ifelse(
  trip_id %in% trip_d_ids$trip_id |
    trip_id %in% trip_o_ids$trip_id,
  "trip_in_mpo",
  "trip_outside_mpo"
)]

trip[, trip_d_in_mpo := ifelse(trip_id %in% trip_d_ids$trip_id,
  "trip_ends_in_mpo",
  "trip_ends_outside_mpo"
)]
trip[, trip_o_in_mpo := ifelse(trip_id %in% trip_o_ids$trip_id,
  "trip_starts_in_mpo",
  "trip_starts_outside_mpo"
)]

rm(trip_o_sf, trip_d_sf, trip_o_ids, trip_d_ids, mpo_sf, db)
