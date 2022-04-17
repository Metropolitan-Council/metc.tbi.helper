### Thrive Geography---------
db <- dbConnect(odbc::odbc(), "GISLibrary") # connect to the Met Council GIS Library for handy Shapefiles

# Read in Thrive Community Designations Shapefile from GIS
thrive2040 <- dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.THRIVEMSP2040COMMUNITYDESIGNATION;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  select(COMDESNAME) %>%
  rename(thriveCategory = COMDESNAME) %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

rm(db)

### TBI HH --> Thrive Cats ---------
hh <- hh %>%
  st_as_sf(coords = c("home_lon", "home_lat"), crs = 4326) %>%
  st_join(thrive2040, join = st_within) %>%
  # Aggregate Thrive Geography Categories
  mutate(thriveCatBroad = recode_factor(thriveCategory,
    "Agricultural" = "Rural",
    "Diversified Rural" = "Rural",
    "Rural Center" = "Rural",
    "Rural Residential" = "Rural"
  )) %>%
  mutate(thriveCatBroad = factor(thriveCatBroad, levels = c(
    "Urban Center",
    "Urban",
    "Suburban",
    "Suburban Edge",
    "Emerging Suburban Edge",
    "Rural"
  ))) %>%
  mutate(thriveCatBroader = recode_factor(thriveCatBroad,
    "Urban Center" = "Urban",
    "Suburban Edge" = "Suburban",
    "Emerging Suburban Edge" = "Suburban"
  )) %>%
  st_drop_geometry() %>%
  # get lat and lon again
  left_join(hh %>% select(hh_id, home_lat, home_lon))


trip_o_thrive <- trip %>%
  select(trip_id, o_lon, o_lat) %>%
  st_as_sf(coords = c("o_lon", "o_lat"), crs = 4326) %>%
  st_join(thrive2040, join = st_within) %>%
  # Aggregate Thrive Geography Categories
  rename(o_thriveCategory = thriveCategory) %>%
  mutate(
    o_thriveCatBroad = recode_factor(
      o_thriveCategory,
      "Agricultural" = "Rural",
      "Diversified Rural" = "Rural",
      "Rural Center" = "Rural",
      "Rural Residential" = "Rural"
    )
  ) %>%
  mutate(o_thriveCatBroad = factor(
    o_thriveCatBroad,
    levels = c(
      "Urban Center",
      "Urban",
      "Suburban",
      "Suburban Edge",
      "Emerging Suburban Edge",
      "Rural"
    )
  )) %>%
  mutate(
    o_thriveCatBroader = recode_factor(
      o_thriveCatBroad,
      "Suburban Edge" = "Suburban",
      "Emerging Suburban Edge" = "Suburban"
    )
  ) %>%
  st_drop_geometry()


trip_d_thrive <- trip %>%
  select(trip_id, d_lon, d_lat) %>%
  st_as_sf(coords = c("d_lon", "d_lat"), crs = 4326) %>%
  st_join(thrive2040, join = st_within) %>%
  # Aggregate Thrive Geography Categories
  rename(d_thriveCategory = thriveCategory) %>%
  mutate(
    d_thriveCatBroad = recode_factor(
      d_thriveCategory,
      "Agricultural" = "Rural",
      "Diversified Rural" = "Rural",
      "Rural Center" = "Rural",
      "Rural Residential" = "Rural"
    )
  ) %>%
  mutate(d_thriveCatBroad = factor(
    d_thriveCatBroad,
    levels = c(
      "Urban Center",
      "Urban",
      "Suburban",
      "Suburban Edge",
      "Emerging Suburban Edge",
      "Rural"
    )
  )) %>%
  mutate(
    d_thriveCatBroader = recode_factor(
      d_thriveCatBroad,
      "Suburban Edge" = "Suburban",
      "Emerging Suburban Edge" = "Suburban"
    )
  ) %>%
  st_drop_geometry()


trip <- trip %>%
  left_join(trip_o_thrive) %>%
  left_join(trip_d_thrive)

rm(trip_d_thrive, trip_o_thrive)

rm(thrive2040)
