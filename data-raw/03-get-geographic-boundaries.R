# Packages -------------
# source("data-raw/00-load-pkgs.R")
### List of Counties ----------
county_list <-
  c(
    "Anoka MN", "Carver MN", "Chisago MN", "Dakota MN", "Goodhue MN",
    "Hennepin MN", "Isanti MN", "Le Sueur MN", "McLeod MN", "Pierce WI",
    "Polk WI", "Ramsey MN", "Rice MN", "Scott MN", "Sherburne MN",
    "Sibley MN", "St. Croix WI", "Washington MN", "Wright MN"
  )


### Create SF objects from TBI tables ------------
message("Creating SF objects from TBI Tables")

##### households -----
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

##### trip origins/destinations -----
trip19_d_sf <- trip19 %>%
  select(trip_id, d_lon, d_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("d_lon", "d_lat"), crs = 4326)

trip21_d_sf <- trip21 %>%
  select(trip_id, d_lon, d_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("d_lon", "d_lat"), crs = 4326)

trip19_o_sf <- trip19 %>%
  select(trip_id, o_lon, o_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("o_lon", "o_lat"), crs = 4326)

trip21_o_sf <- trip21 %>%
  select(trip_id, o_lon, o_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("o_lon", "o_lat"), crs = 4326)


##### work locations -----
work19_sf <- per19 %>%
  select(person_id, work_lon, work_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("work_lon", "work_lat"),
    crs = 4326
  )

work21_sf <- per21 %>%
  select(person_id, work_lon, work_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("work_lon", "work_lat"),
    crs = 4326
  )

##### school locations -----
school19_sf <- per19 %>%
  select(person_id, school_lon, school_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("school_lon", "school_lat"),
    crs = 4326
  )

school21_sf <- per21 %>%
  select(person_id, school_lon, school_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("school_lon", "school_lat"),
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

##### MPO: ----
mpo_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.MetropolitanPlanningOrganizationArea;"
) %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  st_transform(crs = 4326) %>%
  st_make_valid()


##### Counties: ----
# Minnesota:
mn_cty_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.MNCounties;"
) %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  st_transform(crs = 4326) %>%
  rename(county = CO_NAME) %>%
  mutate(county = paste(county, "MN")) %>%
  select(county) %>%
  st_make_valid()

# Wisconsin:
wi_cty_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.WICounties;"
) %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  st_transform(crs = 4326) %>%
  rename(county = CO_NAME) %>%
  mutate(county = paste(stringr::str_to_title(county), "WI")) %>%
  select(county) %>%
  st_make_valid()

# Both states:
cty_sf <- rbind(mn_cty_sf, wi_cty_sf) %>%
  st_make_valid()

##### Cities: ----
ctu_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.CTUs;"
) %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  select(CTU_NAME) %>%
  rename(community_name = CTU_NAME) %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

##### Thrive: ----
thrive_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.THRIVEMSP2040COMMUNITYDESIGNATION;"
) %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  select(COMDESNAME) %>%
  rename(thrive_category = COMDESNAME) %>%
  st_transform(crs = 4326) %>%
  st_make_valid() %>%
  mutate(thrive_category = factor(
    thrive_category,
    levels = c(
      "Urban Center",
      "Urban",
      "Suburban",
      "Suburban Edge",
      "Emerging Suburban Edge",
      "Rural Center",
      "Rural Residential",
      "Diversified Rural",
      "Agricultural"
    )
  )) %>%
  mutate(
    thrive_category_broad = recode_factor(
      thrive_category,
      "Agricultural" = "Rural",
      "Diversified Rural" = "Rural",
      "Rural Center" = "Rural",
      "Rural Residential" = "Rural"
    )
  ) %>%
  mutate(thrive_category_broad = factor(
    thrive_category_broad,
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
    urban_rural_suburban = recode_factor(
      thrive_category_broad,
      "Urban Center" = "Urban",
      "Suburban Edge" = "Suburban",
      "Emerging Suburban Edge" = "Suburban"
    )
  )

##### Census Block Groups: ----
cbg_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.CENSUS2010TIGERBLOCKGROUP;"
) %>%
  sf::st_as_sf(wkt = "geometry", crs = 26915) %>%
  sf::st_make_valid() %>%
  st_transform(crs = 4326) %>%
  select(GEOID10) %>%
  rename(cbg = GEOID10)

##### TAZs (2010): ----
taz_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.TAZ2010;"
) %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  st_transform(crs = 4326) %>%
  st_make_valid() %>%
  mutate(
    taz_pop_per_acre = POPTOTAL / ACRES,
    taz_housing_units_per_acre = HUTOTAL / ACRES,
    taz_jobs_per_acre = TOTAL_EMP / ACRES
  ) %>%
  mutate(taz = as.integer64(CensusTAZ)) %>%
  select(taz, taz_pop_per_acre, taz_housing_units_per_acre, taz_jobs_per_acre)

### Household geographic Info ----------
message("Appending geographic boundaries to household locations")
##### MPO: ----
hh19_mpo <-
  st_join(hh19_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_in_mpo = OBJECTID) %>%
  mutate(hh_in_mpo = case_when(hh_in_mpo == 1 ~ "Household in Twin Cities region", TRUE ~ "Household outside Twin Cities region"))

hh21_mpo <-
  st_join(hh21_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_in_mpo = OBJECTID) %>%
  mutate(hh_in_mpo = case_when(hh_in_mpo == 1 ~ "Household in Twin Cities region", TRUE ~ "Household outside Twin Cities region"))

##### County: ----
hh19_cty <-
  st_join(hh19_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_county = county) %>%
  group_by(hh_county) %>%
  add_tally() %>%
  ungroup() %>%
  mutate(hh_county_n = case_when(!is.na(hh_county) ~ paste0(hh_county, " (n = ", n, ")"))) %>%
  select(-n)

hh21_cty <-
  st_join(hh21_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_county = county) %>%
  group_by(hh_county) %>%
  add_tally() %>%
  ungroup() %>%
  mutate(hh_county_n = case_when(!is.na(hh_county) ~ paste0(hh_county, " (n = ", n, ")"))) %>%
  select(-n)

##### City: ----
hh19_ctu <-
  st_join(hh19_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_city = community_name) %>%
  group_by(hh_city) %>%
  add_tally() %>%
  ungroup() %>%
  mutate(hh_city_n = case_when(!is.na(hh_city) ~ paste0(hh_city, " (n = ", n, ")"))) %>%
  select(-n)

hh21_ctu <-
  st_join(hh21_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_city = community_name) %>%
  group_by(hh_city) %>%
  add_tally() %>%
  ungroup() %>%
  mutate(hh_city_n = case_when(!is.na(hh_city) ~ paste0(hh_city, " (n = ", n, ")"))) %>%
  select(-n)


##### Thrive: ----
hh19_thrive <-
  st_join(hh19_sf, thrive_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    hh_thrive_category = thrive_category,
    hh_thrive_category_broad = thrive_category_broad,
    hh_urban_rural_suburban = urban_rural_suburban
  )

hh21_thrive <-
  st_join(hh21_sf, thrive_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    hh_thrive_category = thrive_category,
    hh_thrive_category_broad = thrive_category_broad,
    hh_urban_rural_suburban = urban_rural_suburban
  )

##### Block Group: ----
hh19_cbg <-
  st_join(hh19_sf, cbg_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_cbg = cbg)

hh21_cbg <-
  st_join(hh21_sf, cbg_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_cbg = cbg)

##### TAZ: ----
hh19_taz <-
  st_join(hh19_sf, taz_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_taz = taz)

hh21_taz <-
  st_join(hh21_sf, taz_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_taz = taz)


#### Compile, write over hh table: -----
hh19 <- hh19 %>%
  left_join(hh19_mpo, by = "hh_id") %>%
  left_join(hh19_cty, by = "hh_id") %>%
  left_join(hh19_ctu, by = "hh_id") %>%
  left_join(hh19_cbg, by = "hh_id") %>%
  left_join(hh19_thrive, by = "hh_id") %>%
  left_join(hh19_taz, by = "hh_id") %>%
  mutate(across(c(hh_in_mpo, hh_county, hh_city), ~ as.factor(.)))

hh21 <- hh21 %>%
  left_join(hh21_mpo, by = "hh_id") %>%
  left_join(hh21_cty, by = "hh_id") %>%
  left_join(hh21_ctu, by = "hh_id") %>%
  left_join(hh21_cbg, by = "hh_id") %>%
  left_join(hh21_thrive, by = "hh_id") %>%
  left_join(hh21_taz, by = "hh_id") %>%
  mutate(across(c(hh_in_mpo, hh_county, hh_city), ~ as.factor(.)))

### Append Geographic Info to Trip Origin & Destination ----------
message("Appending geographic information to trip origin & destination")
##### MPO: ----
message("... trip origin/destination within MPO boundary...")
trip19_o_mpo <-
  st_join(trip19_o_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_in_mpo = OBJECTID) %>%
  mutate(trip_o_in_mpo = case_when(trip_o_in_mpo == 1 ~ "Trip begins in Twin Cities region", TRUE ~ "Trip begins outside Twin Cities region"))

trip21_o_mpo <-
  st_join(trip21_o_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_in_mpo = OBJECTID) %>%
  mutate(trip_o_in_mpo = case_when(trip_o_in_mpo == 1 ~ "Trip begins in Twin Cities region", TRUE ~ "Trip begins outside Twin Cities region"))

trip19_d_mpo <-
  st_join(trip19_d_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_in_mpo = OBJECTID) %>%
  mutate(trip_d_in_mpo = case_when(trip_d_in_mpo == 1 ~ "Trip ends in Twin Cities region", TRUE ~ "Trip ends outside Twin Cities region"))

trip21_d_mpo <-
  st_join(trip21_d_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_in_mpo = OBJECTID) %>%
  mutate(trip_d_in_mpo = case_when(trip_d_in_mpo == 1 ~ "Trip ends in Twin Cities region", TRUE ~ "Trip ends outside Twin Cities region"))

##### County: ----
message("... trip origin/destination county...")
trip19_o_cty <-
  st_join(trip19_o_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_county = county) %>%
  mutate(trip_o_county = ifelse(
    trip_o_county %in% county_list, trip_o_county, "Outside study area"
  ))

trip19_d_cty <-
  st_join(trip19_d_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_county = county) %>%
  mutate(trip_d_county = ifelse(
    trip_d_county %in% county_list, trip_d_county, "Outside study area"
  ))

trip21_o_cty <-
  st_join(trip21_o_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_county = county) %>%
  mutate(trip_o_county = ifelse(
    trip_o_county %in% county_list, trip_o_county, "Outside study area"
  ))

trip21_d_cty <-
  st_join(trip21_d_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_county = county) %>%
  mutate(trip_d_county = ifelse(
    trip_d_county %in% county_list, trip_d_county, "Outside study area"
  ))

##### City: ----
message("... trip origin/destination city...")
trip19_o_ctu <-
  st_join(trip19_o_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(trip_o_city = ifelse(is.na(community_name), "Outside 7-county area", community_name)) %>%
  select(-community_name)

trip19_d_ctu <-
  st_join(trip19_d_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(trip_d_city = ifelse(is.na(community_name), "Outside 7-county area", community_name)) %>%
  select(-community_name)

trip21_o_ctu <-
  st_join(trip21_o_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(trip_o_city = ifelse(is.na(community_name), "Outside 7-county area", community_name)) %>%
  select(-community_name)

trip21_d_ctu <-
  st_join(trip21_d_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(trip_d_city = ifelse(is.na(community_name), "Outside 7-county area", community_name)) %>%
  select(-community_name)

##### Thrive: -----
message("... trip origin/destination thrive category...")

trip19_o_thrive <-
  st_join(trip19_o_sf, thrive_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    trip_o_thrive_category = thrive_category,
    trip_o_thrive_category_broad = thrive_category_broad,
    trip_o_urban_rural_suburban = urban_rural_suburban
  )

trip19_d_thrive <-
  st_join(trip19_d_sf, thrive_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    trip_d_thrive_category = thrive_category,
    trip_d_thrive_category_broad = thrive_category_broad,
    trip_d_urban_rural_suburban = urban_rural_suburban
  )

trip21_o_thrive <-
  st_join(trip21_o_sf, thrive_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    trip_o_thrive_category = thrive_category,
    trip_o_thrive_category_broad = thrive_category_broad,
    trip_o_urban_rural_suburban = urban_rural_suburban
  )

trip21_d_thrive <-
  st_join(trip21_d_sf, thrive_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    trip_d_thrive_category = thrive_category,
    trip_d_thrive_category_broad = thrive_category_broad,
    trip_d_urban_rural_suburban = urban_rural_suburban
  )


##### Block Group: -----
message("... trip origin/destination block group...")
trip19_o_cbg <-
  st_join(trip19_o_sf, cbg_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_cbg = cbg)

trip19_d_cbg <-
  st_join(trip19_d_sf, cbg_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_cbg = cbg)

trip21_o_cbg <-
  st_join(trip21_o_sf, cbg_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_cbg = cbg)

trip21_d_cbg <-
  st_join(trip21_d_sf, cbg_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_cbg = cbg)


##### Compile: -----
trip19 <- trip19 %>%
  left_join(trip19_o_mpo, by = "trip_id") %>%
  left_join(trip19_d_mpo, by = "trip_id") %>%
  left_join(trip19_o_cty, by = "trip_id") %>%
  left_join(trip19_d_cty, by = "trip_id") %>%
  left_join(trip19_o_ctu, by = "trip_id") %>%
  left_join(trip19_d_ctu, by = "trip_id") %>%
  left_join(trip19_o_thrive, by = "trip_id") %>%
  left_join(trip19_d_thrive, by = "trip_id") %>%
  left_join(trip19_o_cbg, by = "trip_id") %>%
  left_join(trip19_d_cbg, by = "trip_id") %>%
  mutate(across(c(
    trip_o_in_mpo, trip_o_county, trip_o_city,
    trip_d_in_mpo, trip_d_county, trip_d_city
  ), ~ as.factor(.)))


trip21 <- trip21 %>%
  left_join(trip21_o_mpo, by = "trip_id") %>%
  left_join(trip21_d_mpo, by = "trip_id") %>%
  left_join(trip21_o_cty, by = "trip_id") %>%
  left_join(trip21_d_cty, by = "trip_id") %>%
  left_join(trip21_o_ctu, by = "trip_id") %>%
  left_join(trip21_d_ctu, by = "trip_id") %>%
  left_join(trip21_o_thrive, by = "trip_id") %>%
  left_join(trip21_d_thrive, by = "trip_id") %>%
  left_join(trip21_o_cbg, by = "trip_id") %>%
  left_join(trip21_d_cbg, by = "trip_id") %>%
  mutate(across(c(
    trip_o_in_mpo, trip_o_county, trip_o_city,
    trip_d_in_mpo, trip_d_county, trip_d_city
  ), ~ as.factor(.)))

### Append Geographic Info to Work ----
message("Appending geographic information to work locations (person table)")
##### MPO: ----
work19_mpo <-
  st_join(work19_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(work_in_mpo = OBJECTID) %>%
  mutate(work_in_mpo = case_when(work_in_mpo == 1 ~ "Workplace in Twin Cities region", TRUE ~ "Workplace outside Twin Cities region"))

work21_mpo <-
  st_join(work21_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(work_in_mpo = OBJECTID) %>%
  mutate(work_in_mpo = case_when(work_in_mpo == 1 ~ "Workplace in Twin Cities region", TRUE ~ "Workplace outside Twin Cities region"))

##### County: ----
work19_cty <-
  st_join(work19_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(work_county = case_when(
    county %in% county_list ~ county,
    TRUE ~ "Outside study area"
  )) %>%
  select(-county)

work21_cty <-
  st_join(work21_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(work_county = case_when(
    county %in% county_list ~ county,
    TRUE ~ "Outside study area"
  )) %>%
  select(-county)


##### City: ----
work19_ctu <-
  st_join(work19_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(work_city = ifelse(is.na(community_name), "Outside 7-county area", community_name)) %>%
  select(-community_name)

work21_ctu <-
  st_join(work21_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(work_city = ifelse(is.na(community_name), "Outside 7-county area", community_name)) %>%
  select(-community_name)

##### Thrive: ----
work19_thrive <-
  st_join(work19_sf, thrive_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    work_thrive_category = thrive_category,
    work_thrive_category_broad = thrive_category_broad,
    work_urban_rural_suburban = urban_rural_suburban
  )

work21_thrive <-
  st_join(work21_sf, thrive_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    work_thrive_category = thrive_category,
    work_thrive_category_broad = thrive_category_broad,
    work_urban_rural_suburban = urban_rural_suburban
  )

##### Block Group: ----
work19_cbg <-
  st_join(work19_sf, cbg_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(work_cbg = cbg)

work21_cbg <-
  st_join(work21_sf, cbg_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(work_cbg = cbg)

#### Compile: -----
per19 <- per19 %>%
  select(-work_county) %>%
  left_join(work19_mpo, by = "person_id") %>%
  left_join(work19_cty, by = "person_id") %>%
  left_join(work19_ctu, by = "person_id") %>%
  left_join(work19_thrive, by = "person_id") %>%
  left_join(work19_cbg, by = "person_id") %>%
  mutate(across(c(work_in_mpo, work_county, work_city), ~ as.factor(.)))

per21 <- per21 %>%
  select(-work_county) %>%
  left_join(work21_mpo, by = "person_id") %>%
  left_join(work21_cty, by = "person_id") %>%
  left_join(work21_ctu, by = "person_id") %>%
  left_join(work21_thrive, by = "person_id") %>%
  left_join(work21_cbg, by = "person_id") %>%
  mutate(across(c(work_in_mpo, work_county, work_city), ~ as.factor(.)))

### Append Geographic Info to School ----
message("Appending geographic information to school locations (person table)")
##### MPO: ----
school19_mpo <-
  st_join(school19_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(school_in_mpo = OBJECTID) %>%
  mutate(school_in_mpo = case_when(school_in_mpo == 1 ~ "School in Twin Cities region", TRUE ~ "School outside Twin Cities region"))

school21_mpo <-
  st_join(school21_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(school_in_mpo = OBJECTID) %>%
  mutate(school_in_mpo = case_when(school_in_mpo == 1 ~ "School in Twin Cities region", TRUE ~ "School outside Twin Cities region"))

##### County: ----
school19_cty <-
  st_join(school19_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(school_county = case_when(
    county %in% county_list ~ county,
    TRUE ~ "Outside study area"
  )) %>%
  select(-county)

school21_cty <-
  st_join(school21_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(school_county = case_when(
    county %in% county_list ~ county,
    TRUE ~ "Outside study area"
  )) %>%
  select(-county)


##### City: ----
school19_ctu <-
  st_join(school19_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(school_city = ifelse(is.na(community_name), "Outside 7-county area", community_name)) %>%
  select(-community_name)

school21_ctu <-
  st_join(school21_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(school_city = ifelse(is.na(community_name), "Outside 7-county area", community_name)) %>%
  select(-community_name)


##### Thrive: ----
school19_thrive <-
  st_join(school19_sf, thrive_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    school_thrive_category = thrive_category,
    school_thrive_category_broad = thrive_category_broad,
    school_urban_rural_suburban = urban_rural_suburban
  )

school21_thrive <-
  st_join(school21_sf, thrive_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    school_thrive_category = thrive_category,
    school_thrive_category_broad = thrive_category_broad,
    school_urban_rural_suburban = urban_rural_suburban
  )

##### Block Group: ----
school19_cbg <-
  st_join(school19_sf, cbg_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(school_cbg = cbg)

school21_cbg <-
  st_join(school21_sf, cbg_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(school_cbg = cbg)

##### Compile: -----
per19 <- per19 %>%
  select(-school_county) %>%
  left_join(school19_mpo, by = "person_id") %>%
  left_join(school19_cty, by = "person_id") %>%
  left_join(school19_ctu, by = "person_id") %>%
  left_join(school19_thrive, by = "person_id") %>%
  left_join(school19_cbg, by = "person_id") %>%
  mutate(across(c(school_in_mpo, school_county, school_city), ~ as.factor(.)))

per21 <- per21 %>%
  select(-school_county) %>%
  left_join(school19_mpo, by = "person_id") %>%
  left_join(school19_cty, by = "person_id") %>%
  left_join(school19_ctu, by = "person_id") %>%
  left_join(school19_thrive, by = "person_id") %>%
  left_join(school19_cbg, by = "person_id") %>%
  mutate(across(c(school_in_mpo, school_county, school_city), ~ as.factor(.)))

# Clean up:
rm(
  "cbg_sf",
  "ctu_sf",
  "cty_sf",
  "mpo_sf",
  "taz_sf",
  "mn_cty_sf",
  "wi_cty_sf",
  "thrive_sf",
  "hh19_cbg",
  "hh19_ctu",
  "hh19_cty",
  "hh19_mpo",
  "hh19_sf",
  "hh19_thrive",
  "hh19_taz",
  "hh21_cbg",
  "hh21_ctu",
  "hh21_cty",
  "hh21_mpo",
  "hh21_sf",
  "hh21_thrive",
  "hh21_taz",
  "school19_cbg",
  "school19_ctu",
  "school19_cty",
  "school19_mpo",
  "school19_sf",
  "school19_thrive",
  "school21_cbg",
  "school21_ctu",
  "school21_cty",
  "school21_mpo",
  "school21_sf",
  "school21_thrive",
  "trip19_d_cbg",
  "trip19_d_ctu",
  "trip19_d_cty",
  "trip19_d_mpo",
  "trip19_d_sf",
  "trip19_d_thrive",
  "trip19_o_cbg",
  "trip19_o_ctu",
  "trip19_o_cty",
  "trip19_o_mpo",
  "trip19_o_sf",
  "trip19_o_thrive",
  "trip21_d_cbg",
  "trip21_d_ctu",
  "trip21_d_cty",
  "trip21_d_mpo",
  "trip21_d_sf",
  "trip21_d_thrive",
  "trip21_o_cbg",
  "trip21_o_ctu",
  "trip21_o_cty",
  "trip21_o_mpo",
  "trip21_o_sf",
  "trip21_o_thrive",
  "work19_cbg",
  "work19_ctu",
  "work19_cty",
  "work19_mpo",
  "work19_sf",
  "work19_thrive",
  "work21_cbg",
  "work21_ctu",
  "work21_cty",
  "work21_mpo",
  "work21_sf",
  "work21_thrive"
)

DBI::dbDisconnect(db)
rm(db)
