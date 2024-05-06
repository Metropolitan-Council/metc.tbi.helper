# ### List of Counties ----------
# county_list <-
#   c(
#     "Anoka MN", "Carver MN", "Chisago MN", "Dakota MN", "Goodhue MN",
#     "Hennepin MN", "Isanti MN", "Le Sueur MN", "McLeod MN", "Pierce WI",
#     "Polk WI", "Ramsey MN", "Rice MN", "Scott MN", "Sherburne MN",
#     "Sibley MN", "St. Croix WI", "Washington MN", "Wright MN"
#   )

### TBI lng/lats ------------
##### households -----
hh_sf <-
  tbi$hh %>%
    select(hh_id, home_lon, home_lat) %>%
    na.omit() %>%
    st_as_sf(
      coords = c("home_lon", "home_lat"),
      crs = 4326
    ) %>%
    st_make_valid()

##### trip origins/destinations -----
trip_d_sf <- tbi$trip %>%
  select(trip_id, d_lon, d_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("d_lon", "d_lat"), crs = 4326) %>%
  st_make_valid()

trip_o_sf <- tbi$trip %>%
  select(trip_id, o_lon, o_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("o_lon", "o_lat"), crs = 4326) %>%
  st_make_valid()


##### work locations -----
work_sf <-
  tbi$person %>%
  select(person_id, work_lon, work_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("work_lon", "work_lat"),
    crs = 4326
  ) %>%
  st_make_valid()

##### school locations -----
school_sf <-
  tbi$person %>%
  select(person_id, school_lon, school_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("school_lon", "school_lat"),
    crs = 4326
  ) %>%
  st_make_valid()


### Shapefiles -------------
##### MPO (newly updated and not in the gis database yet) ----
mpo_sf <-
  st_read("data/MPOArea_2020/") %>%
  st_make_valid() %>%
  st_transform(crs = 4326)

##### Counties: ----
cty_sf <-
  rbind(
    councilR::import_from_gis("MNCounties") %>%
      st_make_valid() %>%
      rename(county = CO_NAME) %>%
      mutate(county = paste(county, "MN")) %>%
      st_transform(crs = 4326) %>%
      select(county),
    councilR::import_from_gis("WICounties") %>%
      st_make_valid() %>%
      rename(county = CO_NAME) %>%
      mutate(county = paste(county, "WI")) %>%
      st_transform(crs = 4326) %>%
      select(county)
  ) %>%
  st_make_valid()

##### Cities: ----
ctu_sf <-
  councilR::import_from_gis("CTUs") %>%
  st_make_valid() %>%
  select(CTU_NAME) %>%
  rename(community_name = CTU_NAME) %>%
  st_transform(crs = 4326)

##### Community Designations 2050: ----
# proposed CD's
# TODO: replace with offical when they're finalized.
cd_2050_sf <-
  councilR::import_from_gis("PROPOSED2050COMMUNITYDESIGNATIONS") %>%
  st_make_valid() %>%
  select(CD2050) %>%
  rename(cd_2050 = CD2050) %>%
  st_transform(4326) %>%
  mutate(
    cd_2050 = as.factor(cd_2050)
  ) %>%
  mutate(
    cd_2050_broad = recode_factor(
      cd_2050,
      "Agricultural" = "Rural/Non-Coucil",
      "Rural Center" = "Rural/Non-Coucil",
      "Rural Residential" = "Rural/Non-Coucil",
      "Non-Council Community" = "Rural/Non-Coucil"
    )
  ) %>%
  mutate(
    cd_2050_rsd = recode_factor(
      cd_2050_broad,
      "Urban Edge" = "Urban",
      "Suburban Edge" = "Suburban",
      "Diversified Residential" = "Rural/Non-Coucil"
    ))

##### Census Block Groups: ----
cbg2010_sf <-
  councilR::import_from_gis("CENSUS2010TIGERBLOCKGROUP") %>%
  sf::st_make_valid() %>%
  st_transform(crs = 4326) %>%
  select(GEOID10) %>%
  rename(cbg_2010 = GEOID10)


cbg2020_sf <-
  councilR::import_from_gis("CENSUS2020TIGERBLOCKGROUP") %>%
  sf::st_make_valid() %>%
  st_transform(crs = 4326) %>%
  select(GEOID20) %>%
  rename(cbg_2020 = GEOID20)

##### TAZs (2010): ----
taz_sf <-
  councilR::import_from_gis("TAZ2010") %>%
  st_make_valid() %>%
  st_transform(crs = 4326) %>%
  mutate(
    taz_pop_per_acre = POPTOTAL / ACRES,
    taz_housing_units_per_acre = HUTOTAL / ACRES,
    taz_jobs_per_acre = TOTAL_EMP / ACRES
  ) %>%
  mutate(taz = as.integer64(CensusTAZ)) %>%
  select(taz, taz_pop_per_acre, taz_housing_units_per_acre, taz_jobs_per_acre)

# Household ----------
hh_mpo <-
  st_join(hh_sf, mpo_sf %>% select(InMPO), join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_in_mpo = InMPO) %>%
  mutate(hh_in_mpo = case_when(
    hh_in_mpo == 1 ~ TRUE,
    .default = FALSE
  ))
hh_cty <-
  st_join(hh_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(
    county = county %>% as.factor()
  ) %>%
  rename(
    hh_county = county
  )
hh_ctu <-
  st_join(hh_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(community_name = community_name %>% as.factor()) %>%
  rename(hh_city = community_name)
hh_cd_2050 <-
  st_join(hh_sf, cd_2050_sf, join = st_within) %>%
  st_drop_geometry()
hh_cbg_2010 <-
  st_join(hh_sf, cbg2010_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_cbg_2010 = cbg_2010)
hh_cbg_2020 <-
  st_join(hh_sf, cbg2020_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_cbg_2020 = cbg_2020)
hh_taz <-
  st_join(hh_sf, taz_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_taz = taz)

tbi$hh <-
  tbi$hh %>%
  left_join(hh_mpo, by = "hh_id") %>%
  left_join(hh_cty, by = "hh_id") %>%
  left_join(hh_ctu, by = "hh_id") %>%
  left_join(hh_cd_2050, by = "hh_id") %>%
  left_join(hh_cbg_2010, by = "hh_id") %>%
  left_join(hh_cbg_2020, by = "hh_id") %>%
  left_join(hh_taz, by = "hh_id")


# Trip Origin ----
trip_o_mpo <-
  st_join(trip_o_sf, mpo_sf %>% select(InMPO), join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_in_mpo = InMPO) %>%
  mutate(trip_o_in_mpo = case_when(
    trip_o_in_mpo == 1 ~ TRUE,
    .default = FALSE
  ))
trip_o_cty <-
  st_join(trip_o_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(
    county = county %>% as.factor()
  ) %>%
  rename(
    trip_o_county = county
  )
trip_o_ctu <-
  st_join(trip_o_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(community_name = community_name %>% as.factor()) %>%
  rename(trip_o_city = community_name)
trip_o_cd_2050 <-
  st_join(trip_o_sf, cd_2050_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    trip_o_cd_2050 = cd_2050,
    trip_o_cd_2050_broad = cd_2050_broad,
    trip_o_cd_2050_rsd = cd_2050_rsd
  )
trip_o_cbg_2010 <-
  st_join(trip_o_sf, cbg2010_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_cbg_2010 = cbg_2010)
trip_o_cbg_2020 <-
  st_join(trip_o_sf, cbg2020_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_cbg_2020 = cbg_2020)
trip_o_taz <-
  st_join(trip_o_sf,
          taz_sf %>% select("taz"),
          join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_taz = taz)

tbi$trip <-
  tbi$trip %>%
  left_join(trip_o_mpo, by = "trip_id") %>%
  left_join(trip_o_cty, by = "trip_id") %>%
  left_join(trip_o_ctu, by = "trip_id") %>%
  left_join(trip_o_cd_2050, by = "trip_id") %>%
  left_join(trip_o_cbg_2010, by = "trip_id") %>%
  left_join(trip_o_cbg_2020, by = "trip_id") %>%
  left_join(trip_o_taz, by = "trip_id")

# Trip Dest ----
trip_d_mpo <-
  st_join(trip_d_sf, mpo_sf %>% select(InMPO), join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_in_mpo = InMPO) %>%
  mutate(trip_d_in_mpo = case_when(
    trip_d_in_mpo == 1 ~ TRUE,
    .default = FALSE
  ))
trip_d_cty <-
  st_join(trip_d_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(county = county %>% as.factor()) %>%
  rename(trip_d_county = county) %>%
  filter(!duplicated(trip_id)) # there is one trip dist on a county line
trip_d_ctu <-
  st_join(trip_d_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(community_name = community_name %>% as.factor()) %>%
  rename(trip_d_city = community_name)
trip_d_cd_2050 <-
  st_join(trip_d_sf, cd_2050_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    trip_d_cd_2050 = cd_2050,
    trip_d_cd_2050_broad = cd_2050_broad,
    trip_d_cd_2050_rsd = cd_2050_rsd
  )
trip_d_cbg_2010 <-
  st_join(trip_d_sf, cbg2010_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_cbg_2010 = cbg_2010)
trip_d_cbg_2020 <-
  st_join(trip_d_sf, cbg2020_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_cbg_2020 = cbg_2020)
trip_d_taz <-
  st_join(trip_d_sf,
          taz_sf %>% select("taz"),
          join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_taz = taz)

tbi$trip <-
  tbi$trip %>%
  left_join(trip_d_mpo, by = "trip_id") %>%
  left_join(trip_d_cty, by = "trip_id") %>%
  left_join(trip_d_ctu, by = "trip_id") %>%
  left_join(trip_d_cd_2050, by = "trip_id") %>%
  left_join(trip_d_cbg_2010, by = "trip_id") %>%
  left_join(trip_d_cbg_2020, by = "trip_id") %>%
  left_join(trip_d_taz, by = "trip_id")

# Work Loc ----
work_mpo <-
  st_join(work_sf, mpo_sf %>% select(InMPO), join = st_within) %>%
  st_drop_geometry() %>%
  rename(work_in_mpo = InMPO) %>%
  mutate(work_in_mpo = case_when(
    work_in_mpo == 1 ~ TRUE,
    .default = FALSE
  ))
work_cty <-
  st_join(work_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(county = county %>% as.factor()) %>%
  rename(work_county = county)
work_ctu <-
  st_join(work_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(community_name = community_name %>% as.factor()) %>%
  rename(work_city = community_name)
work_cd_2050 <-
  st_join(work_sf, cd_2050_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    work_cd_2050 = cd_2050,
    work_cd_2050_broad = cd_2050_broad,
    work_cd_2050_rsd = cd_2050_rsd
  )
work_cbg_2010 <-
  st_join(work_sf, cbg2010_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(work_cbg_2010 = cbg_2010)
work_cbg_2020 <-
  st_join(work_sf, cbg2020_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(work_cbg_2020 = cbg_2020)
work_taz <-
  st_join(work_sf,
          taz_sf %>% select("taz"),
          join = st_within) %>%
  st_drop_geometry() %>%
  rename(work_taz = taz)

tbi$person <-
  tbi$person %>%
  left_join(work_mpo, by = "person_id") %>%
  # left_join(work_cty, by = "person_id") %>% # RSG did this already
  left_join(work_ctu, by = "person_id") %>%
  left_join(work_cd_2050, by = "person_id") %>%
  left_join(work_cbg_2010, by = "person_id") %>%
  left_join(work_cbg_2020, by = "person_id") %>%
  left_join(work_taz, by = "person_id")

# School Loc ----
school_mpo <-
  st_join(school_sf, mpo_sf %>% select(InMPO), join = st_within) %>%
  st_drop_geometry() %>%
  rename(school_in_mpo = InMPO) %>%
  mutate(school_in_mpo = case_when(
    school_in_mpo == 1 ~ TRUE,
    .default = FALSE
  ))
school_cty <-
  st_join(school_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(county = county %>% as.factor()) %>%
  rename(school_county = county)
school_ctu <-
  st_join(school_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(community_name = community_name %>% as.factor()) %>%
  rename(school_city = community_name)
school_cd_2050 <-
  st_join(school_sf, cd_2050_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(
    school_cd_2050 = cd_2050,
    school_cd_2050_broad = cd_2050_broad,
    school_cd_2050_rsd = cd_2050_rsd
  )
school_cbg_2010 <-
  st_join(school_sf, cbg2010_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(school_cbg_2010 = cbg_2010)
school_cbg_2020 <-
  st_join(school_sf, cbg2020_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(school_cbg_2020 = cbg_2020)
school_taz <-
  st_join(school_sf,
          taz_sf %>% select("taz"),
          join = st_within) %>%
  st_drop_geometry() %>%
  rename(school_taz = taz)

tbi$person <-
  tbi$person %>%
  left_join(school_mpo, by = "person_id") %>%
  # left_join(school_cty, by = "person_id") %>% # RSG did this already
  left_join(school_ctu, by = "person_id") %>%
  left_join(school_cd_2050, by = "person_id") %>%
  left_join(school_cbg_2010, by = "person_id") %>%
  left_join(school_cbg_2020, by = "person_id") %>%
  left_join(school_taz, by = "person_id")

# Clean up:
rm(
  cbg2010_sf,
  cbg2020_sf,
  cd_2050_sf,
  ctu_sf,
  cty_sf,
  hh_cbg_2010,
  hh_cbg_2020,
  hh_cd_2050,
  hh_ctu,
  hh_cty,
  hh_mpo,
  hh_sf,
  hh_taz,
  mpo_sf,
  school_cbg_2010,
  school_cbg_2020,
  school_cd_2050,
  school_ctu,
  school_cty,
  school_mpo,
  school_sf,
  school_taz,
  taz_sf,
  trip_d_cbg_2010,
  trip_d_cbg_2020,
  trip_d_cd_2050,
  trip_d_ctu,
  trip_d_cty,
  trip_d_mpo,
  trip_d_sf,
  trip_d_taz,
  trip_o_cbg_2010,
  trip_o_cbg_2020,
  trip_o_cd_2050,
  trip_o_ctu,
  trip_o_cty,
  trip_o_mpo,
  trip_o_sf,
  trip_o_taz,
  work_cbg_2010,
  work_cbg_2020,
  work_cd_2050,
  work_ctu,
  work_cty,
  work_mpo,
  work_sf,
  work_taz
)
