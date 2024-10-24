# libraries --------
suppressMessages(library(data.table, quietly = T))
suppressMessages(library(bit64, quietly = T))
suppressMessages(library(openxlsx, quietly = T))
suppressMessages(library(dplyr, quietly = T))
suppressMessages(library(keyring, quietly = T))
suppressMessages(library(here, quietly = T))
suppressMessages(library(sf, quietly = T))
suppressMessages(library(purrr, quietly = T))
suppressMessages(library(ggplot2, quietly = T))
suppressMessages(library(plotly, quietly = T))
suppressMessages(library(stringr, quietly = T))
suppressMessages(library(tidyr, quietly = T))
suppressMessages(library(srvyr, quietly = T))
suppressMessages(library(councilR, quietly = T))
suppressMessages(library(showtext, quietly = T))
suppressMessages(library(janitor, quietly = T))
suppressMessages(library(leaflet, quietly = T))
suppressMessages(library(DBI, quietly = T))


# utilities -------
source("R/_db_connect.R")

# data --------
load("data/tbi21.rda")
load("data/tbi19.rda")


tbi19 <- tbi19_rmPII
tbi21 <- tbi21_rmPII

lapply(tbi19, setDT)
lapply(tbi21, setDT)

setcolorder(tbi21$trip, tbi21$trip %>% names() %>% grep(pattern = "_id", value = TRUE))
setcolorder(tbi19$trip, tbi19$trip %>% names() %>% grep(pattern = "_id", value = TRUE))
setcolorder(tbi21$household, tbi21$household %>% names() %>% grep(pattern = "_id", value = TRUE))
setcolorder(tbi19$household, tbi19$household %>% names() %>% grep(pattern = "_id", value = TRUE))


# remove special characters in 2021 data
tbi21$trip <- tbi21 %>%
  pluck("trip") %>%
  mutate(
    mode_type_detailed = mode_type_detailed %>%
      as.character() %>%
      str_replace("Â", "") %>%
      str_trim()
  )

# Load mpo----
# if connected to local database
mpo_sf <- councilR::import_from_gis("MetropolitanPlanningOrganizationArea") %>%
  st_transform(4326)

# if not connected to local database
mpo_sf <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_metro_planning_org_area/gpkg_trans_metro_planning_org_area.zip") %>%
  st_transform(4326)

# load CTUs
# db <- db_connect_gis()
