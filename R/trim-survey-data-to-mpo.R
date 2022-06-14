### Toolbox ----------
source("data-raw/00-load-pkgs.R")

### Trim to HHs in MPO----------
tbi_tables$hh <- tbi_tables$hh %>%
  filter(hh_in_mpo == "in_mpo")

### Trim veh: Vehicles owned by HHs in MPO----------
tbi_tables$veh <- tbi_tables$veh %>%
  right_join(tbi_tables$hh %>% select(hh_id))

### Trim per: people who live in MPO----------
tbi_tables$per <- tbi_tables$per %>%
  right_join(tbi_tables$hh %>% select(hh_id))

### Trim day: days for people that live in MPO----------
tbi_tables$day <- tbi_tables$day %>%
  right_join(tbi_tables$hh %>% select(hh_id))

### Trim trip: trips made by HHs in MPO ----------
tbi_tables$trip <- tbi_tables$trip %>%
  right_join(tbi_tables$hh %>% select(hh_id))
