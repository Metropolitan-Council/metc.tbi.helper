### Toolbox ----------
source("data-raw/00-load-pkgs.R")

### Trim to HHs in MPO----------
tbi19$hh <- tbi19$hh %>%
  filter(hh_in_mpo == "in_mpo")

### Trim veh: Vehicles owned by HHs in MPO----------
tbi19$veh <- tbi19$veh %>%
  right_join(tbi19$hh %>% select(hh_id))

### Trim per: people who live in MPO----------
tbi19$per <- tbi19$per %>%
  right_join(tbi19$hh %>% select(hh_id))

### Trim day: days for people that live in MPO----------
tbi19$day <- tbi19$day %>%
  right_join(tbi19$hh %>% select(hh_id))

### Trim trip: trips made by HHs in MPO ----------
tbi19$trip <- tbi19$trip %>%
  right_join(tbi19$hh %>% select(hh_id))
