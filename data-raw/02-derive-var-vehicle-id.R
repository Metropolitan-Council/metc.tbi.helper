message("02-derive-var-vehicle-id.R")
# This script is writen to run after
# 01-get-survey-data.R

# 2019 -------
vehicle19[, veh_id := paste0(hh_id, "-", vehicle_num)]
trip19[
  mode_type_detailed %>% str_detect("Household vehicle"),
  veh_id := paste(hh_id, str_extract(mode_type_detailed, "[0-9]+"), sep = "_")
]

vehicle19[, veh_age := fifelse(year == "1980 or earlier", 39, 2020 - as.numeric(year))]
vehicle19[household19, on = .(hh_id), hh_weight := i.hh_weight]

# 2021 ----
setnames(vehicle21, "vehicle_id", "veh_id")
vehicle21[, veh_age := fifelse(year == "1980 or earlier", 39, 2020 - as.numeric(year))]
vehicle21[household21, on = .(hh_id), hh_weight := i.hh_weight]
