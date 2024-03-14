# 2019 -------
vehicle19[, vehicle_id := paste0(hh_id, "_", vehicle_num %>% str_extract("[0-9]+") %>% as.numeric())]
trip19[
  mode_type_detailed %>% str_detect("Household vehicle"),
  vehicle_id := paste(hh_id, str_extract(mode_type_detailed, "[0-9]+"), sep = "_")
]
vehicle19[, veh_age := fifelse(year == "1980 or earlier", 39, 2020 - as.numeric(year))]
vehicle19[hh19, on = .(hh_id), hh_weight := i.hh_weight]

# 2021 ----
vehicle21[, veh_age := fifelse(year == "1980 or earlier", 39, 2020 - as.numeric(year))]
vehicle21[hh21, on = .(hh_id), hh_weight := i.hh_weight]

