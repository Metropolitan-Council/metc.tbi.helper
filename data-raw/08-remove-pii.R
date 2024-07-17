# This script is writen to run after
# 07-derive-var-trip-seasons.R

# check for any PII and remove
# for vehicle table, remove make, model, year and name, and then round the numbers from DPS/EPA
tbi_rmPII <- copy(tbi)

# vehicle -----------------
tbi_rmPII$vehicle[, c("make", "model", "vehicle_name") := NULL]

# household -----------------
tbi_rmPII$hh[, c("home_lat", "home_lon", "sample_home_lon", "sample_home_lat") := NULL]

# trip -----------------
tbi_rmPII$trip[
  d_purpose_category_broad %in% c("Home", "Work", "School"),
  c("d_lat", "d_lon") := NA
]
tbi_rmPII$trip[
  o_purpose_category_broad %in% c("Home", "Work", "School"),
  c("d_lat", "d_lon") := NA
]

# person ---------------------
tbi_rmPII$person[
  , c("work_lat", "school_lat", "work_lon", "school_lon") := NULL
]
# remove raw locations ------
tbi_rmPII <- tbi_rmPII[names(tbi_rmPII) != "location"]
