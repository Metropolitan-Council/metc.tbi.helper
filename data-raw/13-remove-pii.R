# This script is writen to run after
# 12-derive-var-trip-seasons.R

# check for any PII and remove
# for vehicle table, remove make, model, year and name, and then round the numbers from DPS/EPA

# 2019 ---------------
# * Vehicle -------------
vehicle19_rmPII <- copy(vehicle19)
vehicle19_rmPII[
  , c(
    "make", "model", "vehicle_name"
    # FIXME: add these back in when scripts 4 and 5 are back up and runnign
    # , "class_vehicle"
    # , "epa_tbi_veh_match_notes"
    # , "dps_tbi_veh_match_notes"
  ) := NULL
]

# * household ---------------
hh19_rmPII <- copy(hh19)
hh19_rmPII[
  , c("home_lat", "home_lon", "sample_home_lon", "sample_home_lat") := NULL
]

# * trip -----------------
trip19_rmPII <- copy(trip19)
trip19_rmPII[d_purpose_category_broad %in% c("Home", "Work", "School"),
             c("d_lat", "d_lon") := NA]
trip19_rmPII[o_purpose_category_broad %in% c("Home", "Work", "School"),
             c("o_lat", "o_lon") := NA]

# * person ---------------------
person19_rmPII <- copy(person19)
person19_rmPII[
  , c("work_lat", "school_lat", "work_lon", "school_lon", "race_other_specify") := NULL
]




# 2021 ---------------
# * Vehicle -------------
vehicle21_rmPII <- copy(vehicle21)
vehicle21_rmPII[
  , c(
    "make", "model", "make_model_other"
    # FIXME: add these back in when scripts 4 and 5 are back up and runnign
    # , "class_vehicle"
    # , "epa_tbi_veh_match_notes"
    # , "dps_tbi_veh_match_notes"
  ) := NULL
]

# * household ---------------
hh21_rmPII <- copy(hh21)
hh21_rmPII[
  , c("home_lat", "home_lon", "sample_home_lon", "sample_home_lat") := NULL
]

# * trip -----------------
trip21_rmPII <- copy(trip21)
trip21_rmPII[d_purpose_category_broad %in% c("Home", "Work", "School"),
             c("d_lat", "d_lon") := NA]
trip21_rmPII[o_purpose_category_broad %in% c("Home", "Work", "School"),
             c("o_lat", "o_lon") := NA]

# * person ---------------------
person21_rmPII <- copy(person21)
person21_rmPII[
  , c("work_lat", "school_lat", "work_lon", "school_lon", "race_other_specify") := NULL
]
