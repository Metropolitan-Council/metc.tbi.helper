# This script is writen to run after
# 12-derive-var-trip-seasons.R

# check for any PII and remove
# for vehicle table, remove make, model, year and name, and then round the numbers from DPS/EPA

# 2019 ---------------
# * Vehicle -------------
vehicle19_rmPII <- copy(vehicle19)
vehicle19_rmPII[
  , c("make", "model", "vehicle_name",
      "class_vehicle", "epa_tbi_veh_match_notes",
      "dps_tbi_veh_match_notes") := NULL
]

# * household ---------------
household19_rmPII <- copy(household19)
household19_rmPII[
  , c('home_lat', 'home_lon', 'sample_home_lon', 'sample_home_lat') := NULL
]

# * trip -----------------
trip19_rmPII <- copy(trip19)
trip19_rmPII[, c('o_lat', 'o_lon', 'd_lat', 'd_lon') := NULL]

# * person ---------------------
person19_rmPII <- copy(person19)
person19_rmPII[
  , c('work_lat', 'school_lat', 'work_lon', 'school_lon', 'ethnicity_other_specify') := NULL
]


# 2021 -----
# * Vehicle -------------
vehicle21_rmPII <- copy(vehicle21)
vehicle21_rmPII[
  , c("make", "model", "class_vehicle", "epa_tbi_veh_match_notes",
      "dps_tbi_veh_match_notes") := NULL
]

# * household ---------------
household21_rmPII <- copy(household21)
household21_rmPII[
  , c('home_lat', 'home_lon', 'sample_home_lon', 'sample_home_lat') := NULL
]

# * trip -----------------
trip21_rmPII <- copy(trip21)
trip21_rmPII[, c('o_lat', 'o_lon', 'd_lat', 'd_lon', 'mode_other_comment', 'd_purpose_other') := NULL]

# * person ---------------------
person21_rmPII <- copy(person21)
person21_rmPII[
  , c('work_lat', 'school_lat', 'work_lon', 'school_lon',
      'ethnicity_other_specify', 'race_black_african_other',
      'race_asian_other', 'race_hispanic_other', 'language_at_home_other',
      'ev_typical_charge_other') := NULL
]

