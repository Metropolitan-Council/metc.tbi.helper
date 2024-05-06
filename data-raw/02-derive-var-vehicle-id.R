# vehicle id ----------------
tbi$vehicle[
  , vehicle_id := paste0(hh_id, "_", vehicle_num %>% str_extract("[0-9]+") %>% as.numeric())
]

tbi$trip[mode_type_detailed %>% str_detect("Household vehicle"),
                             vehicle_id := paste0(hh_id,
                                    str_extract(mode_type_detailed, "[0-9]+") %>%
                                      str_extract("[0-9]+") %>%
                                      str_pad(width = 2, pad = 0))
                             ]

# age of vehicle ----------
tbi$vehicle[, year_num :=
              ifelse(year != "Missing",
                     as.character(year),
                     NA_character_) %>%
              str_sub(1, 4) %>%
              as.numeric()
              ]
tbi$vehicle[
  , veh_age := fifelse(year == "1980 or earlier", 39, max(year_num - 1, na.rm = T) - year_num)
  , survey_year
]
tbi$vehicle[, year_num := NULL]

# hh_id ----------------
tbi$vehicle[tbi$hh, on = .(hh_id), hh_weight := i.hh_weight]

