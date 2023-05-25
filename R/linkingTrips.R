# runs from
#tbi 2019

make_var_levels <- function(var){
  tbi19$dictionary[variable == var][order(value), value_label] %>%
    str_subset("Missing", negate = T)
}

tbi19$trip[, d_purpose := factor(d_purpose, levels = make_var_levels('d_purpose'), ordered = T)]
tbi19$trip[, d_purpose_imputed := factor(d_purpose_imputed, levels = make_var_levels("d_purpose_imputed"), ordered = T)]
tbi19$trip[, d_purpose_category := factor(d_purpose_category, levels = make_var_levels('d_purpose_category'), ordered = T)]
tbi19$trip[, d_purpose_category_imputed := factor(d_purpose_category_imputed, levels = make_var_levels("d_purpose_category_imputed"), ordered = T)]
tbi19$trip[, o_purpose_imputed := factor(o_purpose_imputed, levels = make_var_levels("o_purpose_imputed"), ordered = T)]
tbi19$trip[, o_purpose_category_imputed := factor(o_purpose_category_imputed, levels = make_var_levels("o_purpose_category_imputed"), ordered = T)]
tbi19$trip[, mode_type := factor(mode_type, levels = make_var_levels("mode_type"), ordered = T)]
tbi19$trip[, mode_type_detailed := factor(mode_type_detailed, levels = make_var_levels("mode_type_detailed"), ordered = T)]
tbi19$trip[, leg_dur_hr := (1 / speed_mph_imputed) * distance]

linked_trips_19 <- tbi19$trip[
  , .(
    trip_num = first(trip_num)
    , person_num = max(person_num, na.rm = TRUE)
    , day_num = first(day_num)
    , travel_date = first(travel_date)
    , leg_num = max(leg_num, na.rm = TRUE)
    , linked_trip_num = max(linked_trip_num, na.rm = TRUE)
    , depart_time = first(depart_time)
    , depart_time_imputed = first(depart_time_imputed)
    , o_bg = first(o_bg)
    , o_county = first(o_county)
    , arrive_time = last(arrive_time)
    , d_bg = last(d_bg)
    , d_county = last(d_county)
    , duration_imputed = sum(duration_imputed, na.rm = TRUE)
    , distance = sum(distance, na.rm = TRUE)
    , speed_mph_imputed = sum(distance) / sum(leg_dur_hr)
    , trip_weight = max(trip_weight, na.rm = TRUE) # What Ashley did. Is there support?
    , n_links = uniqueN(trip_id)
    , d_purpose = min(d_purpose, na.rm = TRUE)
    , d_purpose_category = min(d_purpose_category, na.rm = TRUE)
    , d_purpose_category_imputed = min(d_purpose_category_imputed, na.rm = TRUE)
    , d_purpose_imputed = min(d_purpose_imputed, na.rm = TRUE)
    , o_purpose_imputed = min(o_purpose_imputed, na.rm = TRUE)
    , o_purpose_category_imputed = min(o_purpose_category_imputed, na.rm = TRUE)
    , mode_type = min(mode_type, na.rm = TRUE)
    , mode_type_detailed = min(mode_type_detailed, na.rm = TRUE)
    , num_travelers = first(num_travelers)
    , trip_o_in_mpo = first(trip_o_in_mpo)
    , vehicle_driver = first(vehicle_driver)
    , veh_id = first(veh_id)
    , trip_o_thrive_category = first(trip_o_thrive_category)
    , trip_o_thrive_category_broad = first(trip_o_thrive_category_broad)
    , trip_d_in_mpo = last(trip_d_in_mpo)
    , trip_d_thrive_category = last(trip_d_thrive_category)
    , trip_d_thrive_category_broad = last(trip_d_thrive_category_broad)
    # , trip_id = paste(trip_id, collapse = ',')
    , travel_dow = first(travel_dow)
    , participation_group = first(participation_group)
    , trip_o_city = first(trip_o_city)
    , trip_d_city = last(trip_d_city)
    , trip_season = first(trip_season)
  )
  , .(person_id, linked_trip_id, hh_id)
]
















































