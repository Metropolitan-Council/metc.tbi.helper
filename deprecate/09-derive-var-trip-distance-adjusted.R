# TODO: Deeper dive warranted to determine transformation.
# trip19[, mean(distance_miles, na.rm = T), participation_group]
# trip19[, median(distance_miles, na.rm = T), participation_group]

# tempDists <- trip19[
#   !is.na(distance_miles)
#   , .(center_dist = median(distance_miles), .N)
#   , .(participation_group %>% str_detect("rMove"))
#   ] %>%
#   print
# tempDists[, diff(center_dist)/min(center_dist)]
#
# trip19[
#   , distance_adj :=
#     fifelse(
#       participation_group == "Online or call center recruit, online or call center diary",
#       distance * 1.35,
#       distance
#     )
# ]
#
# trip21[household21, on = .(hh_id), participation_group := i.participation_group]
# trip21[, survey_group := word(participation_group, -1)]
# trip21[
#   , distance_adj :=
#     fifelse(
#       survey_group %in% c("Center", "Web"),
#       distance * 1.35,
#       distance
#     )
# ]
# trip21[, c("survey_group", "participation_group") := NULL]
