# This script is writen to run after
# 08-derive-var-trip-mode-group.R


trip19[
  , distance_adj :=
    fifelse(
      participation_group == "Online or call center recruit, online or call center diary",
      distance * 1.35,
      distance
    )
]



trip21[household21, on = .(hh_id), participation_group := i.participation_group]
trip21[, survey_group := word(participation_group, -1)]
trip21[
  , distance_adj :=
    fifelse(
      survey_group %in% c("Center", "Web"),
      distance * 1.35,
      distance
    )
]
trip21[, c("survey_group", "participation_group") := NULL]
