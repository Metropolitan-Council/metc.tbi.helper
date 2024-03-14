# paste0(
# '"',
# c(trip19[,c(d_purpose_category, o_purpose_category)], trip21[,c(d_purpose_category, o_purpose_category)]) %>%
#   unique(),
# '" = ,'
# )%>%
#   paste0(collapse = '\n') %>%
#   cat

mapping <-
  c(
    "Work" = "Work",
    "Work related" = "Work",
    "Home" = 'Home',
    "Overnight" = 'Home',
    "School" = "School",
    "School related" = "School",
    "Errand" = "Maintenance",
    "Errand/Other" = "Maintenance",
    "Other" = "Maintenance",
    "Escort" = "Maintenance",
    "Shopping" = "Maintenance",
    "Social/Recreation" = "Social/Recreation",
    "Meal" = "Social/Recreation",
    "Change mode" = "Change mode",
    "Not imputable" = "Not imputable"
  )

### Trip table (purpose on origin & destination ends) --------------
trip19[, d_purpose_category_broad :=  mapping[d_purpose_category]]
trip19[, o_purpose_category_broad :=  mapping[o_purpose_category]]
trip21[, d_purpose_category_broad :=  mapping[d_purpose_category]]
trip21[, o_purpose_category_broad :=  mapping[o_purpose_category]]

