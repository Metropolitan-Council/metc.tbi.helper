# paste0(
# '"',
# c(tbi$trip[,c(d_purpose_category, o_purpose_category)], tbi$trip[,c(d_purpose_category, o_purpose_category)]) %>%
#   unique(),
# '" = ,'
# )%>%
#   paste0(collapse = '\n') %>%
#   cat

mapping <-
  c(
    "Work" = "Work",
    "Work related" = "Work",
    "Home" = "Home",
    "Overnight" = "Home",
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
    "Not imputable" = "Not imputable",
    "Missing" = "Missing"
  )

tbi$trip[, d_purpose_category_broad := d_purpose_category]
levels(tbi$trip$d_purpose_category_broad) <- tbi$trip[, mapping[levels(tbi$trip$d_purpose_category_broad)]]

tbi$trip[, o_purpose_category_broad := o_purpose_category]
levels(tbi$trip$o_purpose_category_broad) <- tbi$trip[, mapping[levels(tbi$trip$o_purpose_category_broad)]]
