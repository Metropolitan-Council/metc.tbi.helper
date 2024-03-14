# income_detailed --------------
income_mapping <-
  c(
    "Prefer not to answer" = "Undisclosed",
    "Less than $15,000" = "<$15K",
    "$15,000-$24,999" = "$15-25K",
    "$25,000-$34,999" = "$25-35K",
    "$35,000-$49,999" = "$35-50K",
    "$50,000-$74,999" = "$50-75K",
    "$75,000-$99,999" = "$75-100K",
    "$100,000-$149,999" = "$100-150K",
    "$150,000-$199,999" = "$150-200K",
    "$200,000-$249,999" = "$200-$250K",
    "$250,000 or more" = "$250K+"
  )

hh19[, income_detailed :=
       income_mapping[income_detailed] %>% factor(levels = income_mapping, ordered = T)]
hh21[, income_detailed :=
       income_mapping[income_detailed] %>% factor(levels = income_mapping, ordered = T)]
rm(income_mapping)

# income_broad --------------
income_mapping <-
  c(
    "Prefer not to answer" = "Undisclosed",
    "Under $25,000" = "<$25K",
    "$25,000-$49,999" = "$25-50K",
    "$50,000-$74,999" = "$50-75K",
    "$75,000-$99,999" = "$75-100K",
    "$100,000 or more" = "$100K+"
  )

hh19[, income_broad :=
       income_mapping[income_broad] %>% factor(levels = income_mapping, ordered = T)]
hh21[, income_broad :=
       income_mapping[income_broad] %>% factor(levels = income_mapping, ordered = T)]
rm(income_mapping)
