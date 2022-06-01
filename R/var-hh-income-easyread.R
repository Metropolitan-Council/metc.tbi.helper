### Vehicle Income ---------
tbi$hh <-
  tbi$hh %>%
  mutate(income_midpoint = recode_factor(income_detailed,
    `Under $15,000` =     "7500",
    `$15,000-$24,999` =   "20000",
    `$25,000-$34,999` =   "30000",
    `$35,000-$49,999` =   "42500",
    `$50,000-$74,999` =   "62500",
    `$75,000-$99,999` =   "87500",
    `$100,000-$149,999` = "125000",
    `$150,000-$199,999` = "175000",
    `$200,000-$249,999` = "225000",
    `$250,000 or more` =  "250000"
  )) %>%
  mutate(income_detailed = recode_factor(income_detailed,
    `Under $15,000` =     "<$15K",
    `$15,000-$24,999` =   "$15-25K",
    `$25,000-$34,999` =   "$25-35K",
    `$35,000-$49,999` =   "$35-50K",
    `$50,000-$74,999` =   "$50-75K",
    `$75,000-$99,999` =   "$75-100K",
    `$100,000-$149,999` = "$100-150K",
    `$150,000-$199,999` = "$150-200K",
    `$200,000-$249,999` = "$200-$250K",
    `$250,000 or more` =  "$250K+"
  )) %>%
  mutate(income_midpoint = as.numeric(as.character(income_midpoint))) %>%
  mutate(income_detailed = factor(income_detailed, levels = rev(levels(income_detailed))))

message("New numeric column: income_midpoint; New factor labels: income_detailed")