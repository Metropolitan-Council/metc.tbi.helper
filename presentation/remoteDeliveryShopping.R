source("_load_libraries.R")
source("_load_data.R")


tbi$household
tbi$person %>% names() %>% str_subset("job")
tbi$trip %>% names() %>% str_subset("on")

tbi$person[, .N, keyby = .(survey_year, job_type)]

tbi$trip[, .N, d_purpose_category] %>% View
tbi$trip[d_purpose_category == "Work related", .N, .(survey_year, driver, d_purpose)] %>% View

tbi$trip %>% sample_n(1)
tbi$trip[, uniqueN(speed_mph), .(survey_year)]

tbi$trip[, .N, mode_type]
tbi$trip[
  mode_type %>% str_detect("Vehicle")
  , .(speed = quantile(speed_mph, na.rm = T, prob = 0.5))
  , .(survey_year, mode_type)
] %>%
  plot_ly() %>%
  add_bars(x=~mode_type, y=~speed, color=~survey_year)
