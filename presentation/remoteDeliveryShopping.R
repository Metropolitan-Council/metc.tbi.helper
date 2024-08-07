source("presentation/_load_libraries.R")
source("presentation/_load_data.R")

person %>% names() %>% str_subset("job")
trip %>% names() %>% str_subset("on")

person[, .N, keyby = .(survey_year, job_type)]

trip[, .N, d_purpose_category]
trip[d_purpose_category == "Work related", .N, .(survey_year, driver, d_purpose)] %>% View

trip %>% sample_n(1)
trip[, uniqueN(speed_mph), .(survey_year)]

trip[, .N, mode_type]
trip[
  mode_type %>% str_detect("Vehicle")
  , .(speed = quantile(speed_mph, na.rm = TRUE, prob = 0.5))
  , .(survey_year, mode_type)
] %>%
  plot_ly() %>%
  add_bars(x= ~mode_type, y= ~speed, color= ~survey_year)
