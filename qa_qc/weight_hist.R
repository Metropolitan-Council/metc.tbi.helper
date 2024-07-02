source("qa_qc/00_load_pkgs.R")
source("data-raw/01-get-survey-data.R")

#---- tbi household ----
tbi$hh[
  hh_weight != 0|NA
] %>%
  plot_ly(x = ~hh_weight, type = "histogram", color = ~survey_year, xbins = list(size = 100)) %>%
  layout(xaxis = list(title = "household weight"),
         yaxis = list(title = "frequency"))

tbi$hh[
  hh_weight != 0|NA
][
  ,.SD %>%
    plot_ly(x = ~hh_weight, type = "histogram", xbins = list(size = 100)) %>%
    layout(xaxis = list(title = "household weight"),
           yaxis = list(title = "frequency"),
           title = survey_year) %>% print
  ,by = survey_year
]


#---- tbi person ----
tbi$person[
  person_weight != 0|NA
] %>%
  plot_ly(x = ~person_weight, type = "histogram", color = ~survey_year, xbins = list(size = 200)) %>%
  layout(xaxis = list(title = "person weight"),
         yaxis = list(title = "frequency"))

tbi$person[
  person_weight != 0|NA
][
  ,.SD %>%
    plot_ly(x = ~person_weight, type = "histogram", xbins = list(size = 100)) %>%
    layout(xaxis = list(title = "person weight"),
           yaxis = list(title = "frequency"),
           title = survey_year) %>% print
  ,by = survey_year
]


#---- tbi day ----
tbi$day[
  day_weight != 0|NA
] %>%
  plot_ly(x = ~day_weight, type = "histogram", color = ~survey_year, xbins = list(size = 100)) %>%
  layout(xaxis = list(title = "day weight"),
         yaxis = list(title = "frequency"))

tbi$day[
  day_weight != 0|NA
][
  ,.SD %>%
    plot_ly(x = ~day_weight, type = "histogram", xbins = list(size = 100)) %>%
    layout(xaxis = list(title = "day weight"),
           yaxis = list(title = "frequency"),
           title = survey_year) %>% print
  ,by = survey_year
]

#---- tbi trip ----
tbi$trip[
  trip_weight != 0|NA
] %>%
  plot_ly(x = ~trip_weight, type = "histogram", color = ~survey_year, xbins = list(size = 200)) %>%
  layout(xaxis = list(title = "trip weight"),
         yaxis = list(title = "frequency"))

tbi$trip[
  trip_weight != 0|NA
][
  ,.SD %>%
    plot_ly(x = ~trip_weight, type = "histogram", xbins = list(size = 100)) %>%
    layout(xaxis = list(title = "trip weight"),
           yaxis = list(title = "frequency"),
           title = survey_year) %>% print
  ,by = survey_year
]
