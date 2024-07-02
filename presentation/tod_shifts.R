source("_load_libraries.R")
source("_load_data.R")
tbi$trip[survey_year == '2019', depart_time := depart_time + 60^2]
tbi$trip[survey_year == '2019', depart_time_imputed := depart_time_imputed + 60^2]

tbi$trip[, doy := depart_time %>%
           as.Date() %>%
           wday(label = T)]

tbi$trip[, depart_floor :=
           fifelse(survey_year == 2019, depart_time_imputed, depart_time) %>%
           floor_date("30 min") %>%
           as.ITime() %>%
           as.POSIXct()
           ]

trips <-
  tbi$trip[
    # # use only rMove trips
    # tbi$household[participation_group %>% str_detect("rMove"), .(hh_id)]
    # , on = "hh_id"
  ][
    , .(
      trips = sum(trip_weight),
      max_wt = quantile(trip_weight, prob = 0.95)
      , surveys = .N %>% as.numeric()
    )
    , .(depart_floor, survey_year)
  ] %>%
  .[, surveys := surveys/max(surveys), survey_year] %>%
  na.omit() %>%
  print

# weighted  ------
plot_ly() %>%
  add_lines(data = trips
            , x=~ depart_floor
            , y=~ trips
            , fill = "tozeroy"
            , color=~survey_year
            , colors = c(colors$councilBlue, colors$esBlue)
            ) %>%
  layout(
    # yaxis = list(title = 'Survey/Max(Surveys)'),
    yaxis = list(title = 'Trips'),
    xaxis = list(tickformat = "%I %p", title = "Depart Time")
    , font = list(size = 16)
  ) %>%
  print %>%
  save_image("output/trip_tod.svg", width = 1200, height = 400)



