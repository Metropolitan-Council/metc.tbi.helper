source("presentation/_load_libraries.R")
source("presentation/_load_data.R")

# trip[survey_year == '2019', depart_time := depart_time + 60^2]
# trip[survey_year == '2019', depart_time_imputed := depart_time_imputed + 60^2]

trip[, doy := depart_time %>%
           as.Date() %>%
           wday(label = T)]

trip[, depart_floor :=
           fifelse(survey_year == 2019, depart_time_imputed, depart_time) %>%
           floor_date("30 min") %>%
           as.ITime() %>%
           as.POSIXct()
           ]

trips <-
  trip[
    # # use only rMove trips
    # household[participation_group %>% str_detect("rMove"), .(hh_id)]
    # , on = "hh_id"
  ][
    trip_weight > 0,
    .(
      trip_weight = max(trip_weight, na.rm = TRUE), depart_floor
    ),
    keyby = .(linked_trip_id, survey_year)
  ][
    , .(
      trips = sum(trip_weight, na.rm = TRUE),
      max_wt = quantile(trip_weight, prob = 0.95, na.rm = TRUE)
      , surveys = .N %>% as.numeric()
    )
    , .(depart_floor, survey_year)
  ] %>%
  .[, surveys := surveys/max(surveys, na.rm = TRUE), survey_year] %>%
  na.omit()

# weighted  ------
Sys.timezone()
trips[, depart_floor_central := as.POSIXct(format(depart_floor, tz = "America/Chicago", usetz = TRUE))]
plot_ly() %>%
  add_lines(data = trips
            , x= ~ depart_floor_central
            , y= ~ trips
            , fill = "tozeroy"
            , color= ~survey_year
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



