source("presentation/_load_libraries.R")
if(!exists("tbi")) source("presentation/_load_data.R")
source('presentation/_plot_styling.R')

tbi$linked_trip[, depart_time := depart_time %>%
                  format(tz = "America/Chicago", usetz = TRUE) %>%
                  as.POSIXct()]
tbi$linked_trip[!is.na(depart_time),
                depart_time_30m :=
                  paste(
                    Sys.Date(),
                    depart_time %>% floor_date("30 min") %>% as.ITime()
                  ) %>% as.POSIXct()
                ]

# tod x year -----
plot_data <-
  tbi$linked_trip %>%
  .[, sum(trip_weight), keyby = .(survey_year, depart_time_30m)]

plot_ly() %>%
  add_lines(data = plot_data[survey_year == 2019],
            x=~depart_time_30m,
            y=~V1,
            color=~survey_year,
            colors = 'Set2',
            fill = "tozeroy") %>%
  add_lines(data = plot_data[survey_year != 2019],
            x=~depart_time_30m,
            y=~V1,
            color=~survey_year %>% fct_rev()) %>%
  councilR::plotly_layout(
    main_title = "Departures by Time of Day",
    subtitle = 'Source: TBI Household 2019-2023',
    x_title = "Depart Time",
    y_title = "Weekday Trips"
  ) %>%
  layout(
    xaxis = list(tickformat = "%I %p")
  ) %>%
  print %>%
  save_image("output/tod.svg", width = 1200, height = 400)

# tod x income x year = 2023 -----
tbi$linked_trip[!is.na(depart_time),
                depart_time_mod :=
                  paste(
                    Sys.Date(),
                    depart_time %>% floor_date("60 min") %>% as.ITime()
                  ) %>% as.POSIXct()
]

plot_data <-
  tbi$linked_trip %>%
  .[tbi$hh, on='hh_id', income_detailed :=i.income_detailed] %>%
  .[tbi$hh, on='hh_id', num_people :=i.num_people] %>%
  .[income_detailed != "Undisclosed"] %>%
  .[, income_modified :=
            income_detailed %>%
            str_replace_all("[\\$K+\\<]|.*-", "") %>%
            as.numeric() * 1000] %>%
  .[, num_people :=
            num_people %>%
            str_replace_all("[ peoplersnm]", "") %>%
            as.numeric()] %>%
  .[, income_pp := income_modified/num_people] %>%
  .[, income_pp := cut(
    income_pp,
    c(0, 25000, 50000, 90000, Inf),
    c("<$25K", "$25-50K", "$50-90K", "$90K+")
  )] %>%
  # .[, .N, keyby = income_pp] %>% print
  .[survey_year == 2023, sum(trip_weight), keyby = .(income_pp, depart_time_mod)] %>%
  .[, prop := V1/max(V1), income_pp] %>%
  setkey("depart_time_mod") %>%
  print

plot_ly() %>%
  add_lines(
    # data = plot_data[income_pp %in% c("<$25K")],
    data = plot_data
    , x=~depart_time_mod
    , y=~V1
    , color=~income_pp %>% fct_rev()
    # , fill = "tozeroy"
    ) %>%
  councilR::plotly_layout(
    main_title = "Trip Departures Times by HH Income Per Person",
    subtitle = 'Source: TBI Household 2023',
    x_title = "Depart Time",
    y_title = "Weekday Trips"
  ) %>%
  layout(
    xaxis = list(tickformat = "%I %p")
    # , yaxis = list(showticklabels = F)
  ) %>%
  print %>%
  save_image("output/tod_income.svg", width = 1200, height = 400)

# mode x race x 2023 --------------
plot_data <-
  tbi$linked_trip %>%
  .[tbi$person, on='person_id', race_ethnicity := i.race_ethnicity] %>%
  # .[, .N, race_ethnicity] %>% print
  .[survey_year == 2023 & !is.na(race_ethnicity)] %>%
  .[,
    race_ethnicity_mod :=
    fcase(
    race_ethnicity == "Don't Know",  NA_character_,
    race_ethnicity == "No Say", "Undisclosed",
    race_ethnicity == "Other", "Undisclosed",
    race_ethnicity == "White", "White",
    default = "Black, Indigenous, \nand People of Color"
  )] %>%
  .[!is.na(race_ethnicity_mod)] %>%
  .[, .(sum(trip_weight), .N), keyby = .(race_ethnicity_mod, depart_time_mod)] %>%
  # .[, prop := V1/max(V1), race_ethnicity_mod] %>%
  .[, pal := race_pal_c[race_ethnicity_mod]] %>%
  setkey("depart_time_mod") %>%
  print

plot_ly() %>%
  add_lines(data = plot_data
            , x=~depart_time_mod
            , y=~V1
            , color =~ race_ethnicity_mod# %>% fct_rev()
            , colors =~ pal
            # , fill = "tozeroy"
            ) %>%
  councilR::plotly_layout(
    main_title = "Trip Departures Times by Race",
    subtitle = 'Source: TBI Household 2023',
    x_title = "Depart Time",
    y_title = "Weekday Trips"
  ) %>%
  layout(
    xaxis = list(tickformat = "%I %p")
    # , yaxis = list(showticklabels = F)
  ) %>%
  print %>%
  save_image("output/tod_race.svg", width = 1200, height = 400)


# mode x worker vs non-workers x 2023 --------------
plot_data <-
  tbi$linked_trip %>%
  .[tbi$person, on='person_id', employment := i.employment] %>%
  .[,
    employment_mod :=
      fcase(
        employment == "Employed full-time",  "Employed full-time",
        employment == "Employed part-time", "Employed part-time",
        employment == "Not employed", "Not employed",
        default = "Other/Missing"
      )] %>%
  .[, .(sum(trip_weight), .N), keyby = .(employment_mod, depart_time_mod)] %>%
  # .[, prop := V1/max(V1), race_ethnicity_mod] %>%
  setkey("depart_time_mod") %>%
  print

plot_ly() %>%
  add_lines(data = plot_data
            , x=~depart_time_mod
            , y=~V1
            , color =~ employment_mod # %>% fct_rev()
            # , fill = "tozeroy"
  ) %>%
  councilR::plotly_layout(
    main_title = "Trip Departures Times by Employment Status",
    subtitle = 'Source: TBI Household 2023',
    x_title = "Depart Time",
    y_title = "Weekday Trips"
  ) %>%
  layout(
    xaxis = list(tickformat = "%I %p")
    # , yaxis = list(showticklabels = F)
  ) %>%
  print %>%
  save_image("output/tod_employment.svg", width = 1200, height = 400)

