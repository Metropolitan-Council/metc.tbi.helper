source("qa_qc/00_load_pkgs.R")
source("data-raw/01-get-survey-data.R")

# scaled weight
tbi$hh[hh_weight != 0|NA, .N, survey_year]
tbi$hh[, scaled_weight := fcase(survey_year == 2021, hh_weight*7905/7516,
                            survey_year == 2023, hh_weight*3749/7516,
                            survey_year == 2019, hh_weight)]

#---- HH size ----
# sum of weight
hh_size <- tbi$hh[
  , num_people := fifelse(
  num_people %in% c("9 people", "10 people", "11 people", "12 or more people"), "9 or more people", as.character(num_people)
)
][
  hh_weight != 0|NA, sum(hh_weight), keyby = .(num_people, survey_year)
  ] %>%
  setnames("V1", "sum_hh_wt")
hh_size <- dcast(hh_size, num_people ~ survey_year, value.var = "sum_hh_wt")

hh_size[
  , .SD %>%
    plot_ly() %>%
    #add_segments(x = ~`2019`, xend = ~`2023`, y = ~num_people, yend = ~num_people) %>%
    add_markers(x = ~`2019`, y = ~num_people, name = "2019") %>%
    add_markers(x = ~`2021`, y = ~num_people, name = "2021") %>%
    add_markers(x = ~`2023`, y = ~num_people, name = "2023") %>%
    layout(xaxis = list(title = "sum household weight"),
           yaxis = list(title = NA))
]

# max scaled weight
hh_size <- tbi$hh[
  hh_weight != 0|NA, max(hh_weight), keyby = .(num_people, survey_year)
] %>%
  setnames("V1", "max_hh_wt")

hh_size[, .SD %>%
              plot_ly() %>%
              add_trace(x = ~num_people, y = ~max_hh_wt, type = "scatter", mode = "lines+markers", color = survey_year) %>%
              layout(xaxis = list(title = NA),
                     yaxis = list(title = "max household weight (scaled)"))
              ]
# 8 people group for year 2023 looks odd


# mean scaled weight
hh_size <- tbi$hh[
  hh_weight != 0|NA, mean(scaled_weight), keyby = .(num_people, survey_year)
] %>%
  setnames("V1", "avg_hh_wt")

hh_size[, .SD %>%
              plot_ly() %>%
              add_trace(x = ~num_people, y = ~avg_hh_wt, type = "scatter", mode = "lines+markers", color = survey_year) %>%
              layout(xaxis = list(title = NA),
                     yaxis = list(title = "average household weight (scaled)"))
]
# large size group looks odd


#---- HH Income ----
# sum of weight
## income (broad)
# hh_income <- tbi$hh[
#   hh_weight != 0|NA, .(sum(hh_weight), .N), keyby = .(income_broad, survey_year)
# ] %>%
#   setnames("V1", "sum_hh_wt")
# hh_income <- dcast(hh_income, income_broad ~ survey_year, value.var = "sum_hh_wt")
# hh_income[
#   , .SD %>%
#     plot_ly() %>%
#     #add_segments(x = ~`2019`, xend = ~`2023`, y = ~income_broad, yend = ~income_broad) %>%
#     add_markers(x = ~`2019`, y = ~income_broad, name = "2019") %>%
#     add_markers(x = ~`2021`, y = ~income_broad, name = "2021") %>%
#     add_markers(x = ~`2023`, y = ~income_broad, name = "2023")
# ]

## income (detailed)
hh_income <- tbi$hh[
  hh_weight != 0|NA, .(sum(hh_weight), .N), keyby = .(income_detailed, survey_year)
] %>%
  setnames("V1", "sum_hh_wt")
hh_income <- dcast(hh_income, income_detailed ~ survey_year, value.var = "sum_hh_wt")
hh_income[
  , .SD %>%
    plot_ly() %>%
    add_markers(x = ~`2019`, y = ~income_detailed, name = "2019") %>%
    add_markers(x = ~`2021`, y = ~income_detailed, name = "2021") %>%
    add_markers(x = ~`2023`, y = ~income_detailed, name = "2023") %>%
    layout(xaxis = list(title = "sum household weight"),
           yaxis = list(title = NA))
]

# max scaled weight
hh_income <- tbi$hh[
  hh_weight != 0|NA, max(scaled_weight), keyby = .(income_detailed, survey_year)
] %>%
  setnames("V1", "max_hh_wt")

hh_income[, .SD %>%
              plot_ly() %>%
              add_trace(x = ~income_detailed, y = ~max_hh_wt, type = "scatter", mode = "lines+markers", color = survey_year) %>%
              layout(xaxis = list(title = NA),
                     yaxis = list(title = "max household weight (scaled)"))
]

# mean scaled weight
hh_income <- tbi$hh[
  hh_weight != 0|NA, .(mean(scaled_weight), .N), keyby = .(income_detailed, survey_year)
] %>%
  setnames("V1", "avg_hh_wt")

hh_income[, .SD %>%
                plot_ly() %>%
                add_trace(x = ~income_detailed, y = ~avg_hh_wt, type = "scatter", mode = "lines+markers", color = survey_year) %>%
                layout(xaxis = list(title = NA),
                       yaxis = list(title = "average household weight (scaled)"))
]


#---- HH Workers ----
# sum of weight
hh_workers <- tbi$hh[
  , num_workers := fifelse(
    num_workers %in% c("5 workers", "6 workers", "7 workers", "8 workers", "10 workers"), "5 or more workers", as.character(num_workers)
  )
][
  hh_weight != 0|NA, sum(hh_weight), keyby = .(num_workers, survey_year)
] %>%
  setnames("V1", "sum_hh_wt")
hh_workers <- dcast(hh_workers, num_workers ~ survey_year, value.var = "sum_hh_wt")

hh_workers[
  , .SD %>%
    plot_ly() %>%
    #add_segments(x = ~`2019`, xend = ~`2023`, y = ~num_workers, yend = ~num_workers) %>%
    add_markers(x = ~`2019`, y = ~num_workers, name = "2019") %>%
    add_markers(x = ~`2021`, y = ~num_workers, name = "2021") %>%
    add_markers(x = ~`2023`, y = ~num_workers, name = "2023") %>%
    layout(xaxis = list(title = "sum household weight"),
           yaxis = list(title = NA))
]

# max scaled weight
hh_workers <- tbi$hh[
  , num_workers := fifelse(
    num_workers %in% c("5 workers", "6 workers", "7 workers", "8 workers", "10 workers"), "5 or more workers", as.character(num_workers)
  )
][
  hh_weight != 0|NA, max(scaled_weight), keyby = .(num_workers, survey_year)
] %>%
  setnames("V1", "max_hh_wt")

hh_workers[, .SD %>%
                plot_ly() %>%
                add_trace(x = ~num_workers, y = ~max_hh_wt, type = "scatter", mode = "lines+markers", color = survey_year) %>%
                layout(xaxis = list(title = NA),
                       yaxis = list(title = "max household weight (scaled)"))
]

# mean scaled weight
hh_workers <- tbi$hh[
  hh_weight != 0|NA, mean(scaled_weight), keyby = .(num_workers, survey_year)
] %>%
  setnames("V1", "avg_hh_wt")

hh_workers[, .SD %>%
             plot_ly() %>%
             add_trace(x = ~num_workers, y = ~avg_hh_wt, type = "scatter", mode = "lines+markers", color = survey_year) %>%
             layout(xaxis = list(title = NA),
                    yaxis = list(title = "average household weight (scaled)"))
]

#---- Vehicles ----
# sum of weight
hh_vehicles <- tbi$hh[
  , num_vehicles := fifelse(
    num_vehicles %in% c("6 vehicles", "7 vehicles", "8 or more vehicles"), "6 or more vehicles", as.character(num_vehicles)
  )
][
  hh_weight != 0|NA, sum(hh_weight), keyby = .(num_vehicles, survey_year)
] %>%
  setnames("V1", "sum_hh_wt")
hh_vehicles <- dcast(hh_vehicles, num_vehicles ~ survey_year, value.var = "sum_hh_wt")

hh_vehicles[
  , .SD %>%
    plot_ly() %>%
    add_markers(x = ~`2019`, y = ~num_vehicles, name = "2019") %>%
    add_markers(x = ~`2021`, y = ~num_vehicles, name = "2021") %>%
    add_markers(x = ~`2023`, y = ~num_vehicles, name = "2023") %>%
    layout(xaxis = list(title = "sum household weight"),
           yaxis = list(title = NA))
]

# max scaled weight
hh_vehicles <- tbi$hh[
  hh_weight != 0|NA, max(scaled_weight), keyby = .(num_vehicles, survey_year)
] %>%
  setnames("V1", "max_hh_wt")

hh_vehicles[, .SD %>%
                 plot_ly() %>%
                 add_trace(x = ~num_vehicles, y = ~max_hh_wt, type = "scatter", mode = "lines+markers", color = survey_year) %>%
                 layout(xaxis = list(title = NA),
                        yaxis = list(title = "max household weight (scaled)"))
]

# mean scaled weight
hh_vehicles <- tbi$hh[
  hh_weight != 0|NA, mean(scaled_weight), keyby = .(num_vehicles, survey_year)
] %>%
  setnames("V1", "avg_hh_wt")

hh_vehicles[, .SD %>%
              plot_ly() %>%
              add_trace(x = ~num_vehicles, y = ~avg_hh_wt, type = "scatter", mode = "lines+markers", color = survey_year) %>%
              layout(xaxis = list(title = NA),
                     yaxis = list(title = "average household weight (scaled)"))
]


#---- Children ----
# sum of weight
hh_child <- tbi$hh[
  , num_kids := fifelse(
    num_kids %in% c("5 children", "6 children", "7 children", "8 children"), "5 or more children", as.character(num_kids)
  )
][
  hh_weight != 0|NA, sum(hh_weight), keyby = .(num_kids, survey_year)
] %>%
  setnames("V1", "sum_hh_wt")
hh_child <- dcast(hh_child, num_kids ~ survey_year, value.var = "sum_hh_wt")

hh_child[
  , .SD %>%
    plot_ly() %>%
    add_markers(x = ~`2019`, y = ~num_kids, name = "2019") %>%
    add_markers(x = ~`2021`, y = ~num_kids, name = "2021") %>%
    add_markers(x = ~`2023`, y = ~num_kids, name = "2023") %>%
    layout(xaxis = list(title = "sum household weight"),
           yaxis = list(title = NA))
]

# max scaled weight
hh_child <- tbi$hh[
  , num_kids := fifelse(
    num_kids %in% c("5 children", "6 children", "7 children", "8 children"), "5 or more children", as.character(num_kids)
  )
][
  hh_weight != 0|NA, max(scaled_weight), keyby = .(num_kids, survey_year)
] %>%
  setnames("V1", "max_hh_wt")

hh_child[, .SD %>%
                  plot_ly() %>%
                  add_trace(x = ~num_kids, y = ~max_hh_wt, type = "scatter", mode = "lines+markers", color = survey_year) %>%
                  layout(xaxis = list(title = NA),
                         yaxis = list(title = "max household weight (scaled)"))
]

# mean scaled weight
hh_child <- tbi$hh[
  hh_weight != 0|NA, mean(scaled_weight), keyby = .(num_kids, survey_year)
] %>%
  setnames("V1", "avg_hh_wt")
hh_child[, .SD %>%
           plot_ly() %>%
           add_trace(x = ~num_kids, y = ~avg_hh_wt, type = "scatter", mode = "lines+markers", color = survey_year) %>%
           layout(xaxis = list(title = NA),
                  yaxis = list(title = "average household weight (scaled)"))
]

