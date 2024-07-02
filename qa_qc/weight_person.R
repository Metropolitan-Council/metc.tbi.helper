source("qa_qc/00_load_pkgs.R")
source("data-raw/01-get-survey-data.R")

#---- person gender ----
p_gender <- tbi$person[
  person_weight != 0 | NA, sum(person_weight),
  keyby = .(gender, survey_year)
] %>%
  setnames("V1", "sum_person_wt")
p_gender <- dcast(p_gender, gender ~ survey_year, value.var = "sum_person_wt")

p_gender[
  , .SD %>%
    plot_ly() %>%
    # add_segments(x = ~`2019`, xend = ~`2023`, y = ~gender, yend = ~gender) %>%
    add_markers(x = ~`2019`, y = ~gender, name = "2019") %>%
    add_markers(x = ~`2021`, y = ~gender, name = "2021") %>%
    add_markers(x = ~`2023`, y = ~gender, name = "2023") %>%
    layout(
      xaxis = list(title = "sum person weight"),
      yaxis = list(title = NA)
    )
]


#---- age ----
p_age <- tbi$person[
  person_weight != 0 | NA, sum(person_weight),
  keyby = .(age, survey_year)
] %>%
  setnames("V1", "sum_person_wt")
p_age <- dcast(p_age, age ~ survey_year, value.var = "sum_person_wt")

p_age[
  , .SD %>%
    plot_ly() %>%
    # add_segments(x = ~`2019`, xend = ~`2023`, y = ~age, yend = ~age) %>%
    add_markers(x = ~`2019`, y = ~age, name = "2019") %>%
    add_markers(x = ~`2021`, y = ~age, name = "2021") %>%
    add_markers(x = ~`2023`, y = ~age, name = "2023") %>%
    layout(
      xaxis = list(title = "sum person weight"),
      yaxis = list(title = NA)
    )
]

#---- employment ----
p_employ <- tbi$person[
  person_weight != 0 | NA, sum(person_weight),
  keyby = .(employment, survey_year)
] %>%
  setnames("V1", "sum_person_wt")
p_employ <- dcast(p_employ, employment ~ survey_year, value.var = "sum_person_wt")

p_employ[
  , .SD %>%
    plot_ly() %>%
    # add_segments(x = ~`2019`, xend = ~`2023`, y = ~employment, yend = ~employment) %>%
    add_markers(x = ~`2019`, y = ~employment, name = "2019") %>%
    add_markers(x = ~`2021`, y = ~employment, name = "2021") %>%
    add_markers(x = ~`2023`, y = ~employment, name = "2023") %>%
    layout(
      xaxis = list(title = "sum person weight"),
      yaxis = list(title = NA)
    )
]


#---- student status ----
p_stu <- tbi$person[
  person_weight != 0 | NA, sum(person_weight),
  keyby = .(student, survey_year)
] %>%
  setnames("V1", "sum_person_wt")
p_stu <- dcast(p_stu, student ~ survey_year, value.var = "sum_person_wt")

p_stu[
  , .SD %>%
    plot_ly() %>%
    # add_segments(x = ~`2019`, xend = ~`2023`, y = ~student, yend = ~student) %>%
    add_markers(x = ~`2019`, y = ~student, name = "2019") %>%
    add_markers(x = ~`2021`, y = ~student, name = "2021") %>%
    add_markers(x = ~`2023`, y = ~student, name = "2023") %>%
    layout(
      xaxis = list(title = "sum person weight"),
      yaxis = list(title = NA)
    )
]


#---- education ----
p_edu <- tbi$person[
  person_weight != 0 | NA, sum(person_weight),
  keyby = .(education, survey_year)
] %>%
  setnames("V1", "sum_person_wt")
p_edu <- dcast(p_edu, education ~ survey_year, value.var = "sum_person_wt")

p_edu[
  , .SD %>%
    plot_ly() %>%
    # add_segments(x = ~`2019`, xend = ~`2023`, y = ~student, yend = ~student) %>%
    add_markers(x = ~`2019`, y = ~education, name = "2019") %>%
    add_markers(x = ~`2021`, y = ~education, name = "2021") %>%
    add_markers(x = ~`2023`, y = ~education, name = "2023") %>%
    layout(
      xaxis = list(title = "sum person weight"),
      yaxis = list(title = NA)
    )
]
