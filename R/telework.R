source("presentation/_load_libraries.R")
if(!exists("tbi")) source("presentation/_load_data.R")
source('presentation/_plot_styling.R')

# telework x year --------------------
plot_data <-
  tbi$person %>%
  .[employment == "Employed full-time" & telework_freq != "Missing"] %>%
  .[
  , .(pop = sum(person_weight))
  , keyby = .(survey_year, telework_freq)
  ] %>%
  .[, pct := pop/sum(pop), survey_year] %>%
  print

plot_ly(data = plot_data,
        y =~ survey_year,
        x =~ pct,
        color =~ telework_freq) %>%
  add_bars() %>%
  councilR::plotly_layout(
    main_title = "Telework",
    subtitle = 'Source: TBI Household 2019-2023',
    y_title = "Frequency",
    x_title = "Pct of People"
  ) %>%
  layout(
    barmode = 'stack',
    xaxis = list(tickformat = ".0%")
  ) %>%
  print %>%
  save_image("output/telework.svg", width = 800, height = 400)

# telework x gender x year == 2023 --------------------
tbi$person[
  , gender :=
    fct_recode(
    gender,
    'Gender Diverse' = "Transgender/Non-binary/Other/prefer to self-describe"
  )
]
plot_data <-
  tbi$person %>%
  .[employment == "Employed full-time" &
      telework_freq != "Missing" &
      gender != "Missing" &
      survey_year == 2023] %>%
  .[
  , .(pop = sum(person_weight))
  , keyby = .(telework_freq, gender %>% droplevels())
  ] %>%
  .[, pct := pop/sum(pop), gender] %>%
  print

catorder <- plot_data[telework_freq == "Never"][order(-pct), gender]
plot_ly(data = plot_data,
        y =~ gender %>% factor(levels = catorder, ordered = T),
        x =~ pct,
        color =~ telework_freq) %>%
  add_bars() %>%
  councilR::plotly_layout(
    main_title = "Telework",
    subtitle = 'Source: TBI Household 2023',
    y_title = "Gender",
    x_title = "Pct of People"
  ) %>%
  layout(
    barmode = 'stack',
    xaxis = list(tickformat = ".0%")
  ) %>%
  print %>%
  save_image("output/telework_gender.svg", width = 800, height = 400)

# telework x age x year == 2023 --------------------
plot_data <-
  tbi$person %>%
  .[employment == "Employed full-time" &
      telework_freq != "Missing" &
      survey_year == 2023] %>%
  .[
  , .(pop = sum(person_weight))
  , keyby = .(telework_freq, age)
  ] %>%
  .[, pct := pop/sum(pop), age] %>%
  print

# catorder <- plot_data[telework_freq == "Never"][order(-pct), gender]
plot_ly(data = plot_data,
        y =~ age, # %>% factor(levels = catorder, ordered = T),
        x =~ pct,
        color =~ telework_freq) %>%
  add_bars() %>%
  councilR::plotly_layout(
    main_title = "Telework",
    subtitle = 'Source: TBI Household 2023',
    y_title = "Age",
    x_title = "Pct of People"
  ) %>%
  layout(
    barmode = 'stack',
    xaxis = list(tickformat = ".0%")
  ) %>%
  print %>%
  save_image("output/telework_age.svg", width = 800, height = 400)

# telework x income x year == 2023 --------------------
tbi$hh$income_detailed
plot_data <-
  tbi$person %>%
  .[employment == "Employed full-time" &
      survey_year == 2023] %>%
  .[tbi$hh, on="hh_id", income_detailed := i.income_detailed] %>%
  .[
  , .(pop = sum(person_weight))
  , keyby = .(telework_freq, income_detailed)
  ] %>%
  .[, pct := pop/sum(pop), income_detailed] %>%
  print

# catorder <- plot_data[telework_freq == "Never"][order(-pct), gender]
plot_ly(data = plot_data,
        y =~ income_detailed, # %>% factor(levels = catorder, ordered = T),
        x =~ pct,
        color =~ telework_freq) %>%
  add_bars() %>%
  councilR::plotly_layout(
    main_title = "Telework",
    subtitle = 'Source: TBI Household 2023',
    y_title = "Income",
    x_title = "Pct of People"
  ) %>%
  layout(
    barmode = 'stack',
    xaxis = list(tickformat = ".0%")
  ) %>%
  print %>%
  save_image("output/telework_income.svg", width = 800, height = 400)


(95 * 8) * (5 * 48 - 12)
str_replace_all("/", "\n")
