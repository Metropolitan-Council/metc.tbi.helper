source("presentation/_load_libraries.R")
if(!exists("tbi")) source("presentation/_load_data.R")

trips <-
  tbi$trip[
    !is.na(trip_weight) & trip_weight > 0
    , .(wtd_num_trips = sum(trip_weight))
    , .(day_id)] %>%
  print

# trip rate x year ----
# join to day table to account for day's without travel
tpp <-
  tbi$day[day_weight > 0, .(hh_id, day_id, day_weight)] %>%
  merge(trips, by = "day_id", all.x = T) %>%
  merge(tbi$hh[, .(hh_id, sample_segment, hh_in_mpo, survey_year)], by = "hh_id", all.x = T) %>%
  .[, wtd_num_trips := nafill(wtd_num_trips, fill = 0)] %>%
  .[
    (hh_in_mpo)
    , .(.N, trip_rate = round(sum(wtd_num_trips)/sum(day_weight), 2))
    , keyby = .(survey_year)
  ] %>%
  print

plot_ly() %>%
  add_bars(
    data = tpp
    , y = ~ trip_rate
    , x = ~ survey_year %>% as.character()
    , color = ~ survey_year %>% as.character()
    , colors = c(colors$councilBlue, colors$esBlue)
    , text = ~ trip_rate %>% prettyNum(',')
    , textfont = list(color = "white")
  ) %>%
  layout(
    barmode = 'group'
    , yaxis = list(title = "Avg Weekday Trips per Person")
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    , legend = list(traceorder = "reversed")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/trip_rate.svg", width = 500, height = 400)

# trip rate x year x income ----
tpp <-
  tbi$day[day_weight > 0, .(hh_id, day_id, day_weight, survey_year)] %>%
  merge(trips, by = "day_id", all.x = T) %>%
  merge(tbi$hh[, .(hh_id, sample_segment, hh_in_mpo, income_broad)], by = "hh_id", all.x = T) %>%
  .[, wtd_num_trips := nafill(wtd_num_trips, fill = 0)] %>%
  .[
    (hh_in_mpo)
    , .(.N, trip_rate = round(sum(wtd_num_trips)/sum(day_weight), 1))
    , keyby = .(income_broad, survey_year)
  ] %>%
  print

tpp[, income_broad := fct_rev(income_broad)]

plot_ly() %>%
  add_bars(
    data = tpp
    , y = ~ trip_rate
    , color = ~ income_broad
    , x = ~ survey_year
    # , colors = c()
    , text = ~ trip_rate %>% prettyNum(',')
    , textfont = list(color = "white")
  ) %>%
  layout(
    barmode = 'group'
    , yaxis = list(title = "Avg Weekday Trips per Person")
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    # , legend = list(traceorder = "reversed")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/trip_rate_income.svg", width = 700, height = 400)

# trip rate x year x gender ----
tpp <-
  tbi$day[day_weight > 0, .(hh_id, day_id, person_id, day_weight)] %>%
  merge(trips, by = "day_id", all.x = T) %>%
  merge(tbi$hh[, .(hh_id, sample_segment, hh_in_mpo, survey_year)], by = "hh_id", all.x = T) %>%
  .[tbi$person, on="person_id", gender := i.gender] %>%
  .[, wtd_num_trips := nafill(wtd_num_trips, fill = 0)] %>%
  .[!gender %in% c("Male", "Female"), gender := "Other/Unknown"] %>%
  .[, gender := droplevels(gender)] %>%
  .[
    (hh_in_mpo)
    , .(.N, trip_rate = round(sum(wtd_num_trips)/sum(day_weight), 2))
    , keyby = .(survey_year, gender)
  ] %>%
  print

plot_ly() %>%
  add_bars(
    data = tpp
    , y = ~ trip_rate
    , color = ~ survey_year
    , x = ~ gender
    # , colors = c()
    , text = ~ trip_rate %>% prettyNum(',')
    , textfont = list(color = "white")
  ) %>%
  layout(
    barmode = 'group'
    , yaxis = list(title = "Avg Weekday Trips per Person")
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    , legend = list(traceorder = "reversed")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/trip_rate_gender.svg", width = 700, height = 400)

# trip rate x year x race ----
tpp <-
  tbi$day[day_weight > 0, .(hh_id, day_id, person_id, day_weight)] %>%
  merge(trips, by = "day_id", all.x = T) %>%
  merge(tbi$hh[, .(hh_id, sample_segment, hh_in_mpo, survey_year)], by = "hh_id", all.x = T) %>%
  .[tbi$person, on="person_id", race_ethnicity := i.race_ethnicity] %>%
  .[, wtd_num_trips := nafill(wtd_num_trips, fill = 0)] %>%
  .[!race_ethnicity %in% c("White"), race_ethnicity := "Non-White"] %>%
  # print
  .[
    (hh_in_mpo)
    , .(.N, trip_rate = round(sum(wtd_num_trips)/sum(day_weight), 2))
    , keyby = .(survey_year, race_ethnicity)
  ] %>%
  print

plot_ly() %>%
  add_bars(
    data = tpp
    , y = ~ trip_rate
    , color = ~ survey_year
    , x = ~ race_ethnicity
    # , colors = c()
    , text = ~ trip_rate %>% prettyNum(',')
    , textfont = list(color = "white")
  ) %>%
  layout(
    barmode = 'group'
    , yaxis = list(title = "Avg Weekday Trips per Person")
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    , legend = list(traceorder = "reversed")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/trip_rate_race.svg", width = 700, height = 400)

# trip rate x year x age ----
tpp <-
  tbi$day[day_weight > 0, .(hh_id, day_id, person_id, day_weight)] %>%
  merge(trips, by = "day_id", all.x = T) %>%
  merge(tbi$hh[, .(hh_id, sample_segment, hh_in_mpo, survey_year)], by = "hh_id", all.x = T) %>%
  .[tbi$person, on="person_id", age := i.age] %>%
  .[, wtd_num_trips := nafill(wtd_num_trips, fill = 0)] %>%
  .[
    (hh_in_mpo)
    , .(.N, trip_rate = round(sum(wtd_num_trips)/sum(day_weight), 1))
    , keyby = .(survey_year, age)
  ] %>%
  print

plot_ly() %>%
  add_bars(
    data = tpp
    , y = ~ trip_rate
    , x = ~ survey_year
    , color = ~ age
    , text = ~ trip_rate %>% prettyNum(',')
    , textfont = list(color = "white")
  ) %>%
  layout(
    barmode = 'group'
    , yaxis = list(title = "Avg Weekday Trips per Person")
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    , legend = list(traceorder = "normal")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/trip_rate_age.svg", width = 800, height = 400)

# trip rate x year x hh_size ----

tpp <-
  tbi$day[day_weight > 0, .(hh_id, day_id, person_id, day_weight)] %>%
  merge(trips, by = "day_id", all.x = T) %>%
  merge(tbi$hh[, .(hh_id, sample_segment, hh_in_mpo, survey_year, num_people)], by = "hh_id", all.x = T) %>%
  .[, wtd_num_trips := nafill(wtd_num_trips, fill = 0)] %>%
  .[, num_people := fct_collapse(num_people,
                                '3 to 5 people' =  paste0(3:5, " people"),
                                "6+ people" = paste0(6:11, " people") %>% c('12 or more people'),
  )] %>%
  .[
    (hh_in_mpo)
    , .(.N, trip_rate = round(sum(wtd_num_trips)/sum(day_weight), 2))
    , keyby = .(survey_year, num_people)
  ] %>%
  print

tpp[, num_people := fct_rev(num_people)]

plot_ly() %>%
  add_bars(
    data = tpp
    , y = ~ trip_rate
    , x = ~ survey_year
    , color = ~ num_people
    , text = ~ trip_rate %>% prettyNum(',')
    , textfont = list(color = "white")
  ) %>%
  layout(
    barmode = 'group'
    , yaxis = list(title = "Avg Weekday Trips per Person")
    , xaxis = list(title = "Year")
    , font = list(size = 16)
    , legend = list(traceorder = "normal")
    , margin = list(t = 50)
  ) %>%
  print %>%
  save_image("output/trip_rate_hhsize.svg", width = 800, height = 400)

rm(tpp, trips)

