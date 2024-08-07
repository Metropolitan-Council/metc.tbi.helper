source("presentation/_load_libraries.R")
source("presentation/_load_data.R")

trip[survey_year == '2019', depart_time := depart_time + 60^2]
trip[survey_year == '2019', depart_time_imputed := depart_time_imputed + 60^2]
trip[, depart_time := fifelse(!is.na(depart_time_imputed), depart_time_imputed, depart_time)]
trip[, depart_time_imputed := NULL]
setkey(trip, linked_trip_id, leg_num)


# Copy and paste into script to help align categories
person[telework_freq == "1 day every 2 weeks", telework_freq := "1-3 days a month"]
person[, telework_freq %>% unique() %>% paste(collapse = "', '")]
temp_levels <- c(
  'Never',
  'Less than monthly',
  '1-3 days a month',
  '1 day a week',
  '2-3 days a week',
  '4 days a week',
  '5 days a week',
  '6-7 days a week',
  'Missing'
)
person[, telework_freq := factor(telework_freq, temp_levels, ordered = TRUE)]


person[, .N, employment]

# telework freq -----
telework <-
person[
  employment %in% c("Employed full-time")
  , .(pop = sum(person_weight, na.rm = TRUE), .N)
  , keyby = .(survey_year, telework_freq)
] %>%
  .[, pct := pop/sum(pop), survey_year] %>%
  print

plot_ly() %>%
  add_bars(data = telework[!telework_freq %in% c("Missing", "Never")]
            , x= ~ telework_freq
            , y= ~ pop
            , color=~survey_year
            , colors = c(colors$councilBlue, colors$esBlue)
  ) %>%
  layout(
    title = "Telework Yes",
    # yaxis = list(title = 'Survey/Max(Surveys)'),
    yaxis = list(title = 'People'),
    xaxis = list(title = "Telework")
    , font = list(size = 16)
    , margin = list(t = 60)
  ) %>%
  print %>%
  save_image("output/telework.svg", width = 800, height = 500)


telework[, telework := case_when(
  telework_freq == "Never" ~ "No",
  telework_freq == "Missing" ~ "Missing",
  .default = "Yes"
)]

plot_ly() %>%
  add_bars(data = telework[, .(pop = sum(pop, na.rm = TRUE), N = sum(N)), .(telework, survey_year)]
            , x= ~ telework
            , y= ~ pop
            , color= ~survey_year
            , colors = c(colors$councilBlue, colors$esBlue)
  ) %>%
  layout(
    # yaxis = list(title = 'Survey/Max(Surveys)'),
    yaxis = list(title = 'People'),
    xaxis = list(title = "Telework")
    , font = list(size = 16)
  ) %>%
  print %>%
  save_image("output/telework2.svg", width = 400, height = 400)

# trip distance ---------
trip[, teleworks := "No Telework"]
trip[person[telework_freq >= "1 day a week"], on = "person_id", teleworks := "Telework"]
trip[, .N, teleworks]

plot_data <-
trip[
  , .(trip_weight = max(trip_weight), distance_miles, mode_type, teleworks)
  , .(linked_trip_id, survey_year)
][
  distance_miles < 40 &
    distance_miles > 0 &
    mode_type == "Household Vehicle"
  , .(N = sum(trip_weight, na.rm = TRUE))
  ,  keyby = .(distance_miles %>% ceiling, survey_year, teleworks)] %>%
  .[, legend := paste0(survey_year, " - ", teleworks)] %>%
  .[, pct := N/sum(N), .(survey_year, teleworks)] %>%
  print

  # pal <- c("#908aff", "#4A40FF", "#ff91af", "#FF295F")
  # setNames(pal, c(
  #   "2019 - No Telework"
  #   , "2019 - Telework"
  #   , "2021 - No Telework"
  #   , "2021 - Telework"
  #   , "2023 - No Telework"
  #   , "2023 - Telework"
  # ))

  subplot(
    plot_data %>%
      .[survey_year == 2019] %>%
      plot_ly() %>%
      add_lines(
        x = ~ distance_miles
        , y = ~ pct
        , color = ~ teleworks
        , colors = c("No Telework" = "#908aff", "Telework" = "#FF295F")
      ) %>%
      layout(
        title = "Trip Distances"
        , font = list(size = 16)
        , yaxis = list(
          title = "Percent of Weekday Trips"
          , tickformat = ".0%"
        )
        , xaxis = list(
          title = "Miles<br>2019"
        )
        , margin = list(t = 45)
        , uniformtext=list(minsize=12, mode = FALSE)
        , legend = list(traceorder = "reversed")
      ),
    plot_data %>%
      .[survey_year == 2021] %>%
      plot_ly() %>%
      add_lines(
        x = ~ distance_miles
        , y = ~ pct
        , color = ~ teleworks
        , showlegend = FALSE
        , colors = c("No Telework" = "#908aff", "Telework" = "#FF295F")
      ) %>%
      layout(
        title = "Trip Distances"
        , font = list(size = 16)
        , yaxis = list(
          title = "Percent of Weekday Trips"
          , tickformat = ".0%"
        )
        , xaxis = list(
          title = "Miles<br>2021"
        )
        , margin = list(t = 45)
        , uniformtext=list(minsize=12, mode = FALSE)
        # , legend = list(traceorder = "reversed")
      ),
    plot_data %>%
      .[survey_year == 2023] %>%
      plot_ly() %>%
      add_lines(
        x = ~ distance_miles
        , y = ~ pct
        , color = ~ teleworks
        , showlegend = FALSE
        , colors = c("No Telework" = "#908aff", "Telework" = "#FF295F")
      ) %>%
      layout(
        title = "Vehicle Trip Distances"
        , font = list(size = 16)
        , yaxis = list(
          title = "Percent of Weekday Trips"
          , tickformat = ".0%"
        )
        , xaxis = list(
          title = "Miles<br>2023"
        )
        , margin = list(t = 45)
        , uniformtext=list(minsize=12, mode = FALSE)
        # , legend = list(traceorder = "reversed")
      ),
    shareX = TRUE, shareY = TRUE
  ) %>%
  print %>%
    save_image("output/trip_distance.svg", width = 1000, height = 500)



# travel freq ---------
# trip[household, on="hh_id", sample_segment := i.sample_segment]
#
#   plot_data <-
#     trip[
#         # mode_type == "Household Vehicle"
#       , .(trips = uniqueN(linked_trip_id))
#       , .(survey_year, teleworks, person_id, travel_date)] %>%
#     .[
#       , .(avgTrips = mean(trips))
#       , .(survey_year, teleworks)
#     ] %>%
#     print
#
#   plot_data <-
#   trip[
#     # mode_type == "Household Vehicle"
#     , .(trips = uniqueN(linked_trip_id)
#         , trip_weight = trip_weight %>% sum(na.rm = TRUE))
#     , .(survey_year, teleworks, person_id, travel_date, sample_segment)] %>%
#   srvyr::as_survey_design(weights = trip_weight, strata = sample_segment) %>%
#     group_by(survey_year, teleworks) %>%
#     summarize(
#       avgTrips = survey_mean(trips, vartype = "ci"),
#       ct = survey_total()
#     ) %>%
#     as.data.table() %>%
#     print
#
#   plot_data[
#     , moe := (avgTrips_upp - avgTrips_low)/2
#   ] %>%
#     print
#
#   plot_data %>%
#     plot_ly() %>%
#     add_bars(
#       y= ~ avgTrips
#       , x= ~ survey_year
#       , color = ~ teleworks
#       , colors = c("No Telework" = "#908aff", "Telework" = "#FF295F")
#       , error_y = ~ list(
#         array = moe
#         , color = 'black'
#       )
#       ) %>%
#     layout(
#       # title = ""
#       font = list(size = 16)
#       , yaxis = list(
#         title = "Ave Number of Trips"
#         # , tickformat = ".0%"
#       )
#       , xaxis = list(
#         title = "Year"
#       )
#       , margin = list(t = 45)
#       , uniformtext=list(minsize=12, mode=F)
#       # , legend = list(traceorder = "reversed")
#     ) %>%
#     print %>%
#     save_image("output/number_of_trips.svg", width = 600, height = 700)








  trip[trip[leg_num > 1 & trip_weight >0 , .N, linked_trip_id], on="linked_trip_id"] %>%
    .[, trip_weight %>% round %>% unique %>% paste0(collapse = ','), .(survey_year, linked_trip_id)] %>%
    .[sample(.N, 10)] %>%
    setorder(survey_year) %>%
    print


  day()
  trip[, .N, .(survey_year, depart_dow)]
