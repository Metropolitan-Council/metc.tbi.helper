source("presentation/d_trip_mode_purpose.R")

# tbi$trip[, purpose_category := fifelse(d_purpose_category == "Home", o_purpose_category, d_purpose_category)]
# tbi$trip[, purpose := fifelse(d_purpose == "Went home", o_purpose, d_purpose)]
# tbi$trip[, survey_year %>% unique() %>% paste(collapse = "_"), keyby = d_purpose] %>% print(n=50)

# Copy and paste into script to help align categories
trip_purpose[
  , survey_year %>% unique() %>% sort %>% paste0(collapse = "_")
  , keyby = .(purpose_category)
] %>%
  .[, paste0('"', purpose_category, '" = , #', V1) %>% unique() %>% paste(collapse = '\n') %>% cat]

mapping <- c(
  "Errand" = "Errand/Misc/Shopping", #2019_2021_2023
  "Errand/Other" = "Errand/Misc/Shopping", #2019_2021_2023
  "Home" = "Errand/Misc/Shopping", #2019_2021_2023
  "Not imputable" = "Errand/Misc/Shopping", #2019_2021_2023
  "Other" = "Errand/Misc/Shopping", #2021_2023
  "Overnight" = "Errand/Misc/Shopping", #2019_2021_2023
  "Missing" = "Missing", #2023
  "Escort" = "Escort", #2019_2021_2023
  "Meal" = "Dining", #2019_2021_2023
  "School" = "Education", #2019_2021_2023
  "School related" = "Education", #2019_2021_2023
  "Shopping" = "Errand/Misc/Shopping", #2019_2021_2023
  "Social/Recreation" = "Social/Rec", #2019_2021_2023
  "Work" = "Work", #2019_2021_2023
  "Work related" =  "Work" #2019_2021_2023
)
trip_purpose[, purpose_aligned := mapping[purpose_category]]
trip_purpose[purpose %>% str_detect("Medical"), purpose_aligned := "Errand/Misc/Shopping"]
trip_purpose[purpose %>% tolower() %>% str_detect("exercise"), purpose_aligned := "Exercise"]


trip_purpose <-
  trip_purpose[
    , .(trips = sum(trip_purpose_weight, na.rm = TRUE),
        person = sum(person_weight, na.rm = TRUE))
    , keyby = .(survey_year, purpose_aligned)
  ] %>%
    .[, pct := trips / sum(trips), survey_year] %>% .[
      , trip_rate := trips/person, survey_year
    ] %>%
    print

# Trip purp --------
# trip_purpose <-
#   trip_purpose19[, .(trips = sum(trip_purpose_weight)),
#                  .(purpose_aligned, year = factor(year))] %>%
#   .[, pct := trips / sum(trips)] %>%
#   rbind(
#     trip_purpose21[, .(trips = sum(trip_purpose_weight)),
#                    .(purpose_aligned, year = factor(year))] %>%
#       .[, pct := trips / sum(trips)]
#   )
fct_order <- trip_purpose[survey_year == 2023, sum(abs(trips)), purpose_aligned][order(-V1),purpose_aligned]
trip_purpose[, purpose_aligned := factor(purpose_aligned, levels = fct_order)]

plot_ly() %>%
  add_bars(data = trip_purpose[purpose_aligned != "Missing"]
           , y = ~trips
           , x = ~ purpose_aligned
           , color = ~survey_year
           , colors = c(colors$councilBlue, colors$esBlue)
           , text =~ sprintf("%sM<br>%s%%"
                             , round(trips/1000000, 2) %>% abs()
                             , round(pct * 100, 1) %>% abs())
           , textfont = list(size = 11, color = "white")
           , textangle = 0
           , textposition = "inside"
           ) %>%
  layout(
    title = "Trip Purpose"
    # , barmode = "relative"
    , font = list(size = 16)
    , yaxis = list(
      # showticklabels = F,
      # showgrid = F,
      title = "Weekday Trips"
    )
    , xaxis = list(
      title = NA
    )
    , margin = list(t = 45)
    , uniformtext = list(minsize = 12, mode = F)
    # , legend = list(traceorder = "reversed")
  ) %>%
  print %>%
  save_image("output/trip_purpose.svg", width = 1200, height = 500)


# 2021 only
# plot_ly() %>%
#   add_bars(data = trip_purpose[survey_year == 2021]
#            , x = ~trips
#            , y = ~ reorder(purpose_aligned, trips)
#            # , color = ~survey_year
#            , colors = c(colors$councilBlue, colors$esBlue)
#            , text =~ sprintf("%sM (%s%%)"
#                              , round(trips/1000000, 2) %>% abs()
#                              , round(pct * 100, 1) %>% abs())
#            , textfont = list(size = 11, color = "white")
#            , textangle = 0
#            , textposition = "inside"
#   ) %>%
#   plotly_layout(
#     main_title = "Trip Purpose",
#     subtitle = "Source: TBI Household 2021",
#     legend_title = "",
#     y_title = "Trip Purpose",
#     x_title = "Average Weekday Trips"
#   )

