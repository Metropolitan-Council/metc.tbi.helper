#basic setup
source("presentation/_load_libraries.R")
if(!exists("tbi")) source("presentation/_load_data.R")

#trip purpose setup
mapping <- c(
  "Not imputable" = "Errand/Misc/Shopping",
  "Home" = "Errand/Misc/Shopping",
  "Work" = "Work",
  "Work related" = "Work",
  "School" = "Education",
  "School related" = "Education",
  "Escort" = "Escort",
  "Shopping" = "Errand/Misc/Shopping",
  "Meal" = "Dining",
  "Social/Recreation" = "Social/Rec",
  "Errand" = "Errand/Misc/Shopping",
  "Overnight" = "Errand/Misc/Shopping",
  "Missing" = "Missing",
  "Other" = "Errand/Misc/Shopping",
  "NA" = "Missing"
)
trip_purpose <- merge(tbi$trip_purpose, tbi$hh, by = c("hh_id", "survey_year"))
trip_purpose[, purpose_category := mapping[purpose_category]]


# trip purpose x year --------------
trip_purpose <-
  trip_purpose %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(purpose_category) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))


plot_ly() %>%
  add_bars(data = trip_purpose["purpose_category" != "Missing"]
           , y = ~prop
           , x = ~ purpose_category
           , color = ~survey_year
           , colors = c(colors$councilBlue)
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
  print
# trip purpose x income x 2023 --------------

# trip purpose x race x 2023 --------------

# trip purpose x age x 2023 --------------

# trip purpose x hhsize x 2023 --------------
