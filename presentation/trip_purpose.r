#basic setup
source("presentation/_load_libraries.R")
if(!exists("tbi")) source("presentation/_load_data.R")

#trip purpose setup
mapping <- c(
  "Not imputable" = "Errand/Shopping/Misc",
  "Home" = "Errand/Shopping/Misc",
  "Work" = "Work",
  "Work related" = "Work",
  "School" = "Education",
  "School related" = "Education",
  "Escort" = "Escort",
  "Shopping" = "Errand/Shopping/Misc",
  "Meal" = "Dining",
  "Social/Recreation" = "Social/Rec",
  "Errand" = "Errand/Shopping/Misc",
  "Overnight" = "Errand/Shopping/Misc",
  "Missing" = "Unknown",
  "Other" = "Errand/Shopping/Misc",
  "NA" = "Unknown"
)


# trip purpose x year --------------
purpose_year <-
  tbi$trip_purpose %>%
  mutate(purpose_mapped = mapping[purpose_category]) %>%
  left_join(tbi$hh) %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(survey_year, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

#purpose_year[,sum(prop),survey_year] Check to make sure prop equals 1, setDT first

year_plot <- plot_ly(
  data = purpose_year,
  x = ~purpose_mapped,
  y = ~prop,
  color = ~as.factor(survey_year),
  type = "bar",
  colors = c(colors$councilBlue, colors$esBlue)
) %>%
  layout(
    xaxis = list(title = "Trip Purpose"),
    yaxis = list(title = "Proportion of Trips", tickformat = ".0%"),
    barmode = "group",
    title = "Trip Purpose Proportions by Year"
  ) %>%
  print

# trip purpose x income x 2023 --------------

# trip purpose x race x 2023 --------------

# trip purpose x age x 2023 --------------

# trip purpose x hhsize x 2023 --------------
