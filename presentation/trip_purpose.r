#basic setup
source("presentation/_load_libraries.R")
if(!exists("tbi")) source("presentation/_load_data.R")

#trip purpose setup

mapping <- c(
  "Not imputable" = "Shopping/Errands/Misc",
  "Went home" = "Home",
  "Went to work/work-related/volunteer-related" = "Work",  # Combined work and volunteer
  "Attended school/class" = "Education",
  "Appointment/shopping/errands" = "Shopping/Errands/Misc",
  "Drop off, pick up, accompany person" = "Escort",
  "Social/leisure/vacation activity" = "Social/Recreation/Vacation", # Combined social, leisure, and vacation
  "Primary workplace" = "Work",
  "Went to work-related activity (e.g., meeting, delivery, worksite)" = "Work",
  "Traveling for work (e.g., going to airport)" = "Work",
  "Volunteering" = "Shopping/Errands/Misc",
  "Other work-related" = "Work",
  "Attend K-12 school" = "Education",
  "Attend college/university" = "Education",
  "Attend other type of class (e.g., cooking class)" = "Education",
  "Attend other education-related activity (e.g., field trip)" = "Education",
  "Attend vocational education class" = "Education",
  "Attend daycare or preschool" = "Education",
  "Grocery shopping" = "Shopping/Errands/Misc",
  "Got gas" = "Shopping/Errands/Misc",
  "Other routine shopping (e.g., pharmacy)" = "Shopping/Errands/Misc",
  "Errand without appointment (e.g., post office)" = "Shopping/Errands/Misc",
  "Medical visit (e.g., doctor, dentist)" = "Shopping/Errands/Misc",
  "Shopping for major item (e.g., furniture, car)" = "Shopping/Errands/Misc",
  "Errand with appointment (e.g., haircut)" = "Shopping/Errands/Misc",
  "Pick-up/drop-off to/from childcare/preschool/adult care" = "Escort",
  "Pick-up/drop-off to/from K-12 school or college" = "Escort",
  "Pick-up/drop-off to/from other person's workplace" = "Escort",
  "Pick-up/drop-off to/from other person's scheduled activity (e.g., lesson, appointment)" = "Escort",
  "Other activity only (e.g., attend meeting, pick-up or drop-off item)" = "Shopping/Errands/Misc",
  "Other place to pick-up/drop-off" = "Escort",
  "Pick someone up" = "Escort",
  "Pick-up/drop-off to/from other person's home" = "Escort",
  "Drop someone off" = "Escort",
  "Accompany someone only (e.g., go along for the ride)" = "Escort",
  "BOTH pick up AND drop off" = "Escort",
  "Dined out, got coffee or take-out" = "Dining",
  "Exercise or recreation (e.g., gym, jog, bike, walk dog)" = "Social/Recreation/Vacation",
  "Social activity (e.g., visit friends/relatives)" = "Social/Recreation/Vacation",
  "Leisure/entertainment/cultural (e.g., cinema, museum, park)" = "Social/Recreation/Vacation",
  "Religious/civic/volunteer activity" = "Shopping/Errands/Misc",
  "Vacation/traveling" = "Social/Recreation/Vacation",
  "Family activity (e.g., watch child's game)" = "Social/Recreation/Vacation",
  "Changed or transferred mode (e.g., waited for bus, drove onto ferry)" = "Shopping/Errands/Misc",
  "Other shopping or errand (e.g., pharmacy, post office)" = "Shopping/Errands/Misc",
  "Other leisure activity" = "Social/Recreation/Vacation",
  "Spent the night at non-home location out of region" = "Social/Recreation/Vacation",
  "Spent the night at non-home location in region" = "Social/Recreation/Vacation",
  "Other reason" = "Shopping/Errands/Misc",
  "Went to another residence (e.g., someone else's home, second home)" = "Social/Recreation/Vacation",
  "Loop trip (split by analyst at furthest location)" = "Shopping/Errands/Misc",
  "Went to temporary lodging (e.g., hotel, vacation rental)" = "Social/Recreation/Vacation",
  "Missing" = "Missing",
  "Other purpose" = "Shopping/Errands/Misc"
)


# trip purpose x year --------------
purpose_year <-
  tbi$trip_purpose %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
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
  color = ~as.factor(survey_year) %>%
    fct_rev,
  type = "bar",
  colors = c(colors$councilBlue, colors$esBlue),
  text = ~ paste0(round(prop * 100, 1), "%"),
  textfont = list(color = "white", textangle = 0),
  error_y = list(
    type = "data",
    array= ~prop_se,
    visible = TRUE,
    color = "grey"
  )
  ) %>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by Year",
    subtitle = "Source: TBI Household",
    x_title = "Trip Purpose",
    y_title = "Proportion of Trips (%)"
  ) %>%
  print %>%
  save_image("output/trip_purpose_year.svg", width = 700, height = 400)

# trip purpose x income x 2023 --------------

# trip purpose x race x 2023 --------------

# trip purpose x age x 2023 --------------

# trip purpose x hhsize x 2023 --------------
