#basic setup
source("presentation/_load_libraries.R")
if(!exists("tbi")) source("presentation/_load_data.R")

#Mapping purpose categories

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
  x = ~purpose_mapped %>%
    str_replace_all("/", "\n"),
  y = ~prop,
  color = ~as.factor(survey_year) %>%
    fct_rev,
  type = "bar",
  colors = c(colors$councilBlue, colors$esBlue),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by Year",
    subtitle = "Source: TBI Household 2019-2023",
    x_title = "Trip Purpose",
    y_title = "Proportion of Trips (%)"
  ) %>%
  layout(
    yaxis = list(tickformat = ".0%")
  ) %>%
  print %>%
  save_image("output/trip_purpose_year.svg", width = 1000, height = 500)

# trip purpose x income x 2023 --------------

purpose_income_2023 <-
  tbi$trip_purpose %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
  filter(survey_year == 2023) %>%
  left_join(tbi$hh) %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(income_broad, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

#To get rid of Undisclosed, insert the following after left_join
#filter(income_broad != "Undisclosed")

income_plot <- plot_ly(
  data = purpose_income_2023,
  x = ~prop,
  y = ~income_broad,
  color = ~as.factor(purpose_mapped),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by Income (2023)",
    subtitle = "Source: TBI Household 2023",
    x_title = "Proportion of Trips (%)",
    y_title = "Income Bracket"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/trip_purpose_income.svg", width = 1000, height = 600)

# trip purpose x race x 2023 --------------

purpose_race_2023 <- tbi$trip_purpose %>%
  select(hh_id, survey_year, trip_purpose_weight, linked_trip_id, purpose) %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
  filter(survey_year == 2023) %>%
  left_join(tbi$hh %>%
              select(hh_id, sample_segment)) %>%
  left_join(tbi$person %>%
              select(hh_id, race_ethnicity)) %>%
  filter(!is.na(race_ethnicity)) %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(race_ethnicity, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

setDT(purpose_race_2023)
catorder_race <- purpose_race_2023[purpose_mapped == "Dining"][order(prop), race_ethnicity]

race_plot <- plot_ly(
  data = purpose_race_2023,
  x = ~prop,
  y = ~race_ethnicity %>%
    factor(levels = catorder_race, ordered = T),
  color = ~as.factor(purpose_mapped),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by Race (2023)",
    subtitle = "Source: TBI Household 2023",
    x_title = "Proportion of Trips (%)",
    y_title = "Race"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/trip_purpose_race.svg", width = 1000, height = 600)

# trip purpose x age x 2023 --------------

purpose_age_2023 <- tbi$trip_purpose %>%
  select(hh_id, survey_year, trip_purpose_weight, linked_trip_id, purpose) %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
  filter(survey_year == 2023) %>%
  left_join(tbi$hh %>%
              select(hh_id, sample_segment)) %>%
  left_join(tbi$person %>%
              select(hh_id, age)) %>%
  filter(!is.na(age)) %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(age, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

age_plot <- plot_ly(
  data = purpose_age_2023,
  x = ~prop,
  y = ~age,
  color = ~as.factor(purpose_mapped),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by Age (2023)",
    subtitle = "Source: TBI Household 2023",
    x_title = "Proportion of Trips (%)",
    y_title = "Age"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/trip_purpose_age.svg", width = 1000, height = 600)

# trip purpose x hhsize x 2023 --------------

hhsize_mapping <- c(
  "1 person" = "1 person",
  "2 people" = "2 people",
  "3 people" = "3-4 people",
  "4 people" = "3-4 people",
  "5 people" = "5-8 people",
  "6 people" = "5-8 people",
  "7 people" = "5-8 people",
  "8 people" = "5-8 people",
  "9 people" = "9 or more people",
  "10 people" = "9 or more people",
  "11 people" = "9 or more people",
  "12 or more people" = "9 or more people",
  "Missing" = "Missing"
)

purpose_hhsize_2023 <-
  tbi$trip_purpose %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
  left_join(tbi$hh) %>%
  filter(!is.na(num_people)) %>%
  mutate(hh_size = hhsize_mapping[num_people]) %>%
  filter(hh_size != "Missing") %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(hh_size, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

hhsize_plot <- plot_ly(
  data = purpose_hhsize_2023,
  x = ~prop,
  y = ~hh_size,
  color = ~as.factor(purpose_mapped),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by Household Size (2023)",
    subtitle = "Source: TBI Household 2023",
    x_title = "Proportion of Trips (%)",
    y_title = "Household Size"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/trip_purpose_hhsize.svg", width = 1000, height = 600)

# trip purpose x gender x 2023 --------------

purpose_gender_2023 <- tbi$trip_purpose %>%
  select(hh_id, survey_year, trip_purpose_weight, linked_trip_id, purpose) %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
  filter(survey_year == 2023) %>%
  left_join(tbi$hh %>%
              select(hh_id, sample_segment)) %>%
  left_join(tbi$person %>%
              select(hh_id, gender)) %>%
  filter(!is.na(gender)) %>%
  filter(gender != "Missing") %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(gender, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

gender_plot <- plot_ly(
  data = purpose_gender_2023,
  x = ~prop,
  y = ~gender %>%
    str_replace_all("/", "\n"),
  color = ~as.factor(purpose_mapped),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by Gender (2023)",
    subtitle = "Source: TBI Household 2023",
    x_title = "Proportion of Trips (%)",
    y_title = "Gender"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/trip_purpose_gender.svg", width = 1000, height = 600)

# trip purpose x trip mode x 2023 --------------

mode_mapping <- c(
  "Rail" = "Public Transit",
  "School Bus" = "School Bus",
  "Public Bus" = "Public Transit",
  "Other Bus" = "Other",
  "Long distance passenger mode" = "Long Distance",
  "Smartphone ridehailing service" = "Ride-hailing/Taxi",
  "For-Hire Vehicle" = "Ride-hailing/Taxi",
  "Household Vehicle" = "Personal Vehicle",
  "Other Vehicle" = "Other",
  "Micromobility" = "Public Transit",
  "Other" = "Other",
  "Walk" = "Walk",
  "Missing" = "Missing"
)

purpose_mode_2023 <- tbi$trip_purpose %>%
  select(hh_id, survey_year, trip_purpose_weight, linked_trip_id, purpose) %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
  filter(survey_year == 2023) %>%
  left_join(tbi$hh %>%
              select(hh_id, sample_segment)) %>%
  left_join(tbi$trip %>%
              select(hh_id, mode_type)) %>%
  filter(!is.na(mode_type)) %>%
  mutate(mode = mode_mapping[mode_type]) %>%
  filter(mode != "Missing") %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(mode, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

mode_plot <- plot_ly(
  data = purpose_mode_2023,
  x = ~prop,
  y = ~mode,
  color = ~as.factor(purpose_mapped),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by Mode (2023)",
    subtitle = "Source: TBI Household 2023",
    x_title = "Proportion of Trips (%)",
    y_title = "Trip Mode"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/trip_purpose_mode.svg", width = 1000, height = 600)

# trip purpose x county x 2023 --------------

council_counties <- c("Anoka County, MN", "Carver County, MN", "Dakota County, MN",
                      "Hennepin County, MN", "Ramsey County, MN",
                      "Scott County, MN", "Washington County, MN")

purpose_county_2023 <-
  tbi$trip_purpose %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
  filter(survey_year == 2023) %>%
  left_join(tbi$hh) %>%
  filter(home_county %in% council_counties) %>%
  mutate(home_county = gsub(" County, MN", "", home_county)) %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(home_county, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

setDT(purpose_county_2023)
catorder_county <- purpose_county_2023[purpose_mapped == "Dining"][order(prop), home_county]

county_plot <- plot_ly(
  data = purpose_county_2023,
  x = ~prop,
  y = ~home_county %>%
    factor(levels = catorder_county, ordered = T),
  color = ~as.factor(purpose_mapped),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by Household Home County (2023)",
    subtitle = "Source: TBI Household 2023",
    x_title = "Proportion of Trips (%)",
    y_title = "Home County"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/trip_purpose_county.svg", width = 1000, height = 600)


# trip purpose x city class x 2023 --------------

#Using cd_2050_broad
purpose_cityclass_2023 <-
  tbi$trip_purpose %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
  filter(survey_year == 2023) %>%
  left_join(tbi$hh) %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(cd_2050_broad, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

setDT(purpose_cityclass_2023)
catorder_cityclass <- purpose_cityclass_2023[purpose_mapped == "Dining"][order(prop), cd_2050_broad]

cityclass_plot <- plot_ly(
  data = purpose_cityclass_2023,
  x = ~prop,
  y = ~cd_2050_broad %>%
    factor(levels = catorder_cityclass, ordered = T),
  color = ~as.factor(purpose_mapped),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by 2050 TPP City Classification (2023)",
    subtitle = "Source: TBI Household 2023",
    x_title = "Proportion of Trips (%)",
    y_title = "City Classification"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/trip_purpose_cityclass.svg", width = 1000, height = 600)

#Using cd_2050_rsd
purpose_cityclassrsd_2023 <-
  tbi$trip_purpose %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
  filter(survey_year == 2023) %>%
  left_join(tbi$hh) %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(cd_2050_rsd, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

setDT(purpose_cityclassrsd_2023)
catorder_cityclassrsd <- purpose_cityclassrsd_2023[purpose_mapped == "Dining"][order(prop), cd_2050_rsd]

cityclassrsd_plot <- plot_ly(
  data = purpose_cityclassrsd_2023,
  x = ~prop,
  y = ~cd_2050_rsd %>%
    factor(levels = catorder_cityclassrsd, ordered = T),
  color = ~as.factor(purpose_mapped),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by 2050 TPP City Classification (2023)",
    subtitle = "Source: TBI Household 2023",
    x_title = "Proportion of Trips (%)",
    y_title = "City Classification"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/trip_purpose_cityclassrsd.svg", width = 1000, height = 300)

#Using cd_2050
purpose_cityclassdetail_2023 <-
  tbi$trip_purpose %>%
  mutate(purpose_mapped = mapping[purpose]) %>%
  filter(!is.na(purpose_mapped)) %>%
  filter(purpose_mapped != "Home") %>%
  filter(purpose_mapped != "Missing") %>%
  filter(survey_year == 2023) %>%
  left_join(tbi$hh) %>%
  as_survey_design(
    weights = "trip_purpose_weight",
    ids = "linked_trip_id",
    strata = "sample_segment"
  ) %>%
  group_by(cd_2050, purpose_mapped) %>%
  summarize(prop = survey_prop(proportion = TRUE, se = TRUE))

setDT(purpose_cityclassdetail_2023)
catorder_cityclassdetail <- purpose_cityclassdetail_2023[purpose_mapped == "Dining"][order(prop), cd_2050]

cityclassdetail_plot <- plot_ly(
  data = purpose_cityclassdetail_2023,
  x = ~prop,
  y = ~cd_2050 %>%
    factor(levels = catorder_cityclassdetail, ordered = T),
  color = ~as.factor(purpose_mapped),
  text = ~ paste0(round(prop * 100, 0), "%"),
  textfont = list(color = "white", textangle = 0))%>%
  councilR::plotly_layout(
    main_title = "Trip Purpose by 2050 TPP City Classification (2023)",
    subtitle = "Source: TBI Household 2023",
    x_title = "Proportion of Trips (%)",
    y_title = "City Classification"
  ) %>%
  layout(barmode = 'stack',
         xaxis = list(tickformat = ".0%"),
         legend = list(traceorder = "normal")
  ) %>%
  print %>%
  save_image("output/trip_purpose_cityclassdetail.svg", width = 1000, height = 800)
