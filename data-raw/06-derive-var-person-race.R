# This script is writen to run after
# 05-get-dps-vehicle-weight-data.R


# Race is a select-all question, but it helps to have a single column sometimes for factor-type analysis.
# This table adds a column for "race_ethnicity_simple" that categorizes people according to the
# race/ethnicity they selected, with an option for "2 or more races" for those that tick more than one box.

# Note, we should really go through the "other_specify" column (not included in this data extract)
# to weed out the (presumably) white people who answer as "human race", "none of your business" etc :/

# 2019 -------------
race_mapping <- c(
  "race_1" = "American Indian, Alaskan Native",
  "race_2" = "Asian, Asian American",
  "race_3" = "Black, African, African American",
  "race_4" = "Hispanic, Latinx, Latino",
  "race_5" = "Middle Eastern, North African",
  "race_6" = "Native Hawaiian, Pacific Islander",
  "race_7" = "White",
  "race_997" = "Other",
  "race_998" = "Don't Know",
  "race_999" = "No Say"
)

# NOTE: Weighted Racial distribution looks Sus. ilona.regan@rsginc.com said:
# In the survey logic for 2021 (this info is in the codebook), race and
# ethnicity are only asked of related adults. Additionally, these questions are
# asked of Person 1 in the signup, but of other adults in a Daily survey,
# meaning this question may be missing for some people. If you filter the
# dataset to include only adults who have an answer for race_ethnicity, the
# percentages should be better aligned.

per_race19 <-
  person19 %>%
  .[, .SD, .SDcols = c("person_id", person19 %>% names %>% str_subset("race"))] %>%
  melt(id.vars = "person_id", variable.name = 'race', variable.factor = F) %>%
  .[value == "Selected"] %>%
  .[, race_decoded := race_mapping[race]] %>%
  .[, .(race_ethnicity = ifelse(uniqueN(race) > 1, "2 or more races", race_decoded)), keyby = person_id]
person19[per_race19, on="person_id", race_ethnicity := i.race_ethnicity]
rm(per_race19)

# 2021 --------
per_race21 <-
  person21 %>%
  .[, .SD, .SDcols = c("person_id", person21 %>% names %>% str_subset("race_\\d{1}"))] %>%
  melt(id.vars = "person_id", variable.name = 'race', variable.factor = F) %>%
  .[value == "Selected"] %>%
  .[, race_decoded := race_mapping[race]] %>%
  .[, .(race_ethnicity = ifelse(uniqueN(race) > 1, "2 or more races", race_decoded)), keyby = person_id]

race_detailed <-
  var_list[
    variable_2021 %>% str_detect("race"),
    .(variable_2021, description_unified)
  ] %>%
  .[, desc := description_unified %>% str_replace_all(".*: ", '')]
race_detailed_mapping <- race_detailed$desc
names(race_detailed_mapping) = race_detailed$variable_2021

per_race21_detailed <-
  person21 %>%
  .[, .SD, .SDcols = c("person_id", person21 %>%
                         names %>%
                         str_subset("race_\\D{1}") %>%
                         str_subset("other", T)
                       )]%>%
  melt(id.vars = "person_id", variable.name = 'race', variable.factor = F) %>%
  .[value == "Selected"] %>%
  .[, race_decoded := race_detailed_mapping[race]] %>%
  .[, .(race_ethnicity = race_decoded %>% unique() %>% paste0(collapse = '; ')), keyby = person_id]

person21[per_race21, on="person_id", race_ethnicity := i.race_ethnicity]
person21[per_race21_detailed, on="person_id", race_ethnicity_detailed := i.race_ethnicity]

# clean up --------
rm(per_race21, per_race21_detailed, race_detailed_mapping, race_mapping, race_detailed)




