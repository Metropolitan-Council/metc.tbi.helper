# Race is a select-all question, but it helps to have a single column sometimes for factor-type analysis.
# This table adds a column for "race_ethnicity_simple" that categorizes people according to the
# race/ethnicity they selected, with an option for "2 or more races" for those that tick more than one box.

# Note, we should really go through the "other_specify" column (not included in this data extract)
# to weed out the (presumably) white people who answer as "human race", "none of your business" etc :/

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

per_race <-
  tbi$person %>%
  .[, .SD, .SDcols = c("person_id", names(race_mapping))] %>%
  melt(id.vars = "person_id", variable.name = "race", variable.factor = F) %>%
  .[value == "Selected"] %>%
  .[, race_decoded := race_mapping[race]] %>%
  .[, .(race_ethnicity = ifelse(uniqueN(race) > 1, "2 or more races", race_decoded)), keyby = person_id]
tbi$person[per_race, on = "person_id", race_ethnicity := i.race_ethnicity]

race_detailed <-
  var_list[
    variable %>% str_detect("^race"),
    .(variable, description)
  ] %>%
  .[, desc := description %>% str_replace_all(".*: ", "")] %>%
  .[, desc := desc %>% str_replace_all("Race-- ", "")]
race_detailed_mapping <- race_detailed$desc
names(race_detailed_mapping) <- race_detailed$variable

per_race_detailed <-
  tbi$person %>%
  .[, .SD, .SDcols = c("person_id", tbi$person %>%
    names() %>%
    str_subset("race_\\D{1}") %>%
    str_subset("other", T))] %>%
  melt(id.vars = "person_id", variable.name = "race", variable.factor = F) %>%
  .[value == "Selected"] %>%
  .[, race_decoded := race_detailed_mapping[race]] %>%
  .[, .(race_ethnicity_detailed = race_decoded %>% unique() %>% na.omit() %>% paste0(collapse = "; ")), keyby = person_id]
per_race_detailed[race_ethnicity_detailed == "", race_ethnicity_detailed := NA]

tbi$person[per_race_detailed, on = "person_id", race_ethnicity_detailed := i.race_ethnicity_detailed]

# rename race columns ------------
race_col_mapping <- c(
  "race_1" = "Native_American",
  "race_2" = "Asian",
  "race_3" = "Black",
  "race_4" = "Hispanic_Latinx_Latino",
  "race_5" = "Middle_Eastern_North_African",
  "race_6" = "Hawaiian_Pacific",
  "race_7" = "White",
  "race_997" = "Other",
  "race_998" = "Not_Known",
  "race_999" = "No_Say"
)

setnames(tbi$person, names(race_col_mapping), paste0("race_", make_clean_names(race_col_mapping)))

# clean up --------
rm(per_race, race_detailed, per_race_detailed, race_mapping, race_col_mapping)
