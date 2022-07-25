# Race is a select-all question, but it helps to have a single column sometimes for factor-type analysis.
# This table adds a column for "race_ethnicity_simple" that categorizes people according to the
# race/ethnicity they selected, with an option for "2 or more races" for those that tick more than one box.

# Note, we should really go through the "other_specify" column (not included in this data extract)
# to weed out the (presumably) white people who answer as "human race", "none of your business" etc :/

per_race19 <-
  per19 %>%
  select(person_id, starts_with("ethnicity")) %>%
  pivot_longer(cols = starts_with("ethnicity"), names_prefix = "ethnicity_") %>%
  filter(value == "Yes") %>%
  select(-value) %>%
  group_by(person_id) %>%
  add_tally(name = "num_races") %>%
  ungroup() %>%
  mutate(race = recode(name,
    "afam" = "Black, African, African American",
    "white" = "White",
    "asian" = "Asian, Asian American",
    "aiak" = "American Indian, Alaskan Native",
    "hisp" = "Hispanic, Latinx, Latino",
    "mideast" = "Hispanic, Latinx, Latino",
    "hapi" = "Native Hawaiian, Pacific Islander",
    "other" = "Other"
  )) %>%
  mutate(race_ethnicity = ifelse(num_races >= 2, "2 or more races", race)) %>%
  select(person_id, race_ethnicity) %>%
  unique()

per19 <- per19 %>%
  left_join(per_race19, by = "person_id")



rm(per_race19)

per_race_broad21 <-
  per21 %>%
  select(person_id, starts_with("ethnicity")) %>%
  pivot_longer(cols = starts_with("ethnicity")) %>%
  filter(value == "Selected") %>%
  select(-value) %>%
  left_join(dictionary21 %>% select(variable, description) %>% unique(),
    by = c("name" = "variable")
  ) %>%
  mutate(description = gsub(pattern = "Race/Ethnicity -- ", replacement = "", x = description)) %>%
  group_by(person_id) %>%
  add_tally(name = "num_races") %>%
  ungroup() %>%
  mutate(race_ethnicity = ifelse(num_races >= 2, "2 or more races", description)) %>%
  select(person_id, race_ethnicity) %>%
  unique()

per_race_detailed21 <-
  per21 %>%
  select(person_id, starts_with("race")) %>%
  pivot_longer(cols = starts_with("race")) %>%
  filter(value == "Selected") %>%
  select(-value) %>%
  left_join(dictionary21 %>% select(variable, description) %>% unique(),
    by = c("name" = "variable")
  ) %>%
  mutate(description = gsub(pattern = "Race/Ethnicity ", replacement = "", x = description)) %>%
  group_by(person_id) %>%
  add_tally(name = "num_races") %>%
  ungroup() %>%
  mutate(race_ethnicity_detailed = ifelse(num_races >= 2, "2 or more races", description)) %>%
  select(person_id, race_ethnicity_detailed) %>%
  unique()

per21 <- per21 %>%
  left_join(per_race_broad21, by = "person_id") %>%
  left_join(per_race_detailed21, by = "person_id")



rm(per_race_broad21, per_race_detailed21)


message("New variable added: a simple race category, race_ethnicity")
