# Race is a select-all question, but it helps to have a single column sometimes for factor-type analysis.
# This table adds a column for "race_ethnicity_simple" that categorizes people according to the
# race/ethnicity they selected, with an option for "2 or more races" for those that tick more than one box.

# Note, we should really go through the "other_specify" column (not included in this data extract)
# to weed out the (presumably) white people who answer as "human race", "none of your business" etc :/

per_race <-
  tbi$per %>%
  select(person_id, starts_with("ethnicity")) %>%
  pivot_longer(cols = starts_with("ethnicity"), names_prefix = "ethnicity_") %>%
  filter(value == "Yes") %>%
  select(-value) %>%
  group_by(person_id) %>%
  add_tally(name = "num_races") %>%
  mutate(race = recode(name,
    "afam" = "Black or African-American",
    "white" = "White",
    "asian" = "Asian",
    "aiak" = " American Indian or Alaska Native",
    "hisp" = "Hispanic, Latino, or Spanish origin",
    "mideast" = "Middle-Eastern",
    "hapi" = "Native Hawaiian or other Pacific Islander",
    "other" = "Other"
  )) %>%
  mutate(race_ethnicity_simple = ifelse(num_races >= 2, "2 or more races", race)) %>%
  select(-num_races, -name, -race) %>%
  unique()
