# This table helps group households into those with/without members who are people of color
# (POC, African-American, Hispanic/Latino, Asian, Native American).
# Can be helpful when wanting to do an analysis of racial disparities at the household level -
# e.g., can group households into those with/without POC members to evaluate whether all-white
# households have structural advanatages in the transportation sphere.

hh_race <-
  tbi$hh %>%
  select(hh_id) %>%
  left_join(tbi$per %>% select(hh_id, starts_with("ethnicity"))) %>%
  select(-ethnicity_other) %>% # mostly white people
  pivot_longer(cols = starts_with("ethnicity"), names_prefix = "ethnicity_") %>%
  filter(value == "Yes") %>%
  mutate(
    with_poc = ifelse(name == "white", 0, 1),
    with_afam = ifelse(name == "afam", 1, 0),
    with_hisp = ifelse(name == "hisp", 1, 0),
    with_asian = ifelse(name == "asian", 1, 0),
    with_aiak = ifelse(name == "aiak", 1, 0)
  ) %>%
  group_by(hh_id) %>%
  summarize(
    hh_with_poc = max(with_poc),
    hh_with_afam = max(with_afam),
    hh_with_hisp = max(with_hisp),
    hh_with_aiak = max(with_aiak)
  )
