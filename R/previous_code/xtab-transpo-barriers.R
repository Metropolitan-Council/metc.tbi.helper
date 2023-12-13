transpo_barriers <-
  per21 %>%
  select(person_id, person_weight, starts_with("transportation_barriers")) %>%
  pivot_longer(
    cols = starts_with("transportation_barriers"), names_prefix = "transportation_barriers_",
    values_to = "barrier_type"
  ) %>%
  filter(!barrier_type == "Not selected") %>%
  filter(!is.na(barrier_type)) %>%
  select(-name) %>%
  mutate(barrier_type = recode_factor(barrier_type,
    "This has not happened in the past 7 days" = "None"
  ))


message("New table added: transportation barriers faced in last 7 days by person in 2021 survey, transpo_barriers21")

# Transportation barriers column for person table
transpo_barriers_col <-
  transpo_barriers %>%
  group_by(person_id) %>%
  mutate(n_barriers = length(barrier_type[!barrier_type == "None"])) %>%
  mutate(transpo_barriers = ifelse(n_barriers > 1, "Multiple", as.character(barrier_type))) %>%
  select(person_id, person_weight, transpo_barriers) %>%
  unique()

per21 <- per21 %>%
  left_join(transpo_barriers_col)

message("New variable added: transportation barriers faced in last 7 days by perso in 2021 survey, transpo_barriers")
