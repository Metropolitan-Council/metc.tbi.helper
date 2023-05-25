source("R/table-transpo-barriers.R")
showtext::showtext_auto()

barrierdat <-
  tbi21$transpo_barriers %>%
  left_join(tbi21$per %>% select(person_id, hh_id, ethnicity_afam, race_ethnicity)) %>%
  left_join(tbi21$hh %>% select(hh_id, income_broad)) %>%
  mutate(income_50k = recode_factor(
    income_broad,
    "$15-25K" = "Less than $50K",
    "$50-75K" = "More than $50K",
    "$75-100K" = "More than $50K",
    "$25-35K" = "Less than $50K",
    "$35-50K" = "Less than $50K",
    "$100-150K" = "More than $50K",
    "$150-200K" = "More than $50K",
    "$200-$250K" = "More than $50K",
    "$250K+" = "More than $50K",
    "Less than $15,000" = "Less than $50K"
  )) %>%
  mutate(ethnicity_afam = recode_factor(ethnicity_afam,
    "Selected" = "Yes",
    "Not Selected" = "No"
  ))

barrierdat %>%
  as_survey_design(w = person_weight) %>%
  group_by(income_50k, barrier_type) %>%
  summarize(pct = survey_prop()) %>%
  filter(!barrier_type == "None") %>%
  filter(!barrier_type == "Prefer not to answer") %>%
  filter(!income_50k == "Prefer not to answer") %>%
  ggplot(aes(x = income_50k, y = pct, fill = barrier_type)) +
  geom_col() +
  facet_wrap(~barrier_type,
    nrow = 1,
    labeller = label_wrap_gen(width = 20, multi_line = TRUE)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(limits = c(0, 0.1), labels = scales::percent) +
  scale_fill_manual(values = c(rep("#A1BAC4", 4), councilR::colors$esBlue)) +
  labs(
    x = "Household Income", y = str_wrap("Percent of Population", width = 10),
    title = str_wrap(
      width = 50,
      "Which of the following prevented you from making a trip you wanted to make in the past week?"
    ),
    caption = "Weighted at the person level. Shown separately by household income category. Asked of all adults 18+."
  ) +
  councilR::theme_council(
    use_showtext = T,
    font_sizes = list(title = 36),
    use_manual_font_sizes = T
  ) +
  theme(panel.grid.major.x = element_blank()) +
  theme(
    strip.text = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18)
  ) +
  theme(legend.position = "none")


barrierdat %>%
  mutate(ethnicity_afam = recode_factor(ethnicity_afam,
    "Yes" = "Yes",
    "Not selected" = "No"
  )) %>%
  as_survey_design(w = person_weight) %>%
  group_by(ethnicity_afam, barrier_type) %>%
  summarize(pct = survey_prop()) %>%
  filter(!barrier_type == "None") %>%
  filter(!barrier_type == "Prefer not to answer") %>%
  filter(!is.na(ethnicity_afam)) %>%
  ggplot(aes(x = ethnicity_afam, y = pct)) +
  geom_col(fill = councilR::colors$metrostatsDaPurp) +
  facet_wrap(~barrier_type, nrow = 1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(limits = c(0, 0.1), labels = scales::percent) +
  labs(
    x = "Race/Ethnicity: Black, African, or African American", y = str_wrap("Percent of Population", width = 10),
    title = "Have you experienced a barrier to transportation in last 7 days?",
    subtitle = "Shown separately for those who do & do not identify as Black, African, or African-American. Asked of all adults in survey.\n"
  ) +
  councilR::theme_council(
    use_showtext = T,
    use_manual_font_sizes = T
  ) +
  theme(panel.grid.major.x = element_blank())
