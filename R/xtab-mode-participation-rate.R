# packages-------------------------------------------
packages <- list("bit64", "dplyr", "ggplot2", "plotly", "srvyr", "councilR", "sysfonts", "showtext", "purrr")
invisible(lapply(packages, library, character.only = TRUE))
rm(packages)

tbi <- readRDS("data/tbi_extract.RData")

source("R/df-lump-mode-types.R")

mode_share_ls <- list()

for (a_mode_type in c("Drive", "Transit", "Walk", "Bicycle", "Other")) {
  mode_share_ls[[a_mode_type]] <-
    tbi$trip %>%
    # Find just the trips using that mode:
    filter(mode_type_cond == !!a_mode_type) %>%
    # ... and the people who made those trips:
    select(person_id, day_num) %>%
    unique() %>%
    # flag for "used that mode"
    mutate(used_mode = 1) %>%
    # Join to day table (repeated measures of mode use over time == need for day table.)
    full_join(tbi$day %>% select(person_id, day_num, day_weight)) %>%
    # only weekdays; only complete survey days:
    filter(day_weight > 0) %>%
    # add flag for "didn't use that mode" - 0:
    mutate(used_mode = replace(used_mode, is.na(used_mode), 0)) %>%
    # filter to adults:
    left_join(tbi$per %>% select(person_id, age)) %>%
    filter(!age %in% c("Under 5", "5-15", "16-17")) %>%
    # get survey total and proportion:
    srvyr::as_survey_design(w = day_weight) %>%
    group_by(used_mode) %>%
    summarize(
      total = survey_total(),
      pct = 100 * survey_prop()
    )
}

# have to add days with no travel:
no_travel_share <-
  tbi$day %>%
  select(person_id, day_num, day_weight, num_trips) %>%
  mutate(zero_trips = case_when(
    num_trips == 0 ~ 1,
    num_trips > 0 ~ 0
  )) %>%
  # only weekdays; only complete survey days:
  filter(day_weight > 0) %>%
  # filter to adults:
  left_join(tbi$per %>% select(person_id, age)) %>%
  filter(!age %in% c("Under 5", "5-15", "16-17")) %>%
  # get survey total and proportion:
  srvyr::as_survey_design(w = day_weight) %>%
  group_by(zero_trips) %>%
  summarize(
    total = survey_total(),
    pct = 100 * survey_prop()
  ) %>%
  filter(zero_trips == 1) %>%
  select(-zero_trips) %>%
  mutate(Mode = "No travel")

mode_share <-
  rbindlist(mode_share_ls, use.names = T, idcol = "Mode") %>%
  filter(used_mode == 1) %>%
  select(-used_mode) %>%
  bind_rows(no_travel_share) %>%
  # round off the numbers to reasonable levels:
  mutate(across(c(total, total_se), round, -2)) %>%
  mutate(across(c(pct, pct_se), round, 1))

# Transit has its own frequency of use question:
transit_freq_self_reported <-
  tbi$per %>%
  select(person_id, person_weight, age, transit_freq) %>%
  filter(!age %in% c("Under 5", "5-15", "16-17")) %>%
  # get rid of those who did not answer as well:
  filter(!is.na(transit_freq)) %>%
  srvyr::as_survey_design(w = person_weight) %>%
  group_by(transit_freq) %>%
  summarize(
    total = survey_total(),
    pct = 100 * survey_prop()
  ) %>%
  # round off the numbers to reasonable levels:
  mutate(across(c(total, total_se), round, -2)) %>%
  mutate(across(c(pct, pct_se), round, 1))


# For Eric
datlist <- list(
  "transit_frequency_selfreported" = transit_freq_self_reported,
  "mode_partn_rate_from_trips" = mode_share
)
openxlsx::write.xlsx(datlist, file = "data/mode-participation-rate.xlsx")
