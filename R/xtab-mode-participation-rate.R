# Load necessary packages ------
source("data-raw/00-load-pkgs.R")

# data -----
load("data/tbi19.rda")
load("data/tbi21.rda")

# create one data frame from both surveys -----
get_modetable <- function(surveyobj, year) {
  modes <-
    surveyobj$trip %>%
    select(mode_group, mode_type, mode_type_detailed) %>%
    filter(!is.na(mode_type) & !is.na(mode_type_detailed)) %>%
    unique()

  dt <-
    surveyobj$day %>%
    select(hh_id, person_id, day_num, day_weight, num_trips) %>%
    left_join(surveyobj$hh %>% select(hh_id, sample_segment), by = "hh_id") %>%
    left_join(
      surveyobj$trip %>% select(person_id, trip_id, day_num, mode_1, mode_2, mode_3),
      by = c("person_id", "day_num")
    ) %>%
    unique() %>%
    pivot_longer(
      cols = c("mode_1", "mode_2", "mode_3"),
      names_to = "mode_num",
      values_to = "mode_type_detailed"
    ) %>%
    unique() %>%
    filter(!is.na(mode_type_detailed)) %>%
    mutate(
      mode_type_detailed =  recode_factor(
        mode_type_detailed,
        "Bike share (electric bicycle)" = "Bike-share (electric bicycle)",
        "Bike share (regular bicycle)" = "Bike-share (regular bicycle)",
        "Dial-a-Ride (e.g., Transit Link)" = "Dial-A-Ride (e.g., Transit Link)",
        "HH vehicle 1 " =  "Household vehicle 1",
        "HH vehicle 2 " =  "Household vehicle 2",
        "HH vehicle 3" =  "Household vehicle 3",
        "HH vehicle 4" =  "Household vehicle 4",
        "HH vehicle 5" =  "Household vehicle 5",
        "HH vehicle 6 " =  "Household vehicle 6",
        "Other motorcycle/moped/scooter" = "Other motorcycle",
        "Other scooter or similar" =  "Other scooter or moped",
        "Scooter: Personal scooter or moped (not shared)" =  "Personal scooter or moped (not shared)",
        "Uber, Lyft, or other smartphone-app car service" = "Uber, Lyft, or other smartphone-app ride service",
        "Walk, jog, or roll using a mobility device" =  "Walk, jog, or roll using a wheelchair"
      )
    ) %>%
    left_join(modes, by = "mode_type_detailed") %>%
    mutate(svy_year = year)

  dt
}

modetab <- base::rbind(
  get_modetable(tbi19, year = "2018-2019"),
  get_modetable(tbi21, year = "2021")
)

# mode share calculation ----
mode_participation_ls <- list()

for (a_mode_type in list("Drive", "Transit", "Walk", "Bicycle", "Other")) {
  dt <- data.table(modetab)
  dt[,used_mode := fcase(num_trips == 0, "No travel",
                              num_trips > 0 & a_mode_type %in% unique(mode_group), "Used mode",
                              num_trips > 0 & !(a_mode_type %in% unique(mode_group)), "Used other modes"),
            by = list(person_id, day_num, day_weight)][ , mode_group := NULL]
  mode_participation_ls[[a_mode_type]] <- unique(dt)
}

mode_participation <-
  rbindlist(mode_participation_ls, use.names = T, idcol = "mode_group")

mode_part_pct <-
mode_participation %>%
  as_survey_design(w = day_weight, strata = sample_segment) %>%
  group_by(svy_year, mode_group, used_mode) %>%
  summarize(pct = survey_prop())

mode_part_pctb4 %>%
  filter(used_mode == "Used mode") %>%
  ggplot(aes(x = svy_year, y = pct)) +
  geom_col() +
  facet_wrap(~mode_group, scales = "free_y") +
  councilR::theme_council_open()

# Self-reported participation rate


# Transit has its own frequency of use question:
transit_freq_self_reported <-
  tbi19$per %>%
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
