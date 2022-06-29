# Load necessary packages ------
source("data-raw/00-load-pkgs.R")
library(srvyr)

# data -----
load("data/tbi19.rda")
load("data/tbi21.rda")

# create one data frame from both surveys -----
get_modetable <- function(surveyobj, year) {
  surveyobj$day %>%
    select(hh_id, person_id, day_num, day_weight, num_trips) %>%
    left_join(surveyobj$hh %>% select(hh_id, sample_segment), by = "hh_id") %>%
    left_join(surveyobj$trip %>% select(person_id, hh_id, day_num, mode_group),
              by = c("person_id", "day_num", "hh_id")) %>%
    mutate(svy_year = !!year) %>%
    unique()
}

modetab <- base::rbind(
  get_modetable(tbi19, year = "2018-2019"),
  get_modetable(tbi21, year = "2021")
)

# mode share calculation ----
mode_participation_ls <- list()

for (a_mode_type in list("Drive", "Transit", "Walk", "Bicycle", "Other")) {
  dt <- copy(modetab)
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

mode_part_pct %>%
  filter(used_mode == "Used mode") %>%
  ggplot(mode_part_pct,
       aes(x = svy_year, y = pct)) +
  geom_col() +
  facet_wrap(~mode_group, scales = "free_y") +
  theme_council_open()




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
