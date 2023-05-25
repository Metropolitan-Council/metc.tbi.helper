# Load necessary packages ------
source("data-raw/00-load-pkgs.R")


# data -----
load("data/tbi19.rda")
load("data/tbi21.rda")

modes19 <-
  tbi19$trip %>%
  select(mode_group, mode_type, mode_type_detailed) %>%
  filter(!is.na(mode_type) & !is.na(mode_type_detailed)) %>%
  unique()

modes21 <-
  read.csv("data-raw/mode_type_hierarchy.csv") %>%
  mutate(
    mode_type_detailed = recode_factor(
      mode_type_detailed,
      "Standard bicycle" = "Standard bicycle (my household's)",
      "Friend’s vehicle" = "Friend's vehicle",
      "Bus rapid transit" = "Bus rapid transit (e.g., A Line, C Line, Red Line)",
      "Electric bicycle" = "Electric bicycle (my household's)",
      "Intercity bus" = "Intercity bus (e.g., BoltBus, Greyhound)",
      "Other private shuttle/bus" = "Other private shuttle/bus (e.g., Bellair Charters, Airporter Shuttle",
      "Carshare service" = "Carshare service (e.g., Zipcar)",
      "Borrowed bicycle" = "Borrowed bicycle (e.g., a friend's)",
      "Metro Mobility" = "Paratransit/Dial-A-Ride",
      "Other motorcycle (not my household’s)" = "Other motorcycle (not my household's)",
      "Intercity rail" = "Intercity rail (e.g., Amtrak)",
      "Peer-to-peer car rental" = "Peer-to-peer car rental (e.g., Turo)"
    )
  ) %>%
  mutate(mode_type_chr = as.character(mode_type)) %>%
  mutate(
    mode_group =
      recode_factor(mode_type_chr,
        `Vehicle` = "Drive",
        `Carshare` = "Drive",
        `Taxi` = "Drive",
        `Smartphone-app ride-hailing service` = "Drive",
        `Smartphone-app ridehailing service` = "Drive",
        `Transit` = "Transit",
        `Bicycle` = "Bicycle",
        `Bicycle or e-bicycle` = "Bicycle",
        `Bike-share` = "Bicycle",
        `Scooter-share` = "Other",
        `Walk` = "Walk",
        Other = "Other",
        `School bus` = "Other",
        `Ferry` = "Other",
        `Shuttle` = "Other",
        `Long distance passenger mode` = "Other"
      )
  ) %>%
  select(-mode_type_chr, -mode_type_value)



modetab19 <-
  tbi19$day %>%
  select(hh_id, person_id, day_num, day_weight, num_trips) %>%
  left_join(tbi19$hh %>% select(hh_id, sample_segment), by = "hh_id") %>%
  left_join(
    tbi19$trip %>% select(person_id, trip_id, day_num, mode_1, mode_2, mode_3),
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
  left_join(modes19, by = "mode_type_detailed") %>%
  mutate(svy_year = "2018-2019")


modetab21 <-
  tbi21$day %>%
  select(hh_id, person_id, day_num, day_weight, num_trips) %>%
  left_join(tbi21$hh %>% select(hh_id, sample_segment), by = "hh_id") %>%
  left_join(
    tbi21$trip %>% select(person_id, trip_id, day_num, mode_1, mode_2, mode_3),
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
  left_join(modes21, by = "mode_type_detailed") %>%
  mutate(svy_year = "2021")

modetab21 %>%
  filter(is.na(mode_group)) %>%
  select(mode_type_detailed) %>%
  unique()


modetab <- base::rbind(
  modetab19, modetab21
)

# mode share calculation ----
mode_participation_ls <- list()

for (a_mode_type in list("Drive", "Transit", "Walk", "Bicycle", "Other")) {
  dt <- data.table(modetab)
  dt[, used_mode := fcase(
    num_trips == 0, "No travel",
    num_trips > 0 & a_mode_type %in% unique(mode_group), "Used mode",
    num_trips > 0 & !(a_mode_type %in% unique(mode_group)), "Used other modes"
  ),
  by = list(person_id, day_num, day_weight)
  ][, mode_group := NULL]
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
  ggplot(aes(x = svy_year, y = pct)) +
  geom_col() +
  facet_wrap(~mode_group, scales = "free_y") +
  theme_minimal()
