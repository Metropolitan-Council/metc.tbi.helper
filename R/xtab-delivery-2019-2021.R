# TEST - a sample of 3 households from each survey
# some_hhs <- bind_rows(tbi21$hh %>% sample_n(3) %>% select(hh_id),
#           tbi19$hh %>% sample_n(3) %>% select(hh_id))

# Household Day Weights -----
## 2021 -----
hh_day_weight21 <- tbi21$hh %>%
  # filter(hh_id %in% some_hhs$hh_id) %>% # TEST
  # select necessary variables; use "contains" to get all day-of-week completion columns
  select(hh_id, hh_weight, sample_segment, contains("complete_")) %>%
  # pivot the data frame to "long" format so we can ...
  pivot_longer(cols = contains("complete"),
               names_prefix = "complete_",
               names_to = "day",
               values_to = "complete") %>%
  # ... get just the completed weekdays (Mon-Thu):
  filter(day %in% c("mon", "tue", "wed", "thu")) %>%
  filter(complete == "Complete") %>%
  # get number of completed weekdays per household:
  group_by(hh_id, hh_weight, sample_segment) %>%
  summarize(num_hh_days_week = n()) %>%
  ungroup() %>%
  # hh day weight is the hh weight divided by weekdays of that hh
  mutate(hh_day_weight = hh_weight/num_hh_days_week)

## 2019 ----
# 2019 household weight is easier, because we already have a num_hh_days_week column:
hh_day_weight19 <- tbi19$hh %>%
  # filter(hh_id %in% some_hhs$hh_id) %>% # TEST
  select(hh_id, hh_weight, num_hh_days_week, sample_segment) %>%
  filter(num_hh_days_week > 0) %>%
  # hh day weight is the hh weight divided by weekdays of that hh
  mutate(hh_day_weight = hh_weight/num_hh_days_week)

## Combine 2019 & 2021 data frames ----
hh_day_weight <- bind_rows(hh_day_weight19,
                           hh_day_weight21)

# Delivery Data ----
source(
  paste0(
    "https://raw.githubusercontent.com/Metropolitan-Council/metc.tbi.helper/main/",
    "R/fun_selectall_pivot.R"
  )
)

## 2021 ----
# tbi21$day %>%
#   filter(hh_id %in% some_hhs$hh_id) %>%
#   select(contains("_num"), contains("_id"), contains("delivery"))

# so for example, hh id 21192703 should have delivery_home, even though person 1, reported it, person 2 did not

hh_deliver_dat21 <-
  selectall_pivot(tbi21, "day", "delivery") %>%
  # filter(hh_id %in% some_hhs$hh_id) %>% # TEST LINE
  # only get weekdays (day_weight > 0) %>%
  filter(day_weight > 0) %>%
  group_by(hh_id, day_num, variable) %>%
  summarize(value = max(as.numeric(value), na.rm = T, na.omit = T)) %>%
  ungroup() %>%
  mutate(value = factor(value,
                        levels = c(1, 2),
                        labels = c("Not selected", "Selected"))) %>%
  mutate(year = "2021")

## 2019 ----
hh_deliver_dat19 <-
  selectall_pivot(tbi19, "day", "delivery") %>%
  # only get weekdays (day_weight > 0) %>%
  filter(day_weight > 0) %>%
  # difference in categories in 2019:
  mutate(variable = recode(variable, "delivery_food" = "delivery_grocery_or_food")) %>%
  group_by(hh_id, day_num, variable) %>%
  summarize(value = max(as.numeric(value), na.rm = T, na.omit = T)) %>%
  ungroup() %>%
  mutate(value = factor(value,
                        levels = c(1, 2),
                        labels = c("Not selected", "Selected"))) %>%
  mutate(year = "2019")

## Combine 2019 & 2021 Data ----
deliver_dat <- bind_rows(hh_deliver_dat21,
                         hh_deliver_dat19 %>% mutate(day_num = as.factor(day_num))) %>%
  left_join(hh_day_weight)

# Survey Summary ----
deliver_prop <-
deliver_dat %>%
  filter(!is.na(hh_day_weight)) %>%
  mutate(my_strata = paste0(year, "_", sample_segment)) %>%
  # weights are hh day weight, strata are sample segment & year
  as_survey_design(w = hh_day_weight, strata = my_strata) %>%
  group_by(year, variable, value) %>%
  summarize(prop = survey_prop()) %>%
  ungroup()

# Plot ----
deliver_prop %>%
  filter(value == "Selected") %>%
  ggplot(aes(x = year, y = prop, fill = year)) +
  geom_col() +
  geom_errorbar(aes(ymin = prop - prop_se, ymax = prop + prop_se), width= 0) +
  facet_wrap(~variable, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()




