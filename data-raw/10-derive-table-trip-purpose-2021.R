
  ### Fix "Change Mode" -----
linked_trips <-
  trip21 %>%
  rename(trip_purpose_weight = trip_weight)
  # mutate(linked_trip_num = row_number())
  # mutate(linked_trip_id = paste0(person_id, "_", linked_trip_num)) %>%
  # select(-linked_trip_num) %>%
  # group_by(linked_trip_id, person_id, hh_id) %>%
  # summarize(
  #   o_purpose_category_imputed = first(o_purpose_category_imputed),
  #   o_purpose_imputed = first(o_purpose_imputed),
  #   d_purpose_category = last(d_purpose_category),
  #   d_purpose = last(d_purpose),
  #   d_purpose_category_imputed = last(d_purpose_category_imputed),
  #   d_purpose_imputed = last(d_purpose_imputed),
  #   trip_purpose_weight = last(trip_weight)
  # )

# add linked trip id to trip table for crosstabs:
# trip21 <- trip21 %>%
#   mutate(linked_trip_id = paste0(person_id, "_", linked_trip_num))

# any "Change mode" trips remaining?
# linked_trips %>%
#   filter(trip_purpose_weight > 0) %>%
#   filter(o_purpose_category_imputed == "Change mode")

# get rid of these
# linked_trips <- linked_trips %>% filter(!o_purpose_category_imputed %in% c("Change mode"))

# linked_trips %>%
#   filter(trip_purpose_weight > 0) %>%
#   filter(d_purpose_category_imputed == "Change mode")

# get rid of these
# linked_trips <- linked_trips %>% filter(!d_purpose_category_imputed %in% c("Change mode"))


### Trip Purpose Table ------------
homecats <- c("Overnight", "Home")
nonhomecats <- c(
  "Work",
  "Work related",
  "School",
  "Shopping",
  "Social/Recreation",
  "Meal",
  "Errand",
  "Other",
  "Escort",
  "School related"
  # "Change mode"
  # "Not imputable"
)

trip_type <- linked_trips %>%
  mutate(
    trip_type = case_when(
      o_purpose_category %in% homecats |
        d_purpose_category %in% homecats ~ "Home-based",
      o_purpose_category %in% nonhomecats |
        d_purpose_category %in% nonhomecats ~ "Non-home-based"
    )
  )

#### Home-based trip purpose = NOT home ----------------
homebasedtrips <- trip_type %>%
  filter(trip_type == "Home-based") %>%
  mutate(
    purpose_category = case_when(
      # when coming FROM home, the purpose is the destination
      o_purpose_category %in% homecats ~ as.character(d_purpose_category),
      # when going TO home, the purpose is the origin:
      d_purpose_category %in% homecats ~ as.character(o_purpose_category)
    ),
    purpose = case_when(
      # when coming FROM home, the purpose is the destination
      o_purpose_category %in% homecats ~ as.character(d_purpose),
      # when going TO home, the purpose is the origin:
      d_purpose_category %in% homecats ~ as.character(o_purpose)
    )
  ) %>%
  select(
    -o_purpose_category,
    -o_purpose,
    -d_purpose_category,
    -d_purpose
  )

### Trip Weight Adjustment: 50% for each half of the trip ----------------
nonhomebasedtrips_1 <-
  trip_type %>%
  filter(trip_type == "Non-home-based") %>%
  pivot_longer(
    cols = c("o_purpose_category", "d_purpose_category"),
    values_to = "purpose_category"
  ) %>%
  select(-name) %>%
  select(purpose_category)

nonhomebasedtrips_2 <-
  trip_type %>%
  filter(trip_type == "Non-home-based") %>%
  pivot_longer(
    cols = c("o_purpose", "d_purpose"),
    values_to = "purpose"
  ) %>%
  select(-name) %>%
  mutate(trip_purpose_weight = 0.5 * trip_purpose_weight) %>%
  select(trip_id, trip_type, person_id, hh_id, trip_purpose_weight, purpose)

nonhomebasedtrips <- cbind(nonhomebasedtrips_2, nonhomebasedtrips_1)


#### Merge home-based and non-homebased trips ------------
trip_purpose21 <- bind_rows(homebasedtrips, nonhomebasedtrips) %>%
  select(-trip_type)

rm(
  homebasedtrips,
  nonhomebasedtrips_o,
  nonhomebasedtrips_d,
  trip_type,
  homecats,
  linked_trips,
  nonhomecats
)
