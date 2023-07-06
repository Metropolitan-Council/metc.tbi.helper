# 2019 -------------
### Fix "Change Mode" -----

linked_trips <-
  trip19 %>%
  # mutate(linked_trip_id = paste0(person_id, str_pad(linked_trip_num, width = 3, side = 'left', pad = '0') )) %>%
  select(-linked_trip_num) %>%
  group_by(linked_trip_id, person_id, hh_id) %>%
  summarize(
    # First origin
    o_purpose_category_imputed = first(o_purpose_category_imputed),
    o_purpose_imputed = first(o_purpose_imputed),

    # Last destination
    d_purpose_category = last(d_purpose_category),
    d_purpose = last(d_purpose),
    d_purpose_category_imputed = last(d_purpose_category_imputed),
    d_purpose_imputed = last(d_purpose_imputed),

    # trip weight:
    trip_purpose_weight = first(trip_weight),

    # distance (total):
    distance = sum(distance),
    distance_adj = sum(distance_adj)
  ) %>%
  ungroup()

# add linked trip id to trip table for crosstabs:
trip19 <- trip19 %>%
  mutate(linked_trip_id = paste0(person_id, "_", linked_trip_num))

# any "Change mode" destination trips remaining?
# linked_trips %>%
#   filter(trip_purpose_weight > 0) %>%
#   filter(o_purpose_category_imputed == "Change mode") %>%
#   nrow()

# get rid of change mode trips (origin)
linked_trips <- linked_trips %>% filter(!o_purpose_category_imputed %in% c("Change mode"))

# linked_trips %>%
#   filter(trip_purpose_weight > 0) %>%
#   filter(d_purpose_category_imputed == "Change mode") %>%
#   nrow()

# get rid of change mode trips (destination)
linked_trips <- linked_trips %>% filter(!d_purpose_category_imputed %in% c("Change mode"))


### Trip Purpose Table ------------
homecats <- c("Spent the night at non-home location", "Home")
nonhomecats <- c(
  "Work",
  "School",
  "Shop",
  "Social/Recreation",
  "Meal",
  "Errand/Other",
  "Work-related",
  "Escort",
  "School-related"
  # "Change mode"
)

linked_trips <- linked_trips %>%
  mutate(
    trip_type = case_when(
      o_purpose_category_imputed %in% homecats |
        d_purpose_category_imputed %in% homecats ~ "Home-based",
      TRUE ~ "Non-home-based"
    )
  )

#### Home-based trip purpose = NOT home ----------------
homebasedtrips <- linked_trips %>%
  filter(trip_type == "Home-based") %>%
  mutate(
    purpose_category = case_when(
      # when coming FROM home, the purpose is the destination
      o_purpose_category_imputed %in% homecats ~ as.character(d_purpose_category_imputed),
      # when going TO home, the purpose is the origin:
      d_purpose_category_imputed %in% homecats ~ as.character(o_purpose_category_imputed)
    ),
    purpose = case_when(
      # when coming FROM home, the purpose is the destination
      o_purpose_category_imputed %in% homecats ~ as.character(d_purpose_imputed),
      # when going TO home, the purpose is the origin:
      d_purpose_category_imputed %in% homecats ~ as.character(o_purpose_imputed)
    )
  ) %>%
  select(
    -o_purpose_category_imputed,
    -o_purpose_imputed,
    -d_purpose_category_imputed,
    -d_purpose_imputed
  )

### Trip Weight Adjustment: 50% for each half of the trip ----------------
nonhomebasedtrips_1 <-
  linked_trips %>%
  filter(trip_type == "Non-home-based") %>%
  pivot_longer(
    cols = c("o_purpose_category_imputed", "d_purpose_category_imputed"),
    values_to = "purpose_category"
  ) %>%
  select(-name) %>%
  select(purpose_category)

nonhomebasedtrips_2 <-
  linked_trips %>%
  filter(trip_type == "Non-home-based") %>%
  pivot_longer(
    cols = c("o_purpose_imputed", "d_purpose_imputed"),
    values_to = "purpose"
  ) %>%
  select(-name) %>%
  mutate(
    trip_purpose_weight = 0.5 * trip_purpose_weight,
    distance = 0.5 * distance,
    distance_adj = 0.5 * distance_adj
  ) %>%
  select(linked_trip_id, trip_type, person_id, hh_id, trip_purpose_weight, purpose, distance, distance_adj)

nonhomebasedtrips <- cbind(nonhomebasedtrips_2, nonhomebasedtrips_1)


#### Merge home-based and non-homebased trips ------------
trip_purpose19 <- bind_rows(homebasedtrips, nonhomebasedtrips) %>%
  select(-trip_type)

rm(
  homebasedtrips,
  nonhomebasedtrips_1,
  nonhomebasedtrips_2,
  homecats,
  linked_trips,
  nonhomecats
)


# 2021 -------------
### Fix "Change Mode" -----
linked_trips <-
  trip21 %>%
  # mutate(linked_trip_id = paste0(person_id, "_", linked_trip_id)) %>%
  # select(-linked_trip_num) %>%
  group_by(linked_trip_id, person_id, hh_id) %>%
  summarize(
    # First origin
    o_purpose_category = first(o_purpose_category),
    o_purpose = first(o_purpose),

    # Last destination
    d_purpose_category = last(d_purpose_category),
    d_purpose = last(d_purpose),
    # trip weight:
    trip_purpose_weight = first(trip_weight),

    # distance (total):
    distance = sum(distance),
    distance_adj = sum(distance_adj)
  ) %>%
  ungroup()

# add linked trip id to trip table for crosstabs:
# trip21 <- trip21 %>%
#   mutate(linked_trip_id = paste0(person_id, "_", linked_trip_num))

# any "Change mode" trips remaining?
# linked_trips %>%
#   filter(trip_purpose_weight > 0) %>%
#   filter(o_purpose_category == "Change mode")

# get rid of these
linked_trips <- linked_trips %>% filter(!o_purpose_category %in% c("Change mode"))

# linked_trips %>%
#   filter(trip_purpose_weight > 0) %>%
#   filter(d_purpose_category == "Change mode")

# get rid of these
linked_trips <- linked_trips %>% filter(!d_purpose_category %in% c("Change mode"))


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

linked_trips <- linked_trips %>%
  mutate(
    trip_type = case_when(
      o_purpose_category %in% homecats |
        d_purpose_category %in% homecats ~ "Home-based",
      o_purpose_category %in% nonhomecats |
        d_purpose_category %in% nonhomecats ~ "Non-home-based"
    )
  )

#### Home-based trip purpose = NOT home ----------------
homebasedtrips <- linked_trips %>%
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
  linked_trips %>%
  filter(trip_type == "Non-home-based") %>%
  pivot_longer(
    cols = c("o_purpose_category", "d_purpose_category"),
    values_to = "purpose_category"
  ) %>%
  select(-name) %>%
  select(purpose_category)

nonhomebasedtrips_2 <-
  linked_trips %>%
  filter(trip_type == "Non-home-based") %>%
  pivot_longer(
    cols = c("o_purpose", "d_purpose"),
    values_to = "purpose"
  ) %>%
  select(-name) %>%
  mutate(
    trip_purpose_weight = 0.5 * trip_purpose_weight,
    distance = 0.5 * distance,
    distance_adj = 0.5 * distance_adj
  ) %>%
  select(linked_trip_id, trip_type, person_id, hh_id, trip_purpose_weight, purpose, distance, distance_adj)

nonhomebasedtrips <- cbind(nonhomebasedtrips_2, nonhomebasedtrips_1)


#### Merge home-based and non-homebased trips ------------
trip_purpose21 <- bind_rows(homebasedtrips, nonhomebasedtrips) %>%
  select(-trip_type)

rm(
  homebasedtrips,
  nonhomebasedtrips_1,
  nonhomebasedtrips_2,
  nonhomebasedtrips,
  # nonhomebasedtrips_o,
  # nonhomebasedtrips_d,
  homecats,
  linked_trips,
  nonhomecats
)
