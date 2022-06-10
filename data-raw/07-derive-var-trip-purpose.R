### Fix "Change Mode" -----
linked_trips <-
  trip %>%
  mutate(linked_trip_id = paste0(person_id, "_", linked_trip_num)) %>%
  select(-linked_trip_num) %>%
  group_by(linked_trip_id, person_id, hh_id) %>%
  summarize(
    o_purpose_category_imputed = first(o_purpose_category_imputed),
    o_purpose_imputed = first(o_purpose_imputed),
    d_purpose_category = first(d_purpose_category),
    d_purpose = first(d_purpose),
    d_purpose_category_imputed = first(d_purpose_category_imputed),
    d_purpose_imputed = first(d_purpose_imputed),
    trip_purpose_weight = first(trip_weight))

# add linked trip id to trip table for crosstabs:
trip <- trip %>%
  mutate(linked_trip_id = paste0(person_id, "_", linked_trip_num))

# any "Change mode" trips remaining?
# linked_trips %>%
#   filter(trip_purpose_weight > 0) %>%
#   filter(o_purpose_category_imputed == "Change mode")

# get rid of these
linked_trips <- linked_trips %>% filter(!o_purpose_category_imputed %in% c("Change mode"))

# linked_trips %>%
#   filter(trip_purpose_weight > 0) %>%
#   filter(d_purpose_category_imputed == "Change mode")

# get rid of these
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

trip_type <- linked_trips %>%
  mutate(
    trip_type = case_when(
      o_purpose_category_imputed %in% homecats |
        d_purpose_category_imputed %in% homecats ~ "home-based",
      TRUE ~ "non-home-based"
    )
  )

#### Home-based trip purpose = NOT home ----------------
homebasedtrips <- trip_type %>%
  filter(trip_type == "home-based") %>%
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
  ) %>%
  mutate(trip_type = "Home-based")

### Trip Weight Adjustment: 50% for each half of the trip ----------------
nonhomebasedtrips_o <-
  trip_type %>%
  filter(trip_type == "non-home-based") %>%
  pivot_longer(
    cols = c("o_purpose_category_imputed", "d_purpose_category_imputed"),
    values_to = "purpose_category"
  ) %>%
  select(-name) %>%
  mutate(trip_purpose_weight = 0.5 * trip_purpose_weight) %>%
  mutate(trip_type = "Non-Home-based")

nonhomebasedtrips_d <-
  trip_type %>%
  filter(trip_type == "non-home-based") %>%
  pivot_longer(cols = c("o_purpose_imputed", "d_purpose_imputed"),
               values_to = "purpose") %>%
  select(-name) %>%
  mutate(trip_purpose_weight = 0.5 * trip_purpose_weight) %>%
  mutate(trip_type = "Non-Home-based")



#### Merge home-based and non-homebased trips ------------
trip_purpose <- bind_rows(homebasedtrips, nonhomebasedtrips_o, nonhomebasedtrips_d) %>%
  select(-trip_type) %>%
  select(-d_purpose_category_imputed,
         -d_purpose_imputed,
         -d_purpose_category,
         -d_purpose,
         -o_purpose_category_imputed,
         -o_purpose_imputed)

rm(homebasedtrips,
   nonhomebasedtrips_o,
   nonhomebasedtrips_d,
   trip_type,
   homecats,
   nonhomecats)
