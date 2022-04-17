
### Get linked trip table --------------
# Transit trips were un-linked where the access/egress mode can be separated from the transit leg.
# We call the linked trip table because we want the beginning/end trip purposes for
# trips to/from the bus -- not "Change mode."
source("R/df-trip-linked.R")



### Trip Purpose Table ------------
#### Reassign purpose: linked trips -> home-based + non-homebased ----------
homecats <- c("Spent the night at non-home location", "Home")
nonhomecats <- c(
  "Work", "School", "Shop", "Social/Recreation", "Meal",
  "Errand/Other", "Work-related", "Escort",
  "School-related"
)

trip_linked <- tbi$trip_linked %>%
  mutate(home_based = case_when(
    o_purpose_category_imputed %in% homecats | d_purpose_category_imputed %in% homecats ~ "home-based",
    TRUE ~ "non-home-based"
  ))

#### Home-based trip purpose = NOT home ----------------
homebasedtrips <- tbi$trip_linked %>%
  filter(home_based == "home-based") %>%
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
  # filter(!purpose %in% c('Change mode', 'Home', 'Missing: Non-imputable'))%>%
  mutate(trip_type = "Home-based")

### Trip Weight Adjustment: 50% for each half of the trip ----------------
nonhomebasedtrips <-
  left_join(
    trip_linked %>%
      filter(home_based == "non-home-based") %>%
      pivot_longer(cols = c("o_purpose_category_imputed", "d_purpose_category_imputed"), values_to = "purpose_category") %>%
      select(-name),
    # Detailed Purposes
    trip_linked %>%
      filter(home_based == "non-home-based") %>%
      pivot_longer(cols = c("o_purpose_imputed", "d_purpose_imputed"), values_to = "purpose") %>%
      select(-name)
  ) %>%
  mutate(trip_weight = 0.5 * trip_weight) %>%
  # filter(!purpose %in% c('Change mode', 'Home', 'Missing: Non-imputable'))%>%
  mutate(trip_type = "Non-Home-based")

#### Merge home-based and non-homebased trips ------------
tbi$trip_purpose <- bind_rows(homebasedtrips, nonhomebasedtrips)
rm(homebasedtrips, nonhomebasedtrips, trip_linked, homecats, nonhomecats)
