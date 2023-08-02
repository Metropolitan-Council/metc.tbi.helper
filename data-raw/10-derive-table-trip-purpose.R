# This script is writen to run after
# 09-derive-var-trip-distance-adjusted.R

# 2019 -------------
# add linked trip id to trip table for crosstabs:
trip19[, linked_trip_id := paste0(person_id, "_", linked_trip_num)]

linked_trips <-
  trip19[
    , .(
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
    )
    , keyby = .(linked_trip_id, person_id, hh_id)
  ]

# get rid of change mode trips (origin)
linked_trips <- linked_trips[!o_purpose_category_imputed %in% "Change mode"]

# get rid of change mode trips (destination)
linked_trips <- linked_trips[!d_purpose_category_imputed %in% "Change mode"]


### Trip Purpose Table ------------
homecats <- c("Spent the night at non-home location", "Home")
linked_trips[
  , trip_type := case_when(
    o_purpose_category_imputed %in% homecats |
      d_purpose_category_imputed %in% homecats ~ "Home-based",
    .default = "Non-home-based"
  )
]

#### Home-based trip purpose = NOT home ----------------
homebasedtrips <-
  linked_trips[
    trip_type == "Home-based"
  ][
  ,  `:=`(
    purpose_category = case_when(
      # when coming FROM home, the purpose is the destination
      o_purpose_category_imputed %in% homecats ~ d_purpose_category_imputed,
      # when going TO home, the purpose is the origin:
      d_purpose_category_imputed %in% homecats ~ o_purpose_category_imputed
    )
    , purpose = case_when(
      # when coming FROM home, the purpose is the destination
      o_purpose_category_imputed %in% homecats ~ d_purpose_imputed,
      # when going TO home, the purpose is the origin:
      d_purpose_category_imputed %in% homecats ~ o_purpose_imputed
    )
  )
][
  , c('o_purpose_category_imputed',
      'o_purpose_imputed',
      'd_purpose_category_imputed',
      'd_purpose_imputed',
      'd_purpose_category',
      'd_purpose'
      ) := NULL
]

### Trip Weight Adjustment: 50% for each half of the trip ----------------
nonhomebasedtrips_1 <-
  linked_trips[
    trip_type == "Non-home-based"
    , melt(
      .SD
      , measure.vars = c('o_purpose_category_imputed', 'd_purpose_category_imputed')
      , value.name = 'purpose_category'
    )
  ][
    , .(purpose_category)
  ]

nonhomebasedtrips_2 <-
  linked_trips[
    trip_type == "Non-home-based"
    , melt(
      .SD
      , measure.vars = c("o_purpose_imputed", "d_purpose_imputed")
      , value.name = 'purpose'
    )
  ][
    , `:=`(
      trip_purpose_weight = 0.5 * trip_purpose_weight,
      distance = 0.5 * distance,
      distance_adj = 0.5 * distance_adj
    )
  ][
    , .(linked_trip_id, person_id, hh_id,
        trip_type, trip_purpose_weight, purpose,
        distance, distance_adj)
  ]

nonhomebasedtrips <- cbind(nonhomebasedtrips_2, nonhomebasedtrips_1)


#### Merge home-based and non-homebased trips ------------
trip_purpose19 <-
  rbind(
    homebasedtrips
    , nonhomebasedtrips
  )

rm(
  homebasedtrips,
  nonhomebasedtrips,
  nonhomebasedtrips_1,
  nonhomebasedtrips_2,
  homecats,
  linked_trips
)


# 2021 -------------
linked_trips <-
  trip21[
    , .(
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
    )
    , keyby = .(linked_trip_id, person_id, hh_id)
  ]

# get rid of these
linked_trips <- linked_trips[!o_purpose_category %in% "Change mode"]

# get rid of these
linked_trips <- linked_trips[!d_purpose_category %in% "Change mode"]


### Trip Purpose Table ------------
homecats <- c("Overnight", "Home")

linked_trips <-
  linked_trips[
    ,trip_type := case_when(
      o_purpose_category %in% homecats |
        d_purpose_category %in% homecats ~ "Home-based",
      .default = 'Non-home-based'
    )
  ]

#### Home-based trip purpose = NOT home ----------------
homebasedtrips <-
  linked_trips[
    trip_type == "Home-based"
  ][
    , `:=`(
      purpose_category = case_when(
        # when coming FROM home, the purpose is the destination
        o_purpose_category %in% homecats ~ d_purpose_category,
        # when going TO home, the purpose is the origin:
        d_purpose_category %in% homecats ~ o_purpose_category
      ),
      purpose = case_when(
        # when coming FROM home, the purpose is the destination
        o_purpose_category %in% homecats ~ d_purpose,
        # when going TO home, the purpose is the origin:
        d_purpose_category %in% homecats ~ o_purpose
      )
    )
  ][
    , c(
      'o_purpose_category',
      'o_purpose',
      'd_purpose_category',
      'd_purpose'
    ) := NULL
  ]


### Trip Weight Adjustment: 50% for each half of the trip ----------------
nonhomebasedtrips_1 <-
  linked_trips[
    trip_type == "Non-home-based"
    , melt(
      .SD
      , measure.vars = c('o_purpose_category', 'd_purpose_category')
      , value.name = 'purpose_category'
    )
  ][
    , .(purpose_category)
  ]

nonhomebasedtrips_2 <-
  linked_trips[
    trip_type == "Non-home-based"
    , melt(
      .SD
      , measure.vars = c("o_purpose", "d_purpose")
      , value.name = 'purpose'
    )
  ][
    , `:=`(
      trip_purpose_weight = 0.5 * trip_purpose_weight,
      distance = 0.5 * distance,
      distance_adj = 0.5 * distance_adj
    )
  ][
    , .(linked_trip_id, trip_type, person_id,
        hh_id, trip_purpose_weight, purpose,
        distance, distance_adj)
  ]

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
  linked_trips
)
