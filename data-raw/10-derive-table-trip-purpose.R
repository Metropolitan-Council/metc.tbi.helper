# 2019 -------------
trip19[, linked_trip_id := paste0(person_id, "_", linked_trip_num)]
setkey(trip19, depart_time)

linked_trips <-
  trip19[
    , .(
      # First origin
      o_purpose_category = first(o_purpose_category),
      o_purpose = first(o_purpose),

      # Last destination
      d_purpose_category = last(d_purpose_category),
      d_purpose = last(d_purpose),

            # trip weight:
      trip_purpose_weight = mean(trip_weight),

      # distance (total):
      distance = sum(distance_miles)
    ),
    keyby = .(linked_trip_id, person_id, hh_id)
  ]

# get rid of change mode trips
linked_trips <- linked_trips[d_purpose_category != 'Change mode']

### Trip Purpose Table ------------
linked_trips[
  , trip_type := case_when(
    o_purpose == "Went home" | d_purpose == "Went home" ~ "Home-based",
    .default = "Non-home-based"
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
        o_purpose_category == "Home" ~ d_purpose_category,
        # when going TO home, the purpose is the origin:
        d_purpose_category == "Home" ~ o_purpose_category
      ),
      purpose = case_when(
        # when coming FROM home, the purpose is the destination
        o_purpose_category == "Home" ~ d_purpose,
        # when going TO home, the purpose is the origin:
        d_purpose_category == "Home" ~ o_purpose
      )
    )
  ]


### Trip Weight Adjustment: 50% for each half of the trip ----------------
linked_trips[trip_type == "Non-home-based"] %>%
  melt(measure.vars = c("o_purpose_category", "d_purpose_category"),
        value.name = "purpose_category")


nonhomebasedtrips_1 <-
  linked_trips[
    trip_type == "Non-home-based",
    melt(
      .SD,
      measure.vars = c("o_purpose_category", "d_purpose_category"),
      value.name = "purpose_category"
    )
  ][
    , .(purpose_category)
  ]

nonhomebasedtrips_2 <-
  linked_trips[
    trip_type == "Non-home-based",
    melt(
      .SD,
      measure.vars = c("o_purpose", "d_purpose"),
      value.name = "purpose"
    )
  ][
    , `:=`(
      trip_purpose_weight = 0.5 * trip_purpose_weight,
      distance = 0.5 * distance
      # , distance_adj = 0.5 * distance_adj
    )
  ]

nonhomebasedtrips <- cbind(nonhomebasedtrips_2, nonhomebasedtrips_1)


#### Merge home-based and non-homebased trips ------------
trip_purpose19 <-
  rbind(
    homebasedtrips,
    nonhomebasedtrips,
    fill = T
  )

rm(
  homebasedtrips,
  nonhomebasedtrips,
  nonhomebasedtrips_1,
  nonhomebasedtrips_2,
  linked_trips
)


# 2021 -------------
setkey(trip21, person_id, depart_time)
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
      distance = sum(distance_miles)
    ),
    keyby = .(linked_trip_id, person_id, hh_id)
  ]

# get rid of change mode trips
linked_trips <- linked_trips[d_purpose_category != 'Change mode']

### Trip Purpose Table ------------
linked_trips[
  , trip_type := case_when(
    o_purpose == "Went home" | d_purpose == "Went home" ~ "Home-based",
    .default = "Non-home-based"
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
        o_purpose_category == "Home" ~ d_purpose_category,
        # when going TO home, the purpose is the origin:
        d_purpose_category == "Home" ~ o_purpose_category
      ),
      purpose = case_when(
        # when coming FROM home, the purpose is the destination
        o_purpose_category == "Home" ~ d_purpose,
        # when going TO home, the purpose is the origin:
        d_purpose_category == "Home" ~ o_purpose
      )
    )
  ]


### Trip Weight Adjustment: 50% for each half of the trip ----------------
linked_trips[trip_type == "Non-home-based"] %>%
  melt(measure.vars = c("o_purpose_category", "d_purpose_category"),
       value.name = "purpose_category")


nonhomebasedtrips_1 <-
  linked_trips[
    trip_type == "Non-home-based",
    melt(
      .SD,
      measure.vars = c("o_purpose_category", "d_purpose_category"),
      value.name = "purpose_category"
    )
  ][
    , .(purpose_category)
  ]

nonhomebasedtrips_2 <-
  linked_trips[
    trip_type == "Non-home-based",
    melt(
      .SD,
      measure.vars = c("o_purpose", "d_purpose"),
      value.name = "purpose"
    )
  ][
    , `:=`(
      trip_purpose_weight = 0.5 * trip_purpose_weight,
      distance = 0.5 * distance
      # , distance_adj = 0.5 * distance_adj
    )
  ]

nonhomebasedtrips <- cbind(nonhomebasedtrips_2, nonhomebasedtrips_1)


#### Merge home-based and non-homebased trips ------------
trip_purpose21 <-
  rbind(
    homebasedtrips,
    nonhomebasedtrips,
    fill = T
  )

rm(
  homebasedtrips,
  nonhomebasedtrips,
  nonhomebasedtrips_1,
  nonhomebasedtrips_2,
  linked_trips
)
