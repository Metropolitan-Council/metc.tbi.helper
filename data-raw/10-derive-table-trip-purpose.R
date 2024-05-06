# missing linked trip ids
tbi$trip[is.na(linked_trip_id), linked_trip_id := paste0(person_id, "_", linked_trip_num)]

setkey(tbi$trip, survey_year, person_id, trip_num)
linked_trips <-
  tbi$trip[
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
    keyby = .(survey_year, hh_id, person_id, linked_trip_id)
]
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
    trip_type == "Home-based",
    `:=`(
      purpose_category =
      fcase(
        o_purpose_category == "Home", d_purpose_category %>% as.vector(),
        d_purpose_category == "Home", o_purpose_category %>% as.vector()
      ),
      purpose =
      fcase(
        o_purpose_category == "Home", d_purpose %>% as.vector(),
        d_purpose_category == "Home", o_purpose %>% as.vector()
      )
    )
  ]

### Trip Weight Adjustment: 50% for each half of the trip ----------------
nonhomebasedtrips_1 <-
  linked_trips %>%
  .[trip_type == "Non-home-based",
    .(linked_trip_id, o_purpose_category, d_purpose_category)] %>%
  melt(id.var = 'linked_trip_id', value.name = "purpose_category") %>%
  .[, .(linked_trip_id, purpose_category)]

nonhomebasedtrips_2 <-
  linked_trips[
    trip_type == "Non-home-based",
    .(linked_trip_id, o_purpose, d_purpose, trip_purpose_weight, distance)
  ] %>%
  melt(id.var = c('linked_trip_id', "trip_purpose_weight", "distance"),
       value.name = "purpose") %>%
  .[
    , `:=`(
      trip_purpose_weight = 0.5 * trip_purpose_weight,
      distance = 0.5 * distance
      # , distance_adj = 0.5 * distance_adj
    )
  ]
nonhomebasedtrips <- cbind(nonhomebasedtrips_2, nonhomebasedtrips_1)

#### Merge home-based and non-homebased trips ------------
trip_purpose <-
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




