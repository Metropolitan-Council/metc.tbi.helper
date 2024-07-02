source("_load_libraries.R")
source("_load_data.R")

# 2019 --------------------
trip19 <- tbi$TBI2019_TRIP[survey_year == 2019]
trip19[, linked_trip_id := paste0(person_id, "_", linked_trip_num)]
setkey(trip19, linked_trip_id, leg_num)

linked_trips19 <-
  trip19[
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
      distance_miles = sum(distance_miles)
    ),
    keyby = .(linked_trip_id, person_id, hh_id)
  ]

# get rid of change mode trips (origin)
linked_trips19 <- linked_trips19[!o_purpose_category %in% "Change mode"]

# get rid of change mode trips (destination)
linked_trips19 <- linked_trips19[!d_purpose_category %in% "Change mode"]

homecats <- c("Overnight", "Home")
linked_trips19[
  , trip_type := case_when(
    o_purpose_category %in% homecats |
      d_purpose_category %in% homecats ~ "Home-based",
    .default = "Non-home-based"
  )
]

#### Home-based trip purpose = NOT home ----------------
homebasedtrips19 <-
  linked_trips19[
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
      "o_purpose_category",
      "d_purpose_category",
      "d_purpose",
      "o_purpose"
    ) := NULL
  ]

### Trip Weight Adjustment: 50% for each half of the trip ----------------
nonhomebasedtrips19_1 <-
  linked_trips19[
    trip_type == "Non-home-based",
    melt(
      .SD,
      measure.vars = c("o_purpose_category", "d_purpose_category"),
      value.name = "purpose_category"
    )
  ][
    , .(purpose_category)
  ]

nonhomebasedtrips19_2 <-
  linked_trips19[
    trip_type == "Non-home-based",
    melt(
      .SD,
      measure.vars = c("o_purpose", "d_purpose"),
      value.name = "purpose"
    )
  ][
    , `:=`(
      trip_purpose_weight = 0.5 * trip_purpose_weight,
      distance_miles = 0.5 * distance_miles
    )
  ][
    , .(
      linked_trip_id, person_id, hh_id,
      trip_type, trip_purpose_weight, purpose,
      distance_miles
    )
  ]

nonhomebasedtrips19 <- cbind(nonhomebasedtrips19_2, nonhomebasedtrips19_1)

#### Merge home-based and non-homebased trips ------------
trip_purpose19 <-
  rbind(homebasedtrips19,nonhomebasedtrips19) %>%
  .[, trip_type := NULL] %>%
  .[, survey_year := "2019"]

rm(
  homebasedtrips19,
  nonhomebasedtrips19,
  nonhomebasedtrips19_1,
  nonhomebasedtrips19_2,
  homecats,
  linked_trips19
)

# 2021 -------------
trip21 <- tbi$TBI2021_TRIP[survey_year == 2021]
setkey(trip21, linked_trip_id, leg_num)

linked_trips21 <-
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
      distance_miles = sum(distance_miles)
    ),
    keyby = .(linked_trip_id, person_id, hh_id)
  ]

# get rid of these
linked_trips21 <- linked_trips21[!o_purpose_category %in% "Change mode"]

# get rid of these
linked_trips21 <- linked_trips21[!d_purpose_category %in% "Change mode"]


### Trip Purpose Table ------------
homecats <- c("Overnight", "Home")
linked_trips21[
  , trip_type := case_when(
    o_purpose_category %in% homecats |
      d_purpose_category %in% homecats ~ "Home-based",
    .default = "Non-home-based"
  )
]

#### Home-based trip purpose = NOT home ----------------
homebasedtrips21 <-
  linked_trips21[
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
      "o_purpose_category",
      "o_purpose",
      "d_purpose_category",
      "d_purpose"
    ) := NULL
  ]


### Trip Weight Adjustment: 50% for each half of the trip ----------------
nonhomebasedtrips21_1 <-
  linked_trips21[
    trip_type == "Non-home-based",
    melt(
      .SD,
      measure.vars = c("o_purpose_category", "d_purpose_category"),
      value.name = "purpose_category"
    )
  ][
    , .(purpose_category)
  ]

nonhomebasedtrips21_2 <-
  linked_trips21[
    trip_type == "Non-home-based",
    melt(
      .SD,
      measure.vars = c("o_purpose", "d_purpose"),
      value.name = "purpose"
    )
  ][
    , `:=`(
      trip_purpose_weight = 0.5 * trip_purpose_weight,
      distance_miles = 0.5 * distance_miles
    )
  ][
    , .(
      linked_trip_id, trip_type, person_id,
      hh_id, trip_purpose_weight, purpose,
      distance_miles
    )
  ]

nonhomebasedtrips21 <- cbind(nonhomebasedtrips21_1, nonhomebasedtrips21_2)


#### Merge home-based and non-homebased trips ------------
trip_purpose21 <-
  rbind(homebasedtrips21, nonhomebasedtrips21, fill = F) %>%
  .[, trip_type := NULL] %>%
  .[, survey_year := "2021"]


rm(
  homebasedtrips21,
  nonhomebasedtrips21_1,
  nonhomebasedtrips21_2,
  nonhomebasedtrips21,
  homecats,
  linked_trips21,
  trip19,
  trip21
)

trip_purpose <- rbind(trip_purpose19,trip_purpose21)

rm(trip_purpose19, trip_purpose21)


