source("R/_load_libraries_data.R")

# how many barriers do they have, what are they? s
tbi21$transpo_barriers <- tbi21$per[
  , melt(.SD,
    id.vars = c("person_id", "person_weight"),
    measure.vars = grep("transportation_barriers", names(tbi21$per)),
    variable.name = "name",
    value.name = "barrier_type"
  )
][
  !barrier_type %ilike% "Not selected" & !is.na(barrier_type)
][
  barrier_type %ilike% "this has not happened",
  barrier_type := "None"
][
  , name := gsub("transportation_barriers_", "", name)
]

message("New table added: transportation barriers faced in last 7 days by person in 2021 survey, transpo_barriers")


tbi21$transpo_barriers[
  , .(
    transpo_barriers = barrier_type |>
      unique() |>
      sort() |>
      paste0(collapse = ","),
    n_barriers = uniqueN(barrier_type)
  ),
  keyby = .(person_id)
][
  transpo_barriers %chin% c("None", "Prefer not to answer") |
    is.na(transpo_barriers),
  n_barriers := n_barriers - 1
][
  n_barriers > 1,
  transpo_barriers := "Multiple"
][
  tbi21$per,
  on = "person_id"
]

message("New variable added: transportation barriers faced in last 7 days by
        person in 2021 survey, transpo_barriers")



# transpo_barriers <-
#   tbi21$per %>%
#   select(person_id, person_weight, starts_with("transportation_barriers")) %>%
#   pivot_longer(
#     cols = starts_with("transportation_barriers"), names_prefix = "transportation_barriers_",
#     values_to = "barrier_type"
#   ) %>%
#   filter(!barrier_type == "Not selected") %>%
#   filter(!is.na(barrier_type)) %>%
#   select(-name) %>%
#   mutate(barrier_type = recode_factor(barrier_type,
#     "This has not happened in the past 7 days" = "None"
#   ))
#
# message("New table added: transportation barriers faced in last 7 days by person in 2021 survey, transpo_barriers21")
#
# # Transportation barriers column for person table
# transpo_barriers_col <-
#   transpo_barriers %>%
#   group_by(person_id) %>%
#   mutate(n_barriers = length(barrier_type[!barrier_type == "None"])) %>%
#   mutate(transpo_barriers = ifelse(n_barriers > 1, "Multiple", as.character(barrier_type))) %>%
#   select(person_id, person_weight, transpo_barriers) %>%
#   unique()
#
# per21 <- per21 %>%
#   left_join(transpo_barriers_col)
#
# message("New variable added: transportation barriers faced in last 7 days by perso in 2021 survey, transpo_barriers")
