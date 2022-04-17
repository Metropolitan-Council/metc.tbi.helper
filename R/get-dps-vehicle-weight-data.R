# read connection string (git-ignored)
source("data-raw/database-connect-string.R")

## connect to database ------------
tbidb <- ROracle::dbConnect(
  dbDriver("Oracle"),
  dbname = connect_string,
  username = "mts_planning_data",
  password = keyring::key_get("mts_planning_data_pw")
)

rm(connect_string)

## Load tables ---------
dps <- ROracle::dbReadTable(tbidb, "DPS_VEHICLE_WEIGHTS")
# remove NA make, model, year
dps <-
  dps %>%
  filter(weight_unladen > 0) %>%
  filter(if_any(c(make, model, year), ~ !is.na(.x)))


# Hand-googled Weights for certain makes/models:  -------------
missingJeep <- data.frame(
  make = c("JEEP"),
  model = c("WRANGLER"),
  year = c(1980:2003),
  weight_unladen = c(2857)
)

missingSub <- data.frame(
  make = c("SUBARU"),
  model = c("OUTBACK"),
  year = c(1990:2006),
  weight_unladen = c(3430)
)

missingSilv <- data.frame(
  make = c("CHEVROLET"),
  model = c("SILVERADO"),
  year = c(1980:1998),
  weight_unladen = c(4600)
)

missingC1500 <- data.frame(
  make = c("CHEVROLET"),
  model = c("C1500"),
  year = c(1980:1998),
  weight_unladen = c(4387)
)


missingC1500 <- data.frame(
  make = c("CHEVROLET"),
  model = c("C1500"),
  year = c(1980:1998),
  weight_unladen = c(4387)
)

missingC2500 <- data.frame(
  make = c("CHEVROLET"),
  model = c("C2500"),
  year = c(1980:1998),
  weight_unladen = c(4887)
)

dps <- bind_rows(dps, missingJeep, missingSub, missingSilv, missingC1500, missingC2500)



# Some specific model/make fixes in DPS data ------------------
dps <- dps %>%
  mutate(
    model =
      case_when(
        make == "BMW" & grepl(pattern = "^[0-9]", model) ~
        paste(substr(model, start = 1, stop = 1), "SERIES"),
        TRUE ~ model
      )
  ) %>%
  mutate(model = case_when(grepl(pattern = "F150", model) & make == "FORD" ~ "F-150", TRUE ~ model)) %>%
  mutate(model = case_when(grepl(pattern = "F250", model) & make == "FORD" ~ "F-250", TRUE ~ model)) %>%
  mutate(model = case_when(grepl(pattern = "F350", model) & make == "FORD" ~ "F-350", TRUE ~ model)) %>%
  mutate(model = case_when(grepl(pattern = "F450", model) & make == "FORD" ~ "F-450", TRUE ~ model)) %>%
  mutate(
    model =
      case_when(
        make == "DODGE" &
          (
            grepl(pattern = "RAM", model) &
              combo_body_class == "Truck"
          ) ~
        "RAM PICKUP",
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "MAZDA" & grepl(pattern = "^[[:digit:]]+$", model) ~
        paste0("MAZDA", substr(model, start = 1, stop = 1)),
        # WHY DOES THIS KEEP PUTTING A SPACE BETWEEN MAZDA and THE NUMBER?!?!
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "SATURN" & grepl("^S", model) ~
        "S SERIES",
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "SATURN" & grepl("^L", model) ~
        "L SERIES",
        TRUE ~ model
      )
  )



# Tiny tweaks to TBI Vehicle Table ---------------------
new_veh <- veh %>%
  mutate(model = case_when(
    make == "Scion" ~ paste("Scion", model),
    TRUE ~ model
  )) %>%
  mutate(make = case_when(make == "Scion" ~ "Toyota", TRUE ~ make)) %>%
  mutate(
    model =
      case_when(
        make == "Mercedes-Benz" ~
        gsub(pattern = "-Class", "", model),
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "Lexus" ~
        paste(substr(model, start = 1, stop = 2)),
        TRUE ~ model
      )
  )


# Calculate Median Values for DPS data ----------------
dps_median <-
  dps %>%
  group_by(make, model, year) %>%
  summarize(
    weight_unladen = median(weight_unladen, na.rm = T),
    class_vehicle = paste0(unique(class_vehicle), collapse = "/"),
    type_vehicle = paste0(unique(type_vehicle), collapse = "/"),
    combo_body_class = paste0(unique(combo_body_class), collapse = "/")
  ) %>%
  ungroup() %>%
  group_by(make, model, year) %>%
  add_tally() %>%
  ungroup()


veh_dps <-
  new_veh %>%
  # Lightweight dataset of unique vehicles in the TBI -------
  select(make, model, year) %>%
  mutate(make_original = make, model_original = model) %>%
  mutate(make = toupper(make), model = toupper(model)) %>%
  unique() %>%
  # join by make and year -- ignore model, match across all of them
  left_join(dps_median,
    by = c("make", "year"),
    suffix = c(".tbi", ".dps")
  ) %>%
  # now find where model (from TBI) is *in* the modelf name from EPA using grepl
  rowwise() %>%
  mutate(
    exact_match = ifelse(model.tbi == model.dps, TRUE, FALSE),
    pattern_match = grepl(model.tbi, model.dps) # will be true if the characters in model.x are in model.y
  ) %>%
  # get just the matches:
  filter(pattern_match == TRUE | exact_match == TRUE) %>%
  group_by(make, model.tbi, year) %>%
  mutate(
    patternMatchMedianWt = median(weight_unladen, na.rm = T),
    patternMatchClassN = length(unique(class_vehicle)),
    patternMatchClassList = paste0(class_vehicle, collapse = ","),
    patternMatchModelList = paste0(model.dps, collapse = ","),
    n_matches = length(model.dps)
  ) %>%
  ungroup()


# choose the BEST match for the vehicle/DPS tables: -----------------
veh_dps_best <-
  veh_dps %>%
  group_by(make, model.tbi, year) %>%
  # find the best match (will sort from true to false on exact, then true to false on pattern)
  arrange(desc(exact_match), desc(pattern_match)) %>%
  slice_head(n = 1) %>%
  # if there is an exact match, prioritize that first
  mutate(
    weight_unladen = case_when(
      exact_match == TRUE ~ weight_unladen,
      # otherwise, go ahead and use the median co2 value from all the pattern matches
      pattern_match == TRUE ~ patternMatchMedianWt
    )
  ) %>%
  mutate(
    class_vehicle = case_when(
      exact_match == TRUE ~ class_vehicle,
      # otherwise, go ahead and use the median co2 value from all the pattern matches
      pattern_match == TRUE ~ patternMatchClassList
    )
  ) %>%
  mutate(dps_tbi_veh_match_notes = case_when(
    exact_match == TRUE ~ "Exact match",
    # otherwise, go ahead and use the median co2 value from all the pattern matches
    pattern_match == TRUE & n_matches == 1 ~ paste0("Used value for: ", patternMatchModelList),
    pattern_match == TRUE & n_matches > 1 ~ paste0("Used median value of these ", n_matches, " models: ", patternMatchModelList)
  )) %>%
  # get rid of anything where there is no match at all (pattern or exact)
  filter(!is.na(weight_unladen)) %>%
  select(
    make,
    model.tbi,
    year,
    weight_unladen,
    class_vehicle,
    dps_tbi_veh_match_notes
  )

veh_dps_full <-
  new_veh %>%
  # Lightweight dataset of unique vehicles in the TBI -------
  # select(make, model, year) %>%
  # unique() %>%
  mutate(make_original = make, model_original = model) %>%
  mutate(make = toupper(make), model = toupper(model)) %>%
  left_join(veh_dps_best %>%
    rename(model = model.tbi))

veh_dps_rename <-
  veh_dps_full %>%
  select(
    hh_id, vehicle_num, vehicle_name, make_original, model_original, year, fuel,
    veh_id, co2_gpm, mpg_city, mpg_highway, epa_tbi_veh_match_notes, fuel_type,
    weight_unladen, class_vehicle, dps_tbi_veh_match_notes
  ) %>%
  rename(make = make_original, model = model_original)



veh <-
  new_veh %>%
  left_join(veh_dps_rename)


# # how many missing?
# summary(veh)
# # about 1026                cars missing (9% of total)
#
#
# remainingproblems <- test %>%
#   filter(is.na(weight_unladen)) %>%
#   filter(!make == "") %>%
#   filter(!make == "Other") %>%
#   filter(!model == "Other") %>%
#   group_by(make, model)  %>%
#   tally()  %>%
#   ungroup() %>%
#   arrange(desc(n))
#
# View(remainingproblems)

rm(veh_dps, veh_dps_best, veh_dps_full, dps, new_veh, dps_median, veh_dps_rename)
rm(tbidb)
rm(missingC1500, missingC2500, missingJeep, missingSilv, missingSub)
