library(dplyr)
library(stringr)

# Read from online -----------
# https://www.fueleconomy.gov/feg/download.shtml
epa_raw <-
  read.csv("https://www.fueleconomy.gov/feg/epadata/vehicles.csv")
# see README for documentation/metadata

# Trim Columns ----------------
epa <- epa_raw %>%
  select(
    make,
    model,
    year,
    fuelType,
    # "Electricity" for electric cars
    co2,
    co2A,
    co2TailpipeAGpm,
    co2TailpipeGpm,
    city08U,
    # city MPG for fuelType1 (2), (11)
    city08,
    # unrounded city MPG for fuelType1 (2), (3)
    cityA08U,
    # city MPG for fuelType2 (2)
    cityA08,
    # unrounded city MPG for fuelType2 (2), (3)
    # same for highways:
    highway08U,
    highway08,
    highwayA08U,
    highwayA08
  )

# Fix up model/makes to ease matching to vehicle dataset ------------------
epa <- epa %>%
  mutate(
    model =
      case_when(
        make == "BMW" & grepl(pattern = "^[[:digit:]]", model) ~
        paste(substr(model, start = 1, stop = 1), "Series"),
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "BMW" & grepl(pattern = "^[A-Z|a-z]", model) ~
        paste(substr(model, start = 1, stop = 2)),
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "Mazda" & grepl(pattern = "^[[:digit:]] ", model) ~
        paste0("Mazda", substr(model, start = 1, stop = 1)),
        # WHY DOES THIS KEEP PUTTING A SPACE BETWEEN MAZDA and THE NUMBER?!?!
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "Mazda" & grepl(pattern = "^[[:digit:]]+$", model) ~
        paste0("Mazda", substr(model, start = 1, stop = 1)),
        # WHY DOES THIS KEEP PUTTING A SPACE BETWEEN MAZDA and THE NUMBER?!?!
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "Dodge" &
          (
            grepl(pattern = "Ram", model) &
              grepl(pattern = "Pickup", model)
          ) ~
        "Ram Pickup",
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "Saturn" & grepl(pattern = "Vue", model) ~
        "VUE",
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "Saturn" & grepl(pattern = "SKY", model) ~
        "Sky",
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "Saturn" & grepl(pattern = "^L", model) ~
        "L Series",
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "Saturn" & grepl(pattern = "^S", model) & (!model == "Sky") ~
        "S Series",
        TRUE ~ model
      )
  ) %>%
  mutate(
    model =
      case_when(
        make == "Nissan" & grepl(pattern = "Leaf", model) ~
        toupper(model),
        TRUE ~ model
      )
  ) %>%
  mutate(model = str_replace(model, pattern = "Lacrosse/Allure", "LaCrosse")) %>%
  mutate(model = str_replace(model, pattern = "Town and Country", "Town & Country")) %>%
  mutate(model = case_when(grepl(pattern = "F150", model) & make == "Ford" ~ "F-150", TRUE ~ model)) %>%
  mutate(model = case_when(grepl(pattern = "F250", model) & make == "Ford" ~ "F-250", TRUE ~ model)) %>%
  mutate(model = case_when(grepl(pattern = "F350", model) & make == "Ford" ~ "F-350", TRUE ~ model)) %>%
  mutate(model = case_when(grepl(pattern = "F450", model) & make == "Ford" ~ "F-450", TRUE ~ model))



# Set numeric columns as such, re-code character NULLS to NA:
my_num_columns <- c(
  "co2",
  "co2A",
  "co2TailpipeAGpm",
  "co2TailpipeGpm",
  "city08U",
  "city08",
  "cityA08U",
  "cityA08",
  "highway08U",
  "highway08",
  "highwayA08U",
  "highwayA08"
)

epa <-
  epa %>%
  # make -1 and NULL into proper NAs
  mutate(across(
    !!(my_num_columns),
    ~ na_if(., "-1")
  )) %>%
  # make -1 and NULL into proper NAs
  mutate(across(
    !!(my_num_columns),
    ~ na_if(., "NULL")
  )) %>%
  # make the column numeric
  mutate(across(
    !!(my_num_columns),
    ~ as.numeric(.)
  )) %>%
  # make zeros NA
  mutate(across(
    !!(my_num_columns),
    ~ na_if(., 0)
  ))

# Average by make, model, year - multiple styles/engine types for the same car: -------------
epa <- epa %>%
  group_by(make, model, year) %>%
  summarise(across(
    !!(my_num_columns),
    ~ median(., na.rm = T)
  ),
  fuelType = first(fuelType)
  ) %>% # take the median emissions value for the Make/Model/year
  ungroup()

epa <- epa %>%
  mutate(
    co2_gpm = coalesce(co2, co2TailpipeGpm, co2A, co2TailpipeAGpm),
    mpg_city = coalesce(city08U, city08, cityA08U, cityA08),
    mpg_highway = coalesce(highway08U, highway08, highwayA08U, highwayA08),
    fuel_type = fuelType
  ) %>%
  select(make, model, year, co2_gpm, mpg_city, mpg_highway, fuel_type)

# Fix up for electric vehicles
epa <- epa %>%
  mutate(co2_gpm = ifelse(fuel_type == "Electricity", 0, co2_gpm)) %>%
  mutate(mpg_city = ifelse(fuel_type == "Electricity", NA, mpg_city)) %>%
  mutate(mpg_highway = ifelse(fuel_type == "Electricity", NA, mpg_highway))



# Matching to Vehicle table ------------
veh_epa <-
  veh %>%
  mutate(
    model =
      case_when(
        make == "BMW" & grepl(pattern = "^[[:digit:]] series", model) ~
        str_to_title(model),
        TRUE ~ model
      )
  ) %>%
  # Lightweight dataset of unique vehicles in the TBI -------
  select(make, model, year) %>%
  unique() %>%
  # join by make and year -- ignore model, match across all of them
  left_join(epa,
    by = c("make", "year"),
    suffix = c(".tbi", ".epa")
  ) %>%
  # now find where model (from TBI) is *in* the modelf name from EPA using grepl
  rowwise() %>%
  mutate(
    exact_match = ifelse(model.tbi == model.epa, TRUE, FALSE),
    pattern_match = grepl(model.tbi, model.epa) # will be true if the characters in model.x are in model.y
  ) %>%
  # get just the matches:
  filter(pattern_match == TRUE | exact_match == TRUE) %>%
  group_by(make, model.tbi, year) %>%
  mutate(
    patternMatchMedianCo2 = median(co2_gpm, na.rm = T),
    patternMatchMedianCity = median(mpg_city, na.rm = T),
    patternMatchMedianHighway = median(mpg_highway, na.rm = T),
    patternMatchModelList = paste0(model.epa, collapse = ","),
    n_matches = length(model.epa)
  ) %>%
  ungroup()


# choose the BEST match for the vehicle/EPA tables:
veh_epa <-
  veh_epa %>%
  group_by(make, model.tbi, year) %>%
  # find the best match (will sort from true to false on exact, then true to false on pattern)
  arrange(desc(exact_match), desc(pattern_match)) %>%
  slice_head(n = 1) %>%
  # if there is an exact match, prioritize that first
  mutate(
    co2_gpm = case_when(
      exact_match == TRUE ~ co2_gpm,
      # otherwise, go ahead and use the median co2 value from all the pattern matches
      pattern_match == TRUE ~ patternMatchMedianCo2
    ),
    mpg_city = case_when(
      exact_match == TRUE ~ mpg_city,
      # otherwise, go ahead and use the median co2 value from all the pattern matches
      pattern_match == TRUE ~ patternMatchMedianCity
    ),
    mpg_highway = case_when(
      exact_match == TRUE ~ mpg_highway,
      # otherwise, go ahead and use the median co2 value from all the pattern matches
      pattern_match == TRUE ~ patternMatchMedianHighway
    )
  ) %>%
  mutate(epa_tbi_veh_match_notes = case_when(
    exact_match == TRUE ~ "Exact match",
    # otherwise, go ahead and use the median co2 value from all the pattern matches
    pattern_match == TRUE & n_matches == 1 ~ paste0("Used value for: ", patternMatchModelList),
    pattern_match == TRUE & n_matches > 1 ~ paste0("Used median value of these ", n_matches, " models: ", patternMatchModelList)
  )) %>%
  # get rid of anything where there is no match at all (pattern or exact)
  filter(!is.na(co2_gpm)) %>%
  select(
    make,
    model.tbi,
    year,
    co2_gpm,
    mpg_city,
    mpg_highway,
    epa_tbi_veh_match_notes,
    fuel_type
  )

veh <-
  veh %>%
  mutate(
    model =
      case_when(
        make == "BMW" & grepl(pattern = "^[[:digit:]] series", model) ~
        str_to_title(model),
        TRUE ~ model
      )
  ) %>%
  left_join(veh_epa %>%
    rename(model = model.tbi))

# # how many missing?
# summary(veh)
# # about 973 cars missing (4% of total)
#
#
# remainingproblems <- veh %>%
#   filter(is.na(co2_gpm)) %>%
#   filter(!make == "") %>%
#   filter(!model == "Other") %>%
#   group_by(make, model)  %>%
#   tally()  %>%
#   ungroup() %>%
#   arrange(desc(n))
#
# View(remainingproblems)


rm(epa_raw, epa, veh_epa, my_num_columns)
