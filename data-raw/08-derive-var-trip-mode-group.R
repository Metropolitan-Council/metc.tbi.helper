# Script out dated with delivery of the
# combined_codebook_metc.xlsx file.

# # 2019 ---------------------------------
# # recode mode_type_detailed
# trip19[, mode_group :=
#   mode_type_detailed %>%
#   case_match(
#     "Airplane/helicopter" ~ "Other",
#     "ATV or snowmobile" ~ "Other",
#     "Golf cart" ~ "Other",
#     "Boat/ferry/water taxi" ~ "Other",
#     "Medical transportation service (non-emergency)" ~ "Other",
#     "Other" ~ "Other",
#     "Bicycle owned by my household" ~ "Micromobility",
#     "Bike-share (electric bicycle)" ~ "Micromobility",
#     "Bike-share (regular bicycle)" ~ "Micromobility",
#     "Borrowed bicycle (e.g., from friend)" ~ "Micromobility",
#     "Moped share (e.g., Scoot)" ~ "Micromobility",
#     "Other rented bicycle" ~ "Micromobility",
#     "Other scooter or moped" ~ "Micromobility",
#     "Personal scooter or moped (not shared)" ~ "Micromobility",
#     "Scooter share: electric push scooter" ~ "Micromobility",
#     "Scooter share: non-electric push scooter" ~ "Micromobility",
#     "Skateboard/rollerblade" ~ "Micromobility",
#     "Bus Rapid Transit (BRT) (e.g., A Line, C Line, Red Line)" ~ "Transit",
#     "Express bus" ~ "Transit",
#     "Light rail (e.g., Blue Line, Green Line)" ~ "Transit",
#     "Local bus" ~ "Transit",
#     "Northstar" ~ "Transit",
#     "Car from work" ~ "Vehicle",
#     "Carshare service (e.g., HOURCAR, Car2Go, Zipcar, Maven)" ~ "Vehicle",
#     "Friend/relative/colleague's car" ~ "Vehicle",
#     "Household vehicle 1" ~ "Vehicle",
#     "Household vehicle 2" ~ "Vehicle",
#     "Household vehicle 3" ~ "Vehicle",
#     "Household vehicle 4" ~ "Vehicle",
#     "Household vehicle 5" ~ "Vehicle",
#     "Household vehicle 6" ~ "Vehicle",
#     "Household vehicle 7" ~ "Vehicle",
#     "Household vehicle 8" ~ "Vehicle",
#     "Other motorcycle" ~ "Vehicle",
#     "Other hired car service (e.g., black car, limo)" ~ "Vehicle",
#     "Other vehicle in household" ~ "Vehicle",
#     "Other vehicle" ~ "Vehicle",
#     "Peer-to-peer car rental (e.g., Turo, Getaround)" ~ "Vehicle",
#     "Regular taxi" ~ "Vehicle",
#     "Rental car" ~ "Vehicle",
#     "Carpool match (e.g., Waze Carpool)" ~ "Other Transit",
#     "Dial-A-Ride (e.g., Transit Link)" ~ "Other Transit",
#     "Employer shuttle/bus" ~ "Other Transit",
#     "Intercity bus (e.g., Greyhound, Bolt Bus)" ~ "Other Transit",
#     "Intercity rail (e.g., Amtrak)" ~ "Other Transit",
#     "Other bus" ~ "Other Transit",
#     "Other rail" ~ "Other Transit",
#     "School bus" ~ "Other Transit",
#     "Other private shuttle/bus (e.g., a hotel's, an airport's)" ~ "Other Transit",
#     "University shuttle/bus" ~ "Other Transit",
#     "Vanpool" ~ "Other Transit",
#     "Lyft Line, Uberpool, or other shared-ride" ~ "TNC",
#     "Uber, Lyft, or other smartphone-app ride service" ~ "TNC",
#     "Metro Mobility" ~ "Metro Mobility",
#     "NA" ~ NA,
#     "Walk, jog, or roll using a wheelchair" ~ "Walk",
#     .default = mode_type_detailed
#   )]
#
# # sometime mode_type_detailed is NA.
# # Impute mode_type instead
# trip19[, mode_group :=
#   fifelse(
#     is.na(mode_group),
#     as.character(mode_type),
#     mode_group
#   )]
#
# # Now the imputed values need to be aligned
# trip19[, mode_group := case_match(
#   mode_group,
#   "Household vehicle" ~ "Vehicle",
#   "Walk" ~ "Walk",
#   "School bus" ~ "Other Transit",
#   "Micromobility" ~ "Micromobility",
#   "Public bus" ~ "Transit",
#   "Other bus" ~ "Other Transit",
#   "Other bus" ~ "Other",
#   .default = mode_group
# )]
#
# # 2021 -----------------------------------------------
#
# # recode mode_type_detailed
# trip21[, mode_group :=
#   mode_type_detailed %>%
#   case_match(
#     "Airplane/helicopter" ~ "Other",
#     "ATV" ~ "Other",
#     "Boat/ferry" ~ "Other",
#     "Golf cart" ~ "Other",
#     "Medical transportation service" ~ "Other",
#     "Other boat (e.g., kayak)" ~ "Other",
#     "Other" ~ "Other",
#     "Snowmobile" ~ "Other",
#     "Vehicle ferry (took vehicle on board)" ~ "Other",
#     "Bike-share - electric bicycle" ~ "Micromobility",
#     "Bike-share - standard bicycle" ~ "Micromobility",
#     "Borrowed bicycle (e.g., a friend's)" ~ "Micromobility",
#     "Electric bicycle (my household's)" ~ "Micromobility",
#     "Moped-share (e.g., Scoot)" ~ "Micromobility",
#     "Other rented bicycle" ~ "Micromobility",
#     "Other scooter or moped" ~ "Micromobility",
#     "Personal scooter or moped (not shared)" ~ "Micromobility",
#     "Scooter-share (e.g., Bird, Lime)" ~ "Micromobility",
#     "Skateboard or rollerblade" ~ "Micromobility",
#     "Standard bicycle (my household's)" ~ "Micromobility",
#     "Bus rapid transit (e.g., A Line, C Line, Red Line)" ~ "Transit",
#     "Express/commuter bus" ~ "Transit",
#     "Light rail" ~ "Transit",
#     "Local fixed-route bus" ~ "Transit",
#     "Northstar" ~ "Transit",
#     "Car rental" ~ "Vehicle",
#     "Carshare service (e.g., Zipcar)" ~ "Vehicle",
#     "Friend's vehicle" ~ "Vehicle",
#     "Household vehicle 1" ~ "Vehicle",
#     "Household vehicle 2" ~ "Vehicle",
#     "Household vehicle 3" ~ "Vehicle",
#     "Household vehicle 4" ~ "Vehicle",
#     "Household vehicle 5" ~ "Vehicle",
#     "Household vehicle 6" ~ "Vehicle",
#     "Household vehicle 7" ~ "Vehicle",
#     "Household vehicle 8" ~ "Vehicle",
#     "Other car" ~ "Vehicle",
#     "Other hired car service (e.g., black car, limo)" ~ "Vehicle",
#     "Other motorcycle (not my household's)" ~ "Vehicle",
#     "Other motorcycle in household" ~ "Vehicle",
#     "Other vehicle in household" ~ "Vehicle",
#     "Regular taxi (e.g., Yellow Cab)" ~ "Vehicle",
#     "Work vehicle" ~ "Vehicle",
#     "Carpool match (e.g., Waze Carpool)" ~ "Other Transit",
#     "Employer-provided shuttle/bus" ~ "Other Transit",
#     "Intercity bus (e.g., BoltBus, Greyhound)" ~ "Other Transit",
#     "Intercity rail (e.g., Amtrak)" ~ "Other Transit",
#     "Other bus" ~ "Other Transit",
#     "Other private shuttle/bus (e.g., Bellair Charters, Airporter Shuttle)" ~ "Other Transit",
#     "Other rail" ~ "Other Transit",
#     "Paratransit/Dial-A-Ride" ~ "Other Transit",
#     "School bus" ~ "Other Transit",
#     "SouthWest Prime or MVTA Connect" ~ "Other Transit",
#     "University/college shuttle/bus" ~ "Other Transit",
#     "Vanpool" ~ "Other Transit",
#     "Metro Mobility" ~ "Metro Mobility",
#     "Uber, Lyft, or other smartphone-app ride service" ~ "TNC",
#     "Walk (or jog/wheelchair)" ~ "Walk",
#     "NA" ~ NA,
#     .default = mode_type_detailed
#   )]
#
# # Since mode group is based off of 2021 data,
# # there is nothing to impute
# trip21[is.na(mode_group), .N, mode_type]
