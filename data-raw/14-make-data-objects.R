# This script is writen to run after
# 13-remove-pii.R


# 2019 -----------------------------
tbi19 <- list(
  person = person19
  , day = day19
  , household = household19
  , vehicle = vehicle19
  , dictionary = dictionary19
  , trip_purpose = trip_purpose19
  , trip = trip19
  , location = location19
)
rm(person19, household19, location19, trip19, vehicle19)

tbi19_rmPII <- list(
  person = person19_rmPII
  , day = day19
  , household = household19_rmPII
  , trip = trip19_rmPII
  , trip_purpose = trip_purpose19
  , vehicle = vehicle19_rmPII
  , dictionary = dictionary19
)
rm(person19_rmPII, day19, household19_rmPII, trip19_rmPII, trip_purpose19, vehicle19_rmPII, dictionary19)

# 2021 -----------------------------
tbi21 <- list(
  person = person21
  , day = day21
  , household = household21
  , location = location21
  , trip = trip21
  , trip_purpose = trip_purpose21
  , vehicle = vehicle21
  , dictionary = dictionary21
)
rm(person21, household21, location21, trip21, vehicle21)

tbi21_rmPII <- list(
  person = person21_rmPII
  , day = day21
  , household = household21_rmPII
  , trip = trip21_rmPII
  , trip_purpose = trip_purpose21
  , vehicle = vehicle21_rmPII
  , dictionary = dictionary21
)
rm(person21_rmPII, day21, household21_rmPII, trip21_rmPII, trip_purpose21, vehicle21_rmPII, dictionary21)




