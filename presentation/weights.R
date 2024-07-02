source("_load_libraries.R")
source("_load_data.R")

tbi$trip[, doy := depart_time %>%
           as.Date() %>%
           wday(label = T)]
tbi$trip[tbi$household, on="hh_id", home_county := i.home_county]

tbi$trip %>%
plot_ly() %>%
  add_boxplot(y=~home_county, x=~ trip_weight)

tbi$trip %>%
plot_ly() %>%
  add_boxplot(y=~doy, x=~ trip_weight)

# tbi$trip <-
tbi$trip[, .SD %>%
           setorder(trip_weight) %>%
           .[.N, trip_weight]
         , .(home_county, doy)]
