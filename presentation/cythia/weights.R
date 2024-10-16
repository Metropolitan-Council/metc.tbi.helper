source("presentation/_load_libraries.R")
source("presentation/_load_data.R")

trip[, doy := depart_time %>%
           as.Date() %>%
           wday(label = T)]
trip[household, on="hh_id", home_county := i.home_county]

trip %>%
plot_ly() %>%
  add_boxplot(y = ~home_county, x = ~trip_weight)

trip %>%
plot_ly() %>%
  add_boxplot(y = ~doy, x = ~trip_weight)

# trip <-
trip[, .SD %>%
           setorder(trip_weight) %>%
           .[.N, trip_weight]
         , .(home_county, doy)]
