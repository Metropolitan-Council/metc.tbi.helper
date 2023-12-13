library(data.table)
library(plotly)
library(stringr)

load("data/tbi21.rda")
tbi21$trip %>% names %>% str_subset("")
tbi21$trip[, .N, trip_weight == 0]
tbi21$day


tbi21$trip[, tbi21$trip %>% names() %>% str_detect("bicycle"), with = F]
tbi21$trip[, .N, mode_type_detailed]
tbi21$trip[mode_type_detailed %>% str_detect("Electric bicycle"), .N, mode_type_detailed]

labs <- c("< 1",
          "1 to 3",
          "3 to 5",
          "5 to 10",
          "10 to 15",
          "15 to 20",
          "> 20")

# standard bikes ---------
tbi21$trip[
   0 < distance & distance < 100 & mode_type_detailed %>% str_detect('bicy')
  , .(
    n_surveys = .N,
    est_trips = sum(trip_weight) %>% round,
    dist_min = min(distance) %>% round(2),
    dist_5th_pct = quantile(distance, 0.05) %>% round(2),
    dist_25th_pct = quantile(distance, 0.25) %>% round(2),
    dist_median = median(distance) %>% round(2),
    avg_dist = weighted.mean(distance, trip_weight) %>% round(2),
    dist_75th_pct = quantile(distance, 0.75) %>% round(2),
    dist_95th_pct = quantile(distance, 0.95) %>% round(2),
    dist_max = max(distance) %>% round(2)
  )
  , keyby = mode_type_detailed
] %>%
  print() %>%
  fwrite("output/d_bike_distances.csv")


plot_data <-
tbi21$trip[
  mode_type_detailed %>% str_detect('Standard bicycle')
  , .(trips = sum(trip_weight), .N),
  distance %>% cut(
    breaks = c(0, 1, 3, 5, 10, 15, 20, 10000),
    labels = labs) %>% factor(levels = labs, ordered = T)
  ][, pct := trips/sum(trips)] %>% print



plot_data %>%
  plot_ly() %>%
  add_bars(
    x = ~distance,
    y = ~ pct,
    text = ~ sprintf("%s%%", round(100 * pct)),
    textfont = list(size = 15),
    textposition = "auto"
  ) %>%
  layout(
    title = "Bike Trips",
    xaxis = list(
      title = "Distance (Miles)",
      tickfont = list(size = 15)
    ),
    yaxis = list(
      title = "Percent of Trips",
      tickformat = "1%",
      tickfont = list(size = 15)
    )
  ) %>%
  save_image(file = "output/bike_distance.svg", width = 600, height = 350)


# ebikes ---------
tbi21$trip[
  mode_type_detailed %>% str_detect('Electric bicycle') & distance < 100
  , weighted.mean(distance, trip_weight)
]

plot_data_2 <-
  tbi21$trip[
    mode_type_detailed %>% str_detect('Electric bicycle')
    , .(trips = sum(trip_weight), .N),
    distance %>% cut(
      breaks = c(0, 1, 3, 5, 10, 15, 20, 10000),
      labels = labs) %>% factor(levels = labs, ordered = T)
  ][, pct := trips/sum(trips)] %>% print


plot_data_2 %>%
  plot_ly() %>%
  add_bars(
    x = ~distance,
    y = ~ pct,
    text = ~ sprintf("%s%%", round(100 * pct)),
    textfont = list(size = 15),
    textposition = "auto"
  ) %>%
  layout(
    title = "eBike Trips",
    xaxis = list(
      title = "Distance (Miles)",
      tickfont = list(size = 15)
    ),
    yaxis = list(
      title = "Percent of Trips",
      tickformat = "1%",
      tickfont = list(size = 15)
    )
  ) %>%
  save_image(file = "output/ebike_distance.svg", width = 600, height = 350)
