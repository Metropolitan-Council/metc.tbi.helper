## code to prepare `histogram_breaks` dataset goes here

histogram_breaks <-
  list(
    "num_trips" = list(
      breaks = c(-.01, 0, 2, 4, 6, 8, 10, 100),
      labels = c("0", "1-2", "3-4", "5-6", "7-8", "9-10", "11 or more")
    ),
    "num_adults" = list(
      breaks = c(0, 1, 2, 3, 4, 10),
      labels = c("1", "2", "3", "4", "5 or more")
    ),
    "num_kids" = list(
      breaks = c(-0.1, 0, 1, 2, 3, 4, 10),
      labels = c("0", "1", "2", "3", "4", "5 or more")
    ),
    "num_students" = list(
      breaks = c(-0.1, 0, 1, 2, 3, 4, 10),
      labels = c("0", "1", "2", "3", "4", "5 or more")
    ),
    "num_workers" = list(
      breaks = c(-0.1, 0, 1, 2, 3, 4, 10),
      labels = c("0", "1", "2", "3", "4", "5 or more")
    ),
    "co2_gpm" = list(
      breaks = c(-0.1, 0, 100, 200, 300, 400, 500, 600, 700, 1000),
      labels = c("0", "1-100", "101-200", "201-300", "301-400", "401-500", "501-600", "601-700", "more than 700")
    ),
    "mpg_city" = list(
      breaks = c(0, 15, 20, 25, 30, 35, 40, 45, 50, 100, Inf),
      labels = c("15 or less", "16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "more than 50", "Electric")
    ),
    "mpg_highway" = list(
      breaks = c(0, 15, 20, 25, 30, 35, 40, 45, 50, 100, Inf),
      labels = c("15 or less", "16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "more than 50", "Electric")
    ),
    "weight_unladen" = list(
      breaks = c(0, 2000, 3000, 4000, 5000, 6000, 7000, Inf),
      labels = c("2000 or less", "2001-3000", "3001-4000", "4001-5000", "5001-6000", "6001-7000", "more than 7000")
    ),
    "veh_age" = list(
      breaks = c(-2, 0, 2, 4, 6, 8, 10, 12, 14, 16, Inf),
      labels = c("0", "1-2", "3-4", "5-6", "7-8", "9-10", "11-12", "13-14", "15-16", "more than 16")
    ),

    # Time will be weird -- need to remember how to cut time
    "depart_time_imputed" = list(
      breaks = c(data.table::as.ITime("00:00"), as.ITime("06:00"), data.table::as.ITime("09:00"), data.table::as.ITime("12:00"), data.table::as.ITime("15:00"), data.table::as.ITime("18:00"), data.table::as.ITime("21:00"), data.table::as.ITime("23:59:59")),
      labels = c("12-6 AM", "6-9 AM", "9 AM-12 PM", "12-3 PM", "3-6 PM", "6-9 PM", "9 PM-12 AM")
    ),
    "arrive_time" = list(
      breaks = c(data.table::as.ITime("00:00"), data.table::as.ITime("06:00"), as.ITime("09:00"), data.table::as.ITime("12:00"), data.table::as.ITime("15:00"), data.table::as.ITime("18:00"), data.table::as.ITime("21:00"), data.table::as.ITime("23:59:59")),
      labels = c("12-6 AM", "6-9 AM", "9 AM-12 PM", "12-3 PM", "3-6 PM", "6-9 PM", "9 PM-12 AM")
    ),
    "duration_imputed" = list(
      breaks = c(-10, 15, 30, 45, 60, 120, Inf),
      labels = c("15 min or less", "15-30 min", "30-45 min", "45-60 min", "1-2 hours", "more than 2 hours")
    ),
    "distance" = list(
      breaks = c(-10, 1, 3, 5, 10, 20, 50, Inf),
      labels = c("1 or less", "1-3", "3-5", "5-10", "10-20", "20-50", "50 or more")
    ),
    "speed_mph_imputed" = list(
      breaks = c(-10, 5, 10, 15, 20, 40, 60, Inf),
      labels = c("5 or less", "5-10", "10-15", "15-20", "20-40", "40-60", "More than 60")
    )
  )


usethis::use_data(histogram_breaks, overwrite = TRUE)
