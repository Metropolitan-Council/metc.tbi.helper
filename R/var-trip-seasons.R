library(lubridate)
# Seasons -------------
getSeason <- function(input.date) {
  numeric.date <- 100 * month(input.date) + day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  # This is set to meterological seasons https://www.timeanddate.com/calendar/aboutseasons.html
  cuts <- base::cut(numeric.date, breaks = c(0, 301, 0531, 0831, 1130, 1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter", "Spring", "Summer", "Fall", "Winter")
  return(cuts)
}

tbi$day <- tbi$day %>% mutate(season = getSeason(travel_date))

tbi$trip <- tbi$trip %>%
  mutate(season = getSeason(travel_date))

rm(getSeason)
