source("qa_qc/00_load_pkgs.R")


tbi <- qread(Sys.getenv("path_to_tbi_qs"))
tbi <-
  lapply(tbi, \(dt){
    dt[hh_id %in% tbi$hh[hh_weight > 0, hh_id]]
  })

tbi$hh %>%
  dcast(num_bicycles~survey_year)


noTravelCols <- tbi$day %>% names %>% str_subset("no_travel_\\d+")
tbi$day[
  , c('survey_year', 'person_id', 'day_id', 'day_weight', noTravelCols)
  , with = F
] %>%
  # Filter out all the days that are missing responses (i.e., they did travel)
  .[tbi$day[
    , apply(.SD, 1, \(x) !all(x %in% c("Missing", NA)))
    , .SDcols = noTravelCols
  ]] %>%
  .[ !is.na(day_weight) & day_weight > 0] %>%
melt(id.vars = c('survey_year', "person_id", "day_id", "day_weight")) %>%
  .[
    , .(
      unwtd_pct = 100 * mean(value %in% c("Selected")),
      wtd_pct = 100 * sum((value %in% c("Selected")) * day_weight)/sum(day_weight)
    )
    , variable
  ] %>%
  .[variable %>% as.character() %>% order()]


