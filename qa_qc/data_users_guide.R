source("qa_qc/00_load_pkgs.R")

tbi <- qread(Sys.getenv("path_to_tbi_qs"))
weighted_hhs <- tbi$hh[!is.na(hh_weight) & hh_weight > 0, hh_id]

tbi <-
  lapply(
    tbi[tbi %>% names %>% str_detect("meta", T)],
    function(dt) {
      dt <- dt[hh_id %in% weighted_hhs]
    }
  )


tbi$hh[
  , .N
  , income_broad
] %>%
  .[, pct := 100 * N/sum(N)] %>%
  print


tbi$hh %>%
  dcast(num_bicycles~survey_year)


travel_cols <-
  names(tbi$day) %>% str_subset('no_travel') %>% str_subset("other", T)

tbi$day %>%
  melt(id.vars = c('survey_year', 'person_id', 'day_id', 'day_weight'),
       measure.vars = travel_cols) %>%
  .[!value %in% c("Missing", NA)] %>%
  .[!is.na(day_weight) & day_weight > 0] %>%
  .[, .(wtd_num = sum(day_weight)), .(survey_year, variable, value)] %>%
  .[, wtd_pct := 100 * round(wtd_num/sum(wtd_num), 3), .(survey_year, variable)] %>%
  .[value == 'Selected'] %>%
  dcast(variable~survey_year, value.var = 'wtd_pct') %>%
  print


# working with weights ------
hh_design <-
  tbi$hh %>%
  .[!is.na(hh_weight)] %>%
  as_survey_design(
    ids = hh_id,
    weights = hh_weight,
    strata = sample_segment
  )

tbl <-
  hh_design %>%
  group_by(home_county, income_broad) %>%
  summarize(
    N = n(),
    wtd_est = survey_total(),
    wtd_prop = survey_prop(proportion = T)
  )


tbl %>%
  mutate(home_county = gsub(" County, MN| County, WI", "", home_county)) %>%
  ggplot(aes(x = income_broad, y = wtd_prop)) +
  geom_col(fill = councilR::colors$councilBlue) +
  geom_linerange(
    aes(ymin = wtd_prop - wtd_prop_se, ymax = wtd_prop + wtd_prop_se)
  ) +
  coord_flip() +
  facet_wrap(~home_county) +
  councilR::theme_council_open()

# Variables in different tables ----

tab <-
  tbi$person %>%
  merge(tbi$hh, by = c('hh_id', 'survey_year')) %>%
  .[survey_year == 2023] %>%
  .[
    , .(wtd_population = sum(person_weight))
    , .(home_county, job_type_pre_covid)
    ] %>%
  .[, wtd_prop_population := round(wtd_population / sum(wtd_population), 4) * 100, home_county]

tab[home_county %like% "Chisago"]


# Weighted mode share -------------------
trips1 <- tbi$trip %>%
  left_join(select(tbi$hh, hh_id, sample_segment), by = "hh_id") %>%
  filter(!is.na(trip_weight) & trip_weight > 0)

trip_design <- trips1 %>%
  as_survey_design(
    ids = trip_id,
    weights = trip_weight,
    strata = sample_segment
  )


tbl <- trip_design %>%
  group_by(mode_type) %>%
  summarize(
    N = n(),
    wtd_N = sum(trip_weight),
    wtd_N_alt = survey_total(),
    wtd_prop = survey_prop(vartype = "se", proportion = TRUE)
  )

tbl %>%
  select(mode_type, N, wtd_N, wtd_N_alt, wtd_prop, wtd_prop_se) %>%
  arrange(desc(wtd_prop)) %>%
  gt::gt() %>%
  gt::fmt_number(columns = c(2:3) + 1, decimals = 0) %>%
  gt::fmt_percent(columns = c(4:5)+ 1, decimals = 1) %>%
  gt::sub_small_vals(threshold = 0.001, small_pattern = "<0.1%")






