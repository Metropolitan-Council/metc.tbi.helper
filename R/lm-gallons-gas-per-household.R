packages <- list("bit64", "dplyr", "ggplot2", "plotly", "srvyr", "councilR", "sysfonts", "showtext", "purrr")
invisible(lapply(packages, library, character.only = TRUE))
rm(packages)

tbi <- readRDS('data/tbi_extract.RData')
source('R/df-lump-hh-incomes.R')
source('R/df-gallons-gas-per-person.R')

tencol <- c(
  '#d8af39',
  '#c2be48',
  '#accc60',
  '#95d77d',
  '#7fe09c',
  '#4bb993',
  '#249183',
  '#126a6b',
  '#14454c',
  'black'
)
# load fonts -------------------------------------------

font_add("HelveticaNeueLTStd", "HelveticaNeueLTStd-Lt.otf")
font_add(
  "HELVETICANEUELTSTD-MDCN_1",
  "HELVETICANEUELTSTD-MDCN_1.otf"
)
font_add("Arial Narrow", "ARIALN.ttf")
font_add("Arial Narrow Italic",
         regular = "ARIALN.ttf",
         italic = "ARIALNI.ttf"
)

showtext_auto()



modl_dat <- gal_per_hh_day %>%
  mutate(day_cost = gal_consumed_day * 4) %>%
  mutate(day_cost2 = gal_consumed_day * 2.42) %>%
  left_join(tbi$hh) %>%
  filter(!is.na(thriveCategory)) %>%
  filter(!thriveCategory == "Non-Council Area") %>%
  mutate(income_50k = case_when(income_broad %in% c("Under $25,000", "$25,000-$49,999") ~ "Under $50K",
                                income_broad %in% c("$50,000-$74,999", "$75,000-$99,999", 
                                                    "$100,000 or more") ~ "Over $50K")) %>%
  select(hh_id, day_num, day_cost, day_cost2, day_weight, thriveCategory, thriveCatBroader, income_broad, income_50k, num_kids, num_workers, income_midpoint) %>%
  mutate(pct_cost = (day_cost * 365)/income_midpoint,
         pct_cost2 = (day_cost2 * 365)/income_midpoint)

modl_dat %>%
  as_survey_design(w = day_weight) %>%
  summarize(mn_pct2 = 100 * survey_mean(pct_cost2, na.rm = T),
            mn_pct = 100 * survey_mean(pct_cost, na.rm = T))


weighted_lm <-
  with(modl_dat,
       lm(day_cost ~
            income_broad * thriveCatBroader + num_kids + num_workers,
          w = day_weight))


weighted_glm <-
  with(modl_dat,
       glm(pct_cost ~
             income_broad * thriveCatBroader + num_kids + num_workers,
          w = day_weight))
summary(weighted_glm)
jtools::summ(weighted_glm)

fac <- 1.5 # scaling factor for fonts

library(tidypredict)
pred_cost<-
modl_dat %>%
  select(income_broad, thriveCatBroader, num_kids, num_workers) %>%
  filter(num_kids == 2, num_workers == 2) %>%
  unique() %>%
  drop_na() %>%
  complete(income_broad, thriveCatBroader, num_kids, num_workers) %>%
  filter(!income_broad == "Prefer not to answer") %>%
  mutate(cost = predict.lm(weighted_lm, ., se.fit = T)$fit,
         cost_se = predict.lm(weighted_lm, ., se.fit = T)$se.fit)

pred_pct <-
  modl_dat %>%
  select(income_broad, thriveCatBroader, num_kids, num_workers) %>%
  filter(num_kids == 2, num_workers == 2) %>%
  unique() %>%
  drop_na() %>%
  complete(income_broad, thriveCatBroader, num_kids, num_workers) %>%
  filter(!income_broad == "Prefer not to answer") %>%
  droplevels() %>%
  mutate(pct = 100 * predict.glm(weighted_glm, ., se.fit = T, type = "response")$fit,
         pct_se = 100 * predict.glm(weighted_glm, ., se.fit = T, type = "response")$se.fit) %>%
  left_join(pred_cost) 

lm_plot <- 
pred_pct %>%
  mutate(income_broad = recode_factor(income_broad, 
                                      "Under $25,000" = "Under $25K",
                                      "$25,000-$49,999" = "$25-50K", 
                                      "$50,000-$74,999" = "$50-75K", 
                                      "$75,000-$99,999" = "$75-100K", 
                                      "$100,000 or more" = "$100K or more")) %>%
  mutate(thriveCatBroader = factor(thriveCatBroader, levels = c("Urban Center", "Urban","Suburban",
                                                             "Rural"),
                                   labels = c("Urban Ctr. households", "Urban households", "Suburban households", "Rural households"))) %>%
  ggplot(aes(x = income_broad, y = pct, fill = thriveCatBroader)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = pct - pct_se, ymax = pct + pct_se), width = 0) + 
  facet_grid(~thriveCatBroader) + 
  geom_text(
    aes(label = scales::dollar(cost), y = pct - pct_se - 0.5),
    size = unit(6, units = "line"),
    # lineheight = 0.25,
    family = "Arial Narrow Italic",
    vjust = 1,
    fontface = "bold",
    color = "white"
  ) +
  geom_text(
    aes(label = "per day", y = pct - pct_se - 0.5),
    size = unit(4, units = "line"),
    # lineheight = 0.25,
    family = "Arial Narrow Italic",
    vjust = 3.5,
    fontface = "bold",
    color = "white"
  ) +
  scale_fill_manual(values = c(colors$esBlue, "#B96EC2", "#003E78")) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) + 
  labs(title = "Suburban, low-income households spend most on gas", 
       subtitle = str_wrap("Estimated percent of household income spent on gas annually at $4 per gallon, for households with two children and two workers. Fuel use estimated from 2019 Travel Behavior Data.", width = 100),
       y = "Percent\nof income\nspent on\ngas",
       x = "Annual Household Income",
       caption = stringr::str_wrap(
         width = 160,
         paste0(
           "\n\nSource: 2019 Metropolitan Council Travel Behavior Inventory (travel and vehicle data) and EPA (fuel efficiency data). ",
           "EPA & TBI data were matched at the level of vehicle make, model and year. ",
           "Fuel consumption was estimated by multiplying each vehicle trip's distance by the inverse of its fuel efficiency",
           " (miles per gallon of gasoline or diesel), with city mpg applied for trips with speeds 45mph or more, and highway mpg for trips with speeds under 45mph. ",
           "To estimate a percent of income spent on gas, we multiplied the daily total gas consumed by $4 per gallon and 365 days, then divided by the numeric midpoint of a the survey responents' detailed income category (approx. $15K bins). ",
           "Estimates shown are from a linear regression that accounts for income, geography, and family size. Survey data were weighted to reflect the average weekday of a metro resident. ",
           "Error bars are standard errors of the weighted estimate from the regression model. ",
           "Some vehicles were missing EPA efficiency records (11% of drivers' trips), ",
           "for these, we used the median fuel efficiency for all vehicles in the TBI."
         )
       )) +
  councilR::theme_council(
    use_showtext = T,
    use_manual_font_sizes = T,
    font_sizes = list(
      title = 22 * fac * 1.1,
      subtitle = 16 * fac,
      axis_title = 14 * fac,
      axis_text = 11 * 1.2,
      legend_title = 14 * fac,
      legend_text = 10 * fac,
      caption = 8 * fac,
      strip = 14 * fac
    )
  ) +
  theme(
    legend.position = "none",
    panel.spacing = unit(2, "lines")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  theme(
    axis.text = element_text(color = "black"),
    strip.text.x = element_text(color = "black", hjust = 0)
  ) +
  theme(
    axis.text = element_text(lineheight = 1.1),
    axis.title = element_text(lineheight = 1.1)
  ) +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 15, l = 0)),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 15, l = 0))
  ) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(axis.text.x = element_text(vjust = 0.95)) + 
  theme(plot.title.position = "plot")

lm_plot

# save plot ----------------------
jpeg("fig/percent-income-on-gas.jpeg", width = 1200, height = 800, units = "px")
lm_plot
dev.off()

png("fig/percent-income-on-gas.png", width = 1200, height = 800, units = "px")
lm_plot
dev.off()

