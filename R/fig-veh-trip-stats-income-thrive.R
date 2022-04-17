# packages-------------------------------------------
packages <- list("bit64", "dplyr", "tidyr", "ggplot2", "plotly", "srvyr", "councilR", "sysfonts", "showtext")
invisible(lapply(packages, library, character.only = TRUE))
rm(packages)
  

# load fonts -------------------------------------------
tbi <- readRDS('data/tbi_extract.RData')
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

# data -------------------------------------------

source("R/df-vehicle-occupancy.R")
source("R/df-veh-trip-stats-income-thrive.R")

plot_dat <- veh_income_thrive %>% left_join(veh_occ_income_thrive) %>%
  pivot_longer(cols = -c(thriveCatBroader, income_broad)) %>%
  mutate(valtype = ifelse(grepl("_se", name), "se", "mn")) %>%
  mutate(name = gsub("_se", "", name)) %>%
  pivot_wider(names_from = valtype, values_from = value) %>%
  rename(variable = name) %>%
  mutate(income_broad = recode_factor(income_broad, 
                                      "Under $25,000" = "Under $25K",
                                      "$25,000-$49,999" = "$25-50K", 
                                      "$50,000-$74,999" = "$50-75K", 
                                      "$75,000-$99,999" = "$75-100K", 
                                      "$100,000 or more" = "$100K or more")) %>%
  mutate(thriveCatBroader = factor(thriveCatBroader, levels = c("Urban Center", "Urban","Suburban",
                                                                "Rural"),
                                   labels = c("Urban Ctr. households", "Urban households", "Suburban households", "Rural households")))


plot_dat_trip <- veh_thrive_trip_summary %>%
  pivot_longer(cols = -c(thriveCatBroad, n, n_hh)) %>%
  mutate(valtype = ifelse(grepl("_se", name), "se", "mn")) %>%
  mutate(name = gsub("_se", "", name)) %>%
  pivot_wider(names_from = valtype, values_from = value) %>%
  rename(variable = name) %>%
  mutate(thriveCatBroad = factor(thriveCatBroad, 
                                 levels = c("Urban Center", 
                                            "Urban",
                                            "Suburban",
                                            "Suburban Edge",
                                            "Emerging Suburban Edge",
                                            "Rural")))

# plot -------------------------------------------
pos <- position_dodge(width = 0.8)
library(cowplot)

fac <- 1.5

veh_wt_tally_thrive %>% 
  arrange(thriveCatBroader, desc(weighted_n_xthrive)) %>%
  group_by(thriveCatBroader) %>% 
  slice_head(n = 3) %>%
  filter(!is.na(thriveCatBroader))

p <- 
plot_dat %>%
  filter(variable == "mpg_highway") %>%
  ggplot(aes(x = income_broad, y = mn, group = thriveCatBroader, fill = thriveCatBroader)) +
  geom_bar(stat = "identity", position = pos, width = 0.8) +
  geom_errorbar(aes(ymin = mn - se, ymax = mn + se), width = 0, position = pos) + 
  facet_wrap(~ thriveCatBroader, nrow = 1) +
  coord_cartesian(ylim=c(18, 32)) + 
  geom_hline(aes(yintercept=45.9), linetype = "dashed") +
  geom_hline(aes(yintercept=29.7), linetype = "dashed") +
  geom_hline(aes(yintercept=20), linetype = "dashed") +
  theme(legend.position = "none") + 
  geom_text(
    aes(label = round(mn), y = mn - se - 0.5),
    size = unit(6, units = "line"),
    # lineheight = 0.25,
    family = "Arial Narrow Italic",
    vjust = 1,
    fontface = "bold",
    color = "white"
  ) +
  geom_text(
    aes(label = "mpg", y = mn - se - 0.5),
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
       y = "miles per\ngallon\nhighway",
       x = "Annual Household Income",
       caption = stringr::str_wrap(
         width = 160,
         paste0(
           "\n\nSource: 2019 Metropolitan Council Travel Behavior Inventory (travel and vehicle data) and EPA (fuel efficiency data). ",
           "EPA & TBI data were matched at the level of vehicle make, model and year. "
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
  theme(plot.title.position = "plot") + 
  theme(plot.margin = unit(c(0, 7, 0, 0), "lines"))

p
ggdraw(p) + draw_label("2011 Toyota Prius", x = 0.825, y = 0.88, hjust = 0, vjust = 0, 
                       fontfamily = "Arial Narrow Italic", fontface = "italic", size = 14)  +
  
  draw_label("2012 Honda CR-V", x = 0.825, y = 0.425, hjust = 0, vjust = 0, 
               fontfamily = "Arial Narrow Italic", fontface = "italic", size = 14) + 
  
  draw_label("2009 Chevy Silverado", x = 0.825, y = 0.15, hjust = 0, vjust = 0, 
             fontfamily = "Arial Narrow Italic", fontface = "italic", size = 14)  
  
# Weight x Thrive -----------
p <- 
  plot_dat_trip %>%
  filter(variable == "weight_unladen") %>%
  ggplot(aes(x = thriveCatBroad, y = mn, group = thriveCatBroad)) +
  geom_bar(stat = "identity", position = pos, width = 0.8, fill = councilR::colors$esBlue) +
  geom_errorbar(aes(ymin = mn - se, ymax = mn + se), width = 0, position = pos) + 
  # facet_wrap(~ thriveCatBroader, nrow = 1) +

  theme(legend.position = "none") + 
  geom_text(
    aes(label = round(mn), y = mn - se - 0.5),
    size = unit(6, units = "line"),
    # lineheight = 0.25,
    family = "Arial Narrow Italic",
    vjust = 1,
    fontface = "bold",
    color = "white"
  ) +
  geom_text(
    aes(label = "lbs", y = mn - se - 0.5),
    size = unit(4, units = "line"),
    # lineheight = 0.25,
    family = "Arial Narrow Italic",
    vjust = 3.5,
    fontface = "bold",
    color = "white"
  ) +
  # scale_fill_manual(values = c(colors$esBlue, "#B96EC2", "#003E78")) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) + 
  labs(title = "Rural, suburban residents drive heavier vehicles", 
       subtitle = str_wrap("Vehicle weight by owner's geography, weighted at the trip level.", width = 100),
       y = "unladen\ncurb\nweight\n(lbs)",
       x = "Community Type",
       caption = stringr::str_wrap(
         width = 160,
         paste0(
           "\n\nSource: 2019 Metropolitan Council Travel Behavior Inventory (travel and vehicle data) and EPA (fuel efficiency data). ",
           "EPA & TBI data were matched at the level of vehicle make, model and year. "
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
  theme(plot.title.position = "plot") + 
  coord_cartesian(ylim = c(2900, 5100)) + 
  geom_hline(aes(yintercept=3042), linetype = "dashed") +
  geom_hline(aes(yintercept=3417), linetype = "dashed") +
  geom_hline(aes(yintercept=4931), linetype = "dashed") + 
  theme(plot.margin = unit(c(0, 12, 0, 0), "lines"))

# p
png("fig/vehicle-weight-trip-by-thrive.png", width = 1200, height = 800, units = "px")

ggdraw(p) + draw_label("2011 Toyota Prius", x = 0.865, y = 0.22, hjust = 0, vjust = 0, 
                       fontfamily = "Arial Narrow Italic", fontface = "italic", size = 14)  +
  
  draw_label("2012 Honda CR-V", x = 0.865, y = 0.33, hjust = 0, vjust = 0, 
             fontfamily = "Arial Narrow Italic", fontface = "italic", size = 14) + 
  
  draw_label("2009 Chevy Silverado", x = 0.87, y = 0.8, hjust = 0, vjust = 0, 
             fontfamily = "Arial Narrow Italic", fontface = "italic", size = 14)  

dev.off()
