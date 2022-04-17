# packages-------------------------------------------
packages <- list("bit64", "dplyr", "ggplot2", "plotly", "srvyr", "councilR", "sysfonts", "showtext", "purrr")
invisible(lapply(packages, library, character.only = TRUE))
rm(packages)

# read data -------------------------------------------
tbi <- readRDS("data/tbi_extract.RData")
source("R/df-gallons-gas-per-person.R")
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
# some extra tidbits for plotting--------------------
plotdat <-
  gal_bins_hh_summary %>%
  mutate(gal_bins = factor(
    gal_bins,
    levels = c(
      "0\nStayed\nhome",
      "0\nRode as\npassgr.",
      "0\nUsed\nother\nmodes",
      "0\nDrove\nan EV",
      "up to\n1",
      "1-2",
      "2-3",
      "3-4",
      "4-5",
      "more than\n5"
    )
  )) %>%
  mutate(mytext1 = paste0(
    round(pct_hh),
    "%"
  )) %>%
  mutate(mytext2 = "of hhs.") %>%
  mutate(mytext_EV1 ="0.4%") %>%
  mutate(mytext_EV2 = "of hhs.") %>%
  mutate(across(c(mytext1, mytext2), ~ case_when(gal_bins == "0\nDrove\nan EV" ~ "", 
                                                 TRUE ~ .))) %>%
  mutate(across(c(mytext_EV1, mytext_EV2), ~ case_when(gal_bins == "0\nDrove\nan EV" ~ ., 
                                                       TRUE ~ "")))




library(openxlsx)

#create a named list of your dataframes. The list names will be the worksheet names.
xl_lst <- list('df1_name' = iris, 'df2_name' = mtcars)
write.xlsx(xl_lst, file = "xl_with_2_worksheets.xlsx")



# plot ---------------------------------
fac <- 1.5 # scaling factor for fonts

plot <-
  plotdat %>%
  ggplot(aes(x = gal_bins, y = n_hh, fill = gal_bins)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(
      ymin = n_hh - n_hh_se,
      ymax = n_hh + n_hh_se
    ),
    width = 0,
    color = "black"
  ) +
  facet_grid(~consumed_gas, scale = "free_x", space = "free_x") +
  scale_fill_manual(values = c(rep("#07BD98", 3), rep(councilR::colors$esBlue, 10))) +
  geom_text(
    aes(label = mytext1, y = n_hh - n_hh_se - 1e4),
    size = unit(8, units = "line"),
    # lineheight = 0.25,
    family = "Arial Narrow Italic",
    vjust = 1,
    fontface = "bold",
    color = "white"
  ) +
  geom_text(
    aes(label = mytext2, y = n_hh - n_hh_se - 1e4),
    size = unit(4, units = "line"),
    # lineheight = 0.25,
    family = "Arial Narrow Italic",
    vjust = 3.5,
    fontface = "bold",
    color = "white"
  ) +
  geom_text(
    aes(label = mytext_EV1, y = n_hh + n_hh_se + 5.5e4),
    size = unit(8, units = "line"),
    # lineheight = 0.25,
    family = "Arial Narrow Italic",
    vjust = 1,
    fontface = "bold",
    color = colors$esBlue
  ) +
  geom_text(
    aes(label = mytext_EV2, y = n_hh + n_hh_se + 4.5e4),
    size = unit(4, units = "line"),
    # lineheight = 0.25,
    family = "Arial Narrow Italic",
    vjust = 3,
    fontface = "bold",
    color = colors$esBlue
  ) +
  labs(
    title = "Most households use less than 2 gallons of gas per day",
    subtitle = "Source: 2019 Metropolitan Council Travel Behavior Inventory and EPA (fuel efficiency data).",
    x = "Gallons of fuel consumed by driving per day",
    y = "Number\nof house-\nholds",
    caption = stringr::str_wrap(
      width = 160,
      paste0(
        "EPA & TBI data were matched at the level of vehicle make, model and year. ",
        "Fuel consumption was estimated by multiplying each vehicle trip's distance by the inverse of its fuel efficiency",
        "(miles per gallon of gasoline or diesel), with city mpg applied for trips with speeds 45mph or more, and highway mpg for trips with speeds under 45mph. ",
        "Data are weighted to reflect the average weekday of a metro household. ",
        "Error bars are standard errors of the weighted total. ",
        "Some vehicles were missing EPA efficiency records (11% of drivers' trips), ",
        "for these, we used the median fuel efficiency for all vehicles in the TBI."
      )
    )
  ) +
  # scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8)) +
  scale_y_continuous(labels = scales::label_number(suffix = " K", scale = 1e-3)) +
  councilR::theme_council(
    use_showtext = T,
    use_manual_font_sizes = T,
    font_sizes = list(
      title = 22 * fac * 1.1,
      subtitle = 16 * fac,
      axis_title = 14 * fac,
      axis_text = 11 * fac,
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

plot

# save plot ----------------------
jpeg("fig/gallons-consumed-per-hh.jpeg", width = 1200, height = 800, units = "px")
plot
dev.off()

png("fig/gallons-consumed-per-hh.png", width = 1200, height = 800, units = "px")
plot
dev.off()

