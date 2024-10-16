source("presentation/_load_libraries.R")
source("presentation/_load_data.R")
source("presentation/factorize_col_function.R")

value_labels <- tbi$TBI19.21.23_VALUE_LIST
variable_list <- tbi$TBI19.21.23_VARIABLE_LIST

trip[, mode_type_binned := fcase(
  mode_type %in% c("Rail", "Public Bus", "Other Bus"), 1L, # Transit
  mode_type == "Long distance passenger mode", 5L, # LD Passenger
  mode_type %in% c("Household Vehicle", "Other Vehicle", "For-Hire Vehicle", "Smartphone ridehailing service"), 3L, # Vehicle
  mode_type == "Micromobility", 4L, # Micromobility
  mode_type %in% c("Other", "School Bus"), 5L, # Other
  mode_type == "Walk", 6L, # Walk
  default = 995L
)]

trip[day, day_id := i.day_id, on = "person_id"] # 2019 trip data doesn't have day_id

variable_list <- unique(rbind(
  variable_list,
  data.table(
    variable = "mode_type_binned",
    hh = 0,
    person = 0,
    vehicle = 0,
    day = 0,
    trip = 1,
    location = 0,
    trip_purpose = 0,
    description = "Binned mode type",
    data_type = "integer/categorical",
    logic = NA,
    is_checkbox = 0,
    shared_name = "mode_type_binned"
  ),
  fill = T
))

value_labels <- rbind(
  value_labels,
  data.table(
    variable = "mode_type_binned",
    value = c(1:6, 995),
    label = c(
      "Transit", "Long Distance Passenger Mode", "Vehicle", "Bike",
      "Other", "Walk", "Missing"
    )
  ),
  fill = T
)

participation_table <- day[, .(hh_id, person_id, day_id, day_weight)]
participation_table[household, survey_year := i.survey_year, on = "hh_id"]

participation_rates <- list()

for (m in value_labels[variable == "mode_type_binned" &
                               !value %in% c(995), value]) {
  # bp_codes = broad_purposes[broad_purpose == bp, value]
  participant_day_ids <- trip[mode_type_binned == m, unique(day_id)]
  participation_table[, participated := ifelse((day_id) %in% participant_day_ids, 1, 0)]

  participation_rates[[m]] <-
    participation_table %>%
    filter(day_weight > 0) %>%
    as_survey_design(weights = "day_weight") %>%
    group_by(survey_year, participated) %>%
    summarize(prop = survey_prop(proportion = FALSE, se = TRUE))
}

participation_rates <- rbindlist(participation_rates, idcol = "mode_type_binned")
participation_rates[, mode_type_binned := factorize_column(mode_type_binned, vals_df = value_labels, "mode_type_binned", value_label_colname = "label")]
participation_rates <- participation_rates[participated == 1]
participation_rates[, participated := NULL]
participation_rates <- participation_rates[order(survey_year, -prop)]
participation_rates[, mode_type_binned := factor(mode_type_binned,
                                                 levels = unique(participation_rates$mode_type_binned)
)]


fig <-
  ggplotly(
    tooltip = "text",
    ggplot(
      data = participation_rates,
      aes(
        x = mode_type_binned,
        y = prop,
        fill = survey_year,
        text = paste0(
          "Survey Year: ",
          survey_year,
          "<br>Mode: ",
          mode_type_binned,
          "<br>Percent of People: ",
          round(100 * prop, 1),
          "%"
        )
      )
    ) +
      geom_bar(
        position = position_dodge(),
        stat = "identity"
      ) +
      geom_errorbar(
        aes(
          ymin = prop - prop_se,
          ymax = prop + prop_se
        ),
        width = 0,
        position = position_dodge(width = 0.9)
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank()) +
      scale_x_discrete(labels = label_wrap_gen()) +
      scale_y_continuous(
        labels = scales::label_percent(),
        breaks = seq(from = 0, to = 1, by = .1)
      )
    # +
    #   scale_fill_manual(values = pal_years)
  ) %>%
  councilR::council_layout(
    x_title = "Trip Mode",
    y_title = "Trip Participation Rate",
    legend_title = "Survey Year"
  )
