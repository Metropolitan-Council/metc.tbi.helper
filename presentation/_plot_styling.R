# race_pal[, paste0('"', race_ethnicity, collapse = '" = "#", \n') %>% cat]
race_pal_c <-
  c("2 or more races" = "#543AA6",
  "American Indian, Alaskan Native" = "#DC6D2F",
  "Native Hawaiian, Pacific Islander" = "#012169",
  "Asian, Asian American" = "#C62A57",
  "Black, African, African American" = "#44ba7f",
  "Middle Eastern, North African" = "#21cbed",
  "Hispanic, Latinx, Latino" = "#f6b33c",
  "White" = "#4485ba",
  "No Say" = "#808080",
  "Don't Know" = "#808080",
  "Other" = "#808080",
  "Undisclosed" = "#fd8aff",
  "Black, Indigenous, \nand People of Color" = "grey20")

race_pal <-
  utils::stack(race_pal_c) %>%
  as.data.table() %>%
  setnames(c('values', 'ind'), c("color", "race"))

font_global <- list(
  size = 17
  , family = "Helvetica"
)

