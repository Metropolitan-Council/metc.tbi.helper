## code to prepare `plotly_layout` dataset goes here
library(councilR)
font_family_list <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"

plotly_layout <- list(
  "hover_text" = list(
    size = 16,
    family = font_family_list
  ),
  "axis_titlefont" = list(
    size = 16,
    family = font_family_list,
    color = councilR::colors$suppBlack
  ),
  "tickfont" = list(
    size = 12,
    family = font_family_list,
    color = councilR::colors$suppBlack
  )
)



usethis::use_data(plotly_layout, overwrite = TRUE)
