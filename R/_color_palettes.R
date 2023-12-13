if (exists("tpal") == FALSE) {
  tpal <-
    list(
      # Blues
      "medblue" = "#0054A4",
      "liblue" = "#1887F0",
      "dablue" = "#002D57",
      "peerblue" = "#ADBFCC",

      # Browns
      "medbrown" = "#A37731",
      "librown" = "#F09400",
      "dabrown" = "#573600",

      # Reds
      "medred" = "#A33A2C",
      "lired" = "#D64C3A",
      "dared" = "#573631",

      # Greens
      "medgreen" = "#57A22F",
      "ligreen" = "#B0F08D",
      "dagreen" = "#3C7021"
    )

  tbiyrpal <-
    c(
      "2010" = tpal$medblue,
      "2018-2019" = tpal$dablue,
      "2019" = tpal$dablue,
      "2021" = tpal$lired
    )


  pvmtpal <-
    c(
      "Fair" = tpal$librown,
      "Good" = tpal$liblue,
      "Poor" = tpal$lired
    )

  cli::cli_inform(
    c("v" = "Color palettes\n"),
    .frequency = "once",
    .frequency_id = "color_palettes"
  )
}
