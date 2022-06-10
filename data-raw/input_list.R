## code to prepare `input_list` dataset goes here

library(dplyr)
# library(travel.survey.explorer)

input_list <- purrr::map(unique(tbi_dict$category), function(x) {

  tb <- tbi_dict %>%
    dplyr::filter(category == x)

  variab <- tb$variable %>%
    unique() %>%
    as.list()

  variab_name <- tb$variable_label %>%
    unique() %>%
    as.list()

  names(variab) <- variab_name
  return(variab)
})

names(input_list) <- unique(tbi_dict$category)


usethis::use_data(input_list, overwrite = TRUE)


## code to prepare `input_list` dataset goes here

library(dplyr)
# library(travel.survey.explorer)

input_question_list <- purrr::map(unique(tbi_dict$variable), function(x) {

    tb <- tbi_dict %>%
    dplyr::filter(variable == x)

  variab <- tb$variable %>%
    unique() %>%
    as.list()

  variab_name <- tb$survey_question %>%
    unique() %>%
    as.list()

  names(variab) <- variab_name

  return(variab)
})


names(input_question_list) <- unique(tbi_dict$variable)


usethis::use_data(input_question_list, overwrite = TRUE)
