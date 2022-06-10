create_distance_threshold_table <-
  function(triptable = tbi_tables$trip,
           min_dist = 1,
           max_dist = 10,
           dist_interval = 1,
           by_variable1 = NULL,
           by_variable2 = NULL) {

    # No xtab variables -----------
    if(missing(by_variable1) & missing(by_variable2)){
    ## Threshold Calculations -------------------
      distlist <- list()
      for (this_distance in seq(from = min_dist, to = max_dist, by = dist_interval)) {
        # For the whole metro:
        distlist[[paste0(this_distance, " miles")]] <-
          triptable %>%
          dplyr::filter(distance > 0) %>%
          dplyr::mutate(help_text = ifelse(distance < !!this_distance, "shorter than", "longer than")) %>%
          # clean up:
          droplevels() %>%
          # big N sample size - for the whole data frame:
          dplyr::mutate(total_N = length(hh_id),
                        # raw sample size - number of people, trips, households, days
                        total_N_hh = length(unique(hh_id))) %>% # total number of households in sample
          srvyr::as_survey_design(weights = trip_weight) %>%
          dplyr::group_by(total_N,
                          total_N_hh,
                          help_text) %>%
          dplyr::summarize(
            group_N = length(hh_id),
            # raw sample size - number of people, trips, households, days (by group)
            group_N_hh = length(unique(hh_id)),
            # number of households in sample (by group)
            n_trips = srvyr::survey_total(),
            pct_trips = 100 * srvyr::survey_prop()
          ) %>%
          dplyr::mutate(distance_cutoff = !!this_distance) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, digits = 5))

      }

      threshold_dat <-
        dplyr::bind_rows(distlist, .id = "distance_threshold") %>%
        dplyr::mutate(distance_threshold = factor(distance_threshold))

      return(threshold_dat)
    }

    # One Way Table Method -----------
    if(!missing(by_variable1) & missing(by_variable2)){
      jointable_1 <-
        tbi_dict %>%
        dplyr::filter(variable == by_variable1) %>%
        dplyr::select(which_table) %>%
        unique() %>%
        magrittr::extract2(1)

      vartype1 <-
        tbi_tables[[jointable_1]] %>%
        select(sym(by_variable1)) %>%
        summarize_all(class) %>%
        purrr::pluck(1)


      tab_0 <- triptable %>%
        dplyr::left_join(tbi_tables[[jointable_1]]) %>%
        dplyr::filter(!(get(by_variable1) %in% missing_codes)) %>%
        dplyr::select(sym(by_variable1), hh_id, person_id, trip_id, distance, trip_weight)

      ## Bin row variable ----------------------------
      if (vartype1 == "numeric") {
        # cut into bins:
        brks <- histogram_breaks[[by_variable1]]$breaks
        brks_labs <- histogram_breaks[[by_variable1]]$labels


        tab_1 <- tab_0 %>%
          dplyr::mutate(cuts = cut(
            get(by_variable1),
            breaks = brks,
            labels =  brks_labs,
            order_result = TRUE
          )) %>%
          dplyr::select(-rlang::sym(by_variable1)) %>%
          dplyr::rename(!!rlang::enquo(by_variable1) := cuts)


      } else if (vartype1 == "ITime") {
        brks <- histogram_breaks[[by_variable1]]$breaks
        brks_labs <- histogram_breaks[[by_variable1]]$labels

        tab_1 <- tab_0 %>%
          dplyr::mutate(
            cuts = cut(
              get(by_variable1),
              breaks = brks,
              labels =  brks_labs,
              order_result = TRUE,
              include.lowest = TRUE
            )
          ) %>%
          dplyr::select(-rlang::sym(by_variable1)) %>%
          dplyr::rename(!!rlang::enquo(by_variable1) := cuts)


      } else {
        tab_1 <- tab_0
      }


      ## Threshold Calculations -------------------
      distlist <- list()
      for (this_distance in seq(from = min_dist, to = max_dist, by = dist_interval)) {
        # For the whole metro:
        distlist[[paste0(this_distance, " miles")]] <-
          tab_1 %>%
          dplyr::filter(distance > 0) %>%
          dplyr::mutate(help_text = ifelse(distance < !!this_distance, "shorter than", "longer than")) %>%
          # clean up:
          droplevels() %>%
          # big N sample size - for the whole data frame:
          dplyr::mutate(total_N = length(hh_id),
                        # raw sample size - number of people, trips, households, days
                        total_N_hh = length(unique(hh_id))) %>% # total number of households in sample
          srvyr::as_survey_design(weights = trip_weight) %>%
          dplyr::group_by(total_N,
                          total_N_hh,
                          help_text,
                          get(by_variable1)) %>%
          dplyr::summarize(
            group_N = length(hh_id),
            # raw sample size - number of people, trips, households, days (by group)
            group_N_hh = length(unique(hh_id)),
            # number of households in sample (by group)
            n_trips = srvyr::survey_total(),
            pct_trips = 100 * srvyr::survey_prop()
          ) %>%
          dplyr::mutate(distance_cutoff = !!this_distance) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, digits = 5)) %>%
          # rename the column back to the original name - it gets weird for some reason
          dplyr::rename(!!rlang::quo_name(by_variable1) := `get(by_variable1)`)

      }

      threshold_dat <-
        dplyr::bind_rows(distlist, .id = "distance_threshold") %>%
        dplyr::mutate(distance_threshold = factor(distance_threshold))

      return(threshold_dat)
    }


    # Two Way Table Method ------------
    else if(!missing(by_variable1) & !missing(by_variable2)){
      jointable_1 <-
        tbi_dict %>%
        dplyr::filter(variable == by_variable1) %>%
        dplyr::select(which_table) %>%
        unique() %>%
        magrittr::extract2(1)

      vartype1 <-
        tbi_tables[[jointable_1]] %>%
        select(sym(by_variable1)) %>%
        summarize_all(class) %>%
        purrr::pluck(1)

      jointable_2 <-
        tbi_dict %>%
        dplyr::filter(variable == by_variable2) %>%
        dplyr::select(which_table) %>%
        unique() %>%
        magrittr::extract2(1)

      vartype2 <-
        tbi_tables[[jointable_2]] %>%
        select(sym(by_variable2)) %>%
        summarize_all(class) %>%
        purrr::pluck(1)

      tab_0 <- triptable %>%
        dplyr::left_join(tbi_tables[[jointable_1]]) %>%
        dplyr::left_join(tbi_tables[[jointable_2]]) %>%
        dplyr::filter(!(get(by_variable1) %in% missing_codes)) %>%
        dplyr::filter(!(get(by_variable2) %in% missing_codes)) %>%
        dplyr::select(sym(by_variable1), sym(by_variable2), hh_id, person_id, trip_id, distance, trip_weight)

      ## Bin row variable ----------------------------
      if (vartype1 == "numeric") {
        # cut into bins:
        brks <- histogram_breaks[[by_variable1]]$breaks
        brks_labs <- histogram_breaks[[by_variable1]]$labels


        tab_1 <- tab_0 %>%
          dplyr::mutate(cuts = cut(
            get(by_variable1),
            breaks = brks,
            labels =  brks_labs,
            order_result = TRUE
          )) %>%
          dplyr::select(-rlang::sym(by_variable1)) %>%
          dplyr::rename(!!rlang::enquo(by_variable1) := cuts)


      } else if (vartype1 == "ITime") {
        brks <- histogram_breaks[[by_variable1]]$breaks
        brks_labs <- histogram_breaks[[by_variable1]]$labels

        tab_1 <- tab_0 %>%
          dplyr::mutate(
            cuts = cut(
              get(by_variable1),
              breaks = brks,
              labels =  brks_labs,
              order_result = TRUE,
              include.lowest = TRUE
            )
          ) %>%
          dplyr::select(-rlang::sym(by_variable1)) %>%
          dplyr::rename(!!rlang::enquo(by_variable1) := cuts)


      } else {
        tab_1 <- tab_0
      }

      ## Bin column variable ----------------------------
      if (vartype2 == "numeric") {
        # cut into bins:
        brks <- histogram_breaks[[by_variable2]]$breaks
        brks_labs <- histogram_breaks[[by_variable2]]$labels

        tab_2 <- tab_1 %>%
          dplyr::mutate(cuts = cut(
            get(by_variable2),
            breaks = brks,
            labels =  brks_labs,
            order_result = TRUE
          )) %>%
          dplyr::select(-rlang::sym(by_variable2)) %>%
          dplyr::rename(!!rlang::enquo(by_variable2) := cuts)


      } else if (vartype2 == "ITime") {
        brks <- histogram_breaks[[vartype2]]$breaks
        brks_labs <- histogram_breaks[[vartype2]]$labels

        tab_2 <- tab_1 %>%
          dplyr::mutate(
            cuts = cut(
              get(vartype2),
              breaks = brks,
              labels =  brks_labs,
              order_result = TRUE,
              include.lowest = TRUE
            )
          ) %>%
          dplyr::select(-rlang::sym(vartype2)) %>%
          dplyr::rename(!!rlang::enquo(vartype2) := cuts)

      } else {
        tab_2 <- tab_1
      }

      ## Threshold Calculations -------------------
      distlist <- list()
      for (this_distance in seq(from = min_dist, to = max_dist, by = dist_interval)) {
        # For the whole metro:
        distlist[[paste0(this_distance, " miles")]] <-
          tab_2 %>%
          dplyr::filter(distance > 0) %>%
          dplyr::mutate(help_text = ifelse(distance < !!this_distance, "shorter than", "longer than")) %>%
          # clean up:
          droplevels() %>%
          # big N sample size - for the whole data frame:
          dplyr::mutate(total_N = length(hh_id),
                        # raw sample size - number of people, trips, households, days
                        total_N_hh = length(unique(hh_id))) %>% # total number of households in sample
          srvyr::as_survey_design(weights = trip_weight) %>%
          dplyr::group_by(total_N,
                          total_N_hh,
                          help_text,
                          get(by_variable1),
                          get(by_variable2)) %>%
          dplyr::summarize(
            group_N = length(hh_id),
            # raw sample size - number of people, trips, households, days (by group)
            group_N_hh = length(unique(hh_id)),
            # number of households in sample (by group)
            n_trips = srvyr::survey_total(),
            pct_trips = 100 * srvyr::survey_prop()
          ) %>%
          dplyr::mutate(distance_cutoff = !!this_distance) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, digits = 5)) %>%
          # rename the column back to the original name - it gets weird for some reason
          dplyr::rename(!!rlang::quo_name(by_variable1) := `get(by_variable1)`) %>%
          dplyr::rename(!!rlang::quo_name(by_variable2) := `get(by_variable2)`)

      }

      threshold_dat <-
        dplyr::bind_rows(distlist, .id = "distance_threshold") %>%
        dplyr::mutate(distance_threshold = factor(distance_threshold))

      return(threshold_dat)
    }


  }
