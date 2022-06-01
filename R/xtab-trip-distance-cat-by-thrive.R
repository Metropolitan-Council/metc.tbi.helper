### Cumulative Trip Distances, All trips -------------
get_distance_threshold_table <-

  function(triptable,
           min_dist = 1,
           max_dist = 10,
           dist_interval = 1,
           byvar1 = NULL,
           byvar2 = NULL) {

    distlist <- list()

    if (missing(byvar1) & missing(byvar2)) {
      for (this_distance in seq(from = min_dist, to = max_dist, by = dist_interval)) {
        # For the whole metro:
        distlist[[paste0(this_distance, " miles")]] <-
          triptable %>%
          filter(distance > 0) %>%
          mutate(help_text = ifelse(distance < !!this_distance, "shorter than", "longer than")) %>%
          srvyr::as_survey_design(ids = 1, weights = trip_weight) %>%
          group_by(help_text, byvar1, byvar2) %>%
          summarize(
            n_trips = srvyr::survey_total(),
            pct_trips = 100 * srvyr::survey_prop()
          ) %>%
          mutate(distance_cutoff = !!this_distance)
      }
      threshold_dat <-
        bind_rows(distlist) %>%
        mutate(help_text = paste0(round(pct_trips), "% of trips are ", help_text, " miles."))

      return(threshold_dat)
    }

    else if (!missing(byvar1) & missing(byvar2)) {
      for (this_distance in seq(from = min_dist, to = max_dist, by = dist_interval)) {
        # For the whole metro:
        distlist[[paste0(this_distance, " miles")]] <-
          triptable %>%
          filter(distance > 0) %>%
          mutate(help_text = ifelse(distance < !!this_distance, "shorter than", "longer than")) %>%
          srvyr::as_survey_design(ids = 1, weights = trip_weight) %>%
          group_by(help_text, get(byvar1)) %>%
          summarize(
            n_trips = srvyr::survey_total(),
            pct_trips = 100 * srvyr::survey_prop()
          ) %>%
          mutate(distance_cutoff = !!this_distance)
      }
      threshold_dat <-
        bind_rows(distlist) %>%
        mutate(help_text = paste0(round(pct_trips), "% of trips are ", help_text, " miles."))

      return(threshold_dat)
    }

    else if (!missing(byvar1) & !missing(byvar2)) {
      for (this_distance in seq(from = min_dist, to = max_dist, by = dist_interval)) {
        # For the whole metro:
        distlist[[paste0(this_distance, " miles")]] <-
          triptable %>%
          filter(distance > 0) %>%
          mutate(help_text = ifelse(distance < !!this_distance, "shorter than", "longer than")) %>%
          srvyr::as_survey_design(ids = 1, weights = trip_weight) %>%
          group_by(help_text, get(byvar1), get(byvar2)) %>%
          summarize(
            n_trips = srvyr::survey_total(),
            pct_trips = 100 * srvyr::survey_prop()
          ) %>%
          mutate(distance_cutoff = !!this_distance)
      }
      threshold_dat <-
        bind_rows(distlist) %>%
        mutate(help_text = paste0(round(pct_trips), "% of trips are ", help_text, " miles."))

      return(threshold_dat)
    }

    else {message("Error in specifying function arguments. Check again?")}



  }
