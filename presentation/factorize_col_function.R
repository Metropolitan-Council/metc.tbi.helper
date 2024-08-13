factorize_column = function(x,
                            var_str,
                            vals_df,
                            variable_colname = 'variable',
                            value_colname = 'value',
                            value_label_colname = 'value_label',
                            value_order_colname = 'value',
                            extra_labels = NULL,
                            add_na = TRUE) {
  vals_df = data.table::data.table(vals_df)

  # sort the vals_df to ensure the ordered factor is ordered correctly
  vals_df = vals_df[with(vals_df,
                         order(get(variable_colname),
                               get(value_order_colname),
                               get(value_colname),
                               get(value_label_colname))), ]

  # select levels and labels for this variable
  levels  = vals_df[get(variable_colname) == var_str, get(value_colname)]
  labels  = vals_df[get(variable_colname) == var_str, get(value_label_colname)]

  # add extra labels, if provided
  if ( !missing(extra_labels) ) {
    levels = c(levels, vals_df[get(variable_colname) %in% extra_labels,  get(value_colname)])
    labels = c(labels, vals_df[get(variable_colname) %in% extra_labels,  get(value_label_colname)])
  }

  # only apply labels if there are labels specific to this variable
  if (var_str %in% unique(vals_df[, get(variable_colname)])) {

    #perform QC checks before factorizing

    # check for duplicate labels (gets a warning)
    if ( any(duplicated(labels)) ) {
      warning('Duplicated labels in variable "', var_str, '". Labels: ',
              paste(labels, collapse = '; '))
    }

    # check for duplicate values (or "levels") (gets a warning)
    if ( any(duplicated(levels)) ) {
      warning('Duplicated values/levels in variable "', var_str, '". Values: ',
              paste(levels, collapse = '; '))
    }

    # check for missing levels (except NA) (gets a warning)
    if ( any(!(unique(x) %in% c(levels, NA))) ) {
      missing_levels = unique(x)[!unique(x) %in% c(levels, NA)]

      levels = c(levels, missing_levels)
      labels = c(labels, missing_levels)

      warning('Missing labels in variable "', var_str, '". Values missing labels: ',
              paste(missing_levels, collapse = '; '))
    }

    # TODO add checks after making variable a factor to ensure it's right?

    y = factor(x, levels = levels, labels = labels, ordered = TRUE)

    # if NAs are desired.
    if (add_na) {y = addNA(y)}
    #if (add_na) {forcats::fct_explicit_na(y, na_level = "(Missing - NA)")}

  } else {

    # if there are no labels in the codebook for this variable, it is returned unchanged.
    y = x
  }
  return(invisible(y))
}
