# Load data from local file
# MetC staff only
# see Brandon/Liz to update the R environment file
# load data --------
source("data-raw/00-load-pkgs.R")
if (Sys.info()["sysname"] == "Darwin") system('open smb://rafsshare.mc.local/shared/MTS/')

tbi <- qread(file.path(Sys.getenv("path_to_tbi"), "dat_all_upcoded.qs"))$dat_all_upcoded
metaData_values <- fread(file.path(Sys.getenv("path_to_tbi"), "values.csv"))

# cleaning table
tbi$trip[pt_density == "Inf", pt_density := 0]
tbi$trip[point_dist_index == 0, point_dist_index := NA]

## Upcoded:  labeled data -----
tbi <-
  lapply(tbi,
         function(dt)
           factorize_df(
             dt,
             vals_df =
               unique(
                 metaData_values[,.(variable_unified,
                                        value_upcoded,
                                        label_upcoded)]
               ),
             variable_colname = 'variable_unified',
             value_colname = 'value_upcoded',
             value_label_colname = 'label_upcoded',
             value_order_colname = 'value_upcoded',
             add_na = FALSE
           ))
tbi$metaData_variables <- fread(file.path(Sys.getenv("path_to_tbi"), "variables.csv"))
tbi$metaData_values <- metaData_values
