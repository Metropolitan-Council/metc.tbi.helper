# Load data from local file
# MetC staff only
# see Brandon/Liz to update the R environment file
qs::qload(file.path(Sys.getenv("path_to_tbi"), "dat_all_upcoded_labeled.qs"))
tbi <- dat_all_upcoded_labeled
rm(dat_all_upcoded_labeled)

val_list <- fread(file.path(Sys.getenv("path_to_tbi_wt"), "value_labels.csv"))
var_list <- fread(file.path(Sys.getenv("path_to_tbi_wt"), "variable_list.csv"))

