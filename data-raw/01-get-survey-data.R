# Load data from local file
# MetC staff only
system("open smb://rafsshare.mc.local/shared/MTS/")
qs::qload("/Volumes/MTS/Working/Modeling/Household_Survey_Data/Dataset_2024-05-13/CombinedDataset_2024-05-13/dat_all_upcoded_labeled.qs")
tbi <- dat_all_upcoded_labeled
rm(dat_all_upcoded_labeled)

val_list <- fread("/Volumes/MTS/Working/Modeling/Household_Survey_Data/Dataset_2024-05-13/WeightedData_2024-05-13/value_labels.csv")
var_list <- fread("/Volumes/MTS/Working/Modeling/Household_Survey_Data/Dataset_2024-05-13/WeightedData_2024-05-13/variable_list.csv")
