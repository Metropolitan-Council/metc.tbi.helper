# Load data from local file
# MetC staff only
# qs::qload("/Volumes/MTS/Working/Modeling/Household_Survey_Data/Dataset_2024-05-13/CombinedDataset_2024-05-13/dat_all_upcoded_labeled.qs")
# qs::qload("N:/MTS/Working/Modeling/Household_Survey_Data/Dataset_2024-05-13/CombinedDataset_2024-05-13/dat_all_upcoded_labeled.qs")
if(Sys.info()['sysname'] == 'Darwin'){
  system("open smb://rafsshare.mc.local/shared/MTS/")
  qs::qload("/Volumes/MTS/Working/Modeling/Household_Survey_Data/Dataset_2024-05-13/CombinedDataset_2024-05-13/dat_all_upcoded_labeled.qs")
}else{
  qs::qload("N:/MTS/Working/Modeling/Household_Survey_Data/Dataset_2024-05-13/CombinedDataset_2024-05-13/dat_all_upcoded_labeled.qs")
}
tbi <- dat_all_upcoded_labeled
rm(dat_all_upcoded_labeled)

var_list <- fread("/Volumes/MTS/Working/Modeling/Household_Survey_Data/Dataset_2024-05-13/WeightedData_2024-05-13/variable_list.csv")
val_list <- fread("/Volumes/MTS/Working/Modeling/Household_Survey_Data/Dataset_2024-05-13/WeightedData_2024-05-13/value_labels.csv")
