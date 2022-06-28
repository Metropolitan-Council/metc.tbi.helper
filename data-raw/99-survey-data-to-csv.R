library(tidyr)
library(devtools)
directory_to_save <- "data/"

# load 2019 data ----
load(url(paste0("https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
                "tbi19.rda")))

# Write 2019 data ----
for(i in names(tbi19)){
  write.csv(tbi19[[i]], paste0(directory_to_save, i, "-2019", ".csv"), row.names = F)
}

# load 2021 data ----
load(url(paste0("https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
                "tbi21.rda")))

# Write 2021 data ----
for(i in names(tbi21)){
  write.csv(tbi21[[i]], paste0(directory_to_save, i, "-2021", ".csv"), row.names = F)
}

