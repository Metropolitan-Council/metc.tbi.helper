library(tidyr)
library(ROracle)

tbidb <- ROracle::dbConnect(
  dbDriver("Oracle"),
  dbname = keyring::key_get("mts_planning_database_string"),
  username = "mts_planning_data",
  password = keyring::key_get("mts_planning_data_pw")
)

ROracle::dbReadTable(tbidb, "tbi_19_day_public") %>%
  write.csv("data/data-csv/day.csv", row.names = F)

ROracle::dbWriteTable(tbidb, "tbi_19_trip_public")%>%
  write.csv("data/data-csv/trip.csv", row.names = F)

ROracle::dbWriteTable(tbidb, "tbi_19_hh_public")%>%
  write.csv("data/data-csv/hh.csv", row.names = F)

ROracle::dbWriteTable(tbidb, "tbi_19_veh_public")%>%
  write.csv("data/data-csv/veh.csv", row.names = F)

ROracle::dbWriteTable(tbidb, "tbi_19_per_public")%>%
  write.csv("data/data-csv/per.csv", row.names = F)

