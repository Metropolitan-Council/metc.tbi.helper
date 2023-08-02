# This script is writen to run after
# "06-derive-var-person-race.R"

# 2019 --------------------------------------------
# * income_detailed --------------
from19 <- c(
  'Under $15,000',
  '$15,000-$24,999',
  '$25,000-$34,999',
  '$35,000-$49,999',
  '$50,000-$74,999',
  '$75,000-$99,999',
  '$100,000-$149,999',
  '$150,000-$199,999',
  '$200,000-$249,999',
  '$250,000 or more'
)
to19 <- c(
  "<$15K",
  "$15-25K",
  "$25-35K",
  "$35-50K",
  "$50-75K",
  "$75-100K",
  "$100-150K",
  "$150-200K",
  "$200-$250K",
  "$250K+"
)

temp_join <- data.table(
  income_detailed = from19
  , to19 = factor(to19, levels = to19, ordered = T)
)
household19[temp_join
            , on=.(income_detailed)
            , income_detailed := i.to19]
rm(from19, to19, temp_join)

# * income_broad --------------
from19 <- c(
  "Under $25,000"
  , "$25,000-$49,999"
  , "$50,000-$74,999"
  , "$75,000-$99,999"
  , "$100,000 or more"
)
to19 <- c(
  "<$25K"
  , "$25-50K"
  , "$50-75K"
  , "$75-100K"
  , "$100K+"
)
temp_join <- data.table(
  income_broad = from19
  , to19 = factor(to19, levels = to19, ordered = T)
)

household19[temp_join
            , on=.(income_broad)
            , income_broad := i.to19]
rm(from19, to19, temp_join)

# 2021 --------------------------------------------
# * income_detailed --------------
from21 <- c(
  'Less than $15,000',
  '$15,000-$24,999',
  '$25,000-$34,999',
  '$35,000-$49,999',
  '$50,000-$74,999',
  '$75,000-$99,999',
  '$100,000-$149,999',
  '$150,000-$199,999',
  '$200,000-$249,999',
  '$250,000 or more'
)
to21 <- c(
  "<$15K",
  "$15-25K",
  "$25-35K",
  "$35-50K",
  "$50-75K",
  "$75-100K",
  "$100-150K",
  "$150-200K",
  "$200-$250K",
  "$250K+"
)
temp_join <- data.table(
  income_detailed = from21
  , to21 = factor(to21, levels = to21, ordered = T)
)
household21[temp_join
            , on=.(income_detailed)
            , income_detailed := i.to21]
rm(from21, to21, temp_join)

# * income_broad --------------
from21 <- c(
  'Under $25,000',
  '$25,000-$49,999',
  '$50,000-$74,999',
  '$75,000-$99,999',
  '$100,000-$199,999',
  '$200,000 or more'
)
to21 <- c(
  "<$25K",
  "$25-50K",
  "$50-75K",
  "$75-100K",
  "$100K-200K",
  "$200K+"
)
temp_join <- data.table(
  income_broad = from21
  , to21 = factor(to21, levels = to21, ordered = T)
)
household21[temp_join
            , on=.(income_broad)
            , income_broad := i.to21]
rm(from21, to21, temp_join)










