# This script is writen to run after
# 10-derive-table-trip-purpose.R

# 2019 -----
### Trip table (purpose on origin & destination ends) --------------
from <- c(
  "Work",
  "Work-related",
  "Home",
  "Spent the night at non-home location",
  "School",
  "School-related",
  "Escort",
  "Shop",
  "Errand/Other",
  "Social/Recreation",
  "Meal"
)
to <- c(
  "Work",
  "Work",
  "Home",
  "Home",
  "School",
  "School",
  "Maintenance",
  "Maintenance",
  "Maintenance",
  "Social/Recreation",
  "Social/Recreation"
)

trip19[
  data.table(from, to),
  on = .(d_purpose_category_imputed = from),
  d_purpose_category_broad := i.to
]

trip19[
  data.table(from, to),
  on = .(o_purpose_category_imputed = from),
  o_purpose_category_broad := i.to
]

trip_purpose19[
  data.table(from, to),
  on = .(purpose_category = from),
  purpose_category_broad := i.to
]

# 2021 -----
### Trip table (purpose on origin & destination ends) --------------
from <- c(
  "Work",
  "Work related",
  "Home",
  "Overnight",
  "School",
  "School related",
  "Escort",
  "Shopping",
  "Errand",
  "Other",
  "Social/Recreation",
  "Meal"
)
to <- c(
  "Work",
  "Work",
  "Home",
  "Home",
  "School",
  "School",
  "Maintenance",
  "Maintenance",
  "Maintenance",
  "Maintenance",
  "Social/Recreation",
  "Social/Recreation"
)

trip21[
  data.table(from, to),
  on = .(d_purpose_category = from),
  d_purpose_category_broad := i.to
]

trip21[
  data.table(from, to),
  on = .(o_purpose_category = from),
  o_purpose_category_broad := i.to
]

trip_purpose21[
  data.table(from, to),
  on = .(purpose_category = from),
  purpose_category_broad := i.to
]
