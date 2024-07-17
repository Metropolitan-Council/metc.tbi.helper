# This script writes a function called "create_table". The purpose is to convert data types of variables to what SQL Server can read and create an empty table before writing data.

create_table <- function(db_conn, tab, table_name) {
  types <- tbi[[tab]] %>%
    sapply(typeof)
  class <- tbi[[tab]] %>%
    sapply(class)

  # logical/double are invalid in SQL server, convert them to bit/float
  types[types == "logical"] <- "bit"
  types[types == "double"] <- "float"

  # for Date variables, set the data type as date
  types[class == "Date"] <- "date"

  # for integer64 variables, set the data type as character
  types[class == "integer64"] <- "character"

  # for POSIXct variables, set the data type as datetime
  time_check <- class %>%
    sapply(\(x) "POSIXct" %in% x)
  types[names(types) %in% names(time_check[time_check == TRUE])] <- "datetime"

  # for factor variables, set the data type as character
  factor_check <- class %>%
    sapply(\(x) "factor" %in% x)
  types[names(types) %in% names(factor_check[factor_check == TRUE])] <- "character"

  # adapt long string by editing the length of character
  check_length <- tbi[[tab]][, .SD, .SDcols = types == "character"] %>%
    sapply(\(x){
      na.omit(x) %>%
        as.character() %>%
        nchar() %>%
        max() > 255
    })
  fifelse(
    check_length[check_length == TRUE],
    types[names(types) %in% names(check_length[check_length == TRUE])] <- "varchar(max)",
    types[names(types) %in% names(check_length[check_length == FALSE])] <- "varchar(255)"
  )

  # create empty table in database
  dbCreateTable(conn = db_conn, name = table_name, fields = types)
}
