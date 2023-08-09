if (exists("db_connect") == FALSE) {
  db_connect <- function(uid = keyring::key_get("councilR.uid"),
                         pwd = keyring::key_get("councilR.pwd")) {
    purrr::map(c(uid, pwd), rlang:::check_string)

    requireNamespace("DBI", quietly = TRUE)


    drv <- if (grepl("mac", osVersion)) {
      "FreeTDS"
    } else {
      "SQL Server"
    }

    # check that db can connect
    if (drv == "FreeTDS") {
      if (DBI::dbCanConnect(odbc::odbc(),
        Driver = drv,
        Database = "MTS_Planning_Data",
        Uid = uid,
        Pwd = pwd,
        Server = keyring::key_get("mts_planning_database_string")
      ) == FALSE) {
        cli::cli_abort("Database failed to connect")
      }
    } else if (drv == "SQL Server") {
      if (DBI::dbCanConnect(odbc::odbc(),
        Driver = drv,
        Database = "MTS_Planning_Data",
        Server = keyring::key_get("mts_planning_database_string"),
        Trusted_Connection = "yes"
      ) ==
        FALSE) {
        cli::cli_abort("Database failed to connect")
      }
    }


    # create db connection

    conn <- if (drv == "FreeTDS") {
      DBI::dbConnect(odbc::odbc(),
        Driver = drv,
        Database = "MTS_Planning_Data",
        Uid = uid,
        Pwd = pwd,
        Server = keyring::key_get("mts_planning_database_string")
      )
    } else if (drv == "SQL Server") {
      DBI::dbConnect(odbc::odbc(),
        Driver = drv,
        Database = "MTS_Planning_Data",
        Server = keyring::key_get("mts_planning_database_string"),
        Trusted_Connection = "yes"
      )
    }



    return(conn)
  }


  db_connect_gis <- function(uid = keyring::key_get("mts_planning_database_string")("councilR.uid"),
                             pwd = keyring::key_get("mts_planning_database_string")("councilR.pwd")) {
    purrr::map(c(uid, pwd), rlang:::check_string)
    requireNamespace("DBI", quietly = TRUE)



    drv <- if (grepl("mac", osVersion)) {
      "FreeTDS"
    } else {
      "SQL Server"
    }

    # check that db can connect
    if (drv == "FreeTDS") {
      if (DBI::dbCanConnect(odbc::odbc(),
        "GISLibrary",
        Driver = "FreeTDS",
        timeout = 10,
        Uid = uid,
        Pwd = pwd
      ) == FALSE) {
        cli::cli_abort("Database failed to connect")
      }
    } else if (drv == "SQL Server") {
      if (DBI::dbCanConnect(
        odbc::odbc(),
        "GISLibrary"
      ) == FALSE) {
        cli::cli_abort("Database failed to connect")
      }
    }


    # create db connection

    conn <- if (drv == "FreeTDS") {
      DBI::dbConnect(odbc::odbc(),
        "GISLibrary",
        Driver = "FreeTDS",
        timeout = 10,
        Uid = uid,
        Pwd = pwd
      )
    } else if (drv == "SQL Server") {
      DBI::dbConnect(
        odbc::odbc(),
        "GISLibrary"
      )
    }



    return(conn)
  }


  cli::cli_inform(
    c("v" = "Database functions\n"),
    .frequency = "once",
    .frequency_id = "db_connectr"
  )
}
