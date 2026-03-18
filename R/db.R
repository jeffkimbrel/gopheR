#' Get path to a gopheR SQLite database file
#'
#' Reliable set of code to determine where exactly a gopheR database is in a
#' project.
#'
#' @param path Character scalar. Directory containing the database file.
#'   If `NULL`, uses `getOption("gopheR.db_path")` or `Sys.getenv("GOPHER_DB_PATH")`.
#' @param db Character scalar. Database filename.
#'
#' @returns A length-1 character vector: full path to the database file.
#' @export

gopher_db_path <- function(path = NULL,
                           db = getOption("gopheR.db_file", "gopheR_db.sqlite")) {

  if (is.null(path)) {
    path <- getOption("gopheR.db_path", Sys.getenv("GOPHER_DB_PATH", unset = ""))
    if (!nzchar(path)) {
      cli::cli_abort(c(
        "No database path set.",
        "i" = "Provide {.arg db_path} directly, or set {.option gopheR.db_path}, or {.envvar GOPHER_DB_PATH}."
      ))
    }
  }

  gopher_path <- file.path(path, db)
  if (!file.exists(gopher_path)) {
    cli::cli_abort(c(
      "No database file found.",
      "x" = "Path: {.path {gopher_path}}",
      "i" = "Check that {.option gopheR.db_path} and {.option gopheR.db_file} are set correctly."
    ))
  }

  gopher_path
}



#' Open a database connection
#'
#' @param db_path path to gopher DB
#' @param read_only Logical. If `TRUE`, attempts to open the SQLite DB in read-only mode.
#'
#' @returns A DBI connection.
#' @export

gopher_con <- function(db_path = NULL, read_only = FALSE) {
  dbfile <- if (is.null(db_path)) gopher_db_path() else db_path

  if (isTRUE(read_only)) {
    uri <- paste0("file:", normalizePath(dbfile, winslash = "/"), "?mode=ro")
    con <- DBI::dbConnect(RSQLite::SQLite(), uri, extended_types = TRUE)
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
    return(con)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), dbfile, extended_types = TRUE)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  con
}

#' Evaluate a function with a database connection
#'
#' @param .f A function with first argument `con`.
#' @param ... Additional arguments passed to `.f`.
#' @param db_path,read_only Passed to [gopher_con()].
#'
#' @returns The return value of `.f`.
#' @export

with_gopher_con <- function(.f, ...,
                            db_path = NULL,
                            read_only = FALSE) {
  con <- gopher_con(db_path = db_path, read_only = read_only)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .f(con, ...)
}

#' Run a parameterized query and return a data frame
#'
#' @param con A DBI connection.
#' @param sql A SQL query string, using `?` placeholders for parameters.
#' @param params A list of parameters to bind.
#'
#' @returns A data.frame (or tibble, depending on DBI backend).
#' @export

gopher_query <- function(con, sql, params = NULL) {
  if (is.null(params) || length(params) == 0) {
    return(DBI::dbGetQuery(con, sql))
  }
  DBI::dbGetQuery(con, sql, params = params)
}
