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
      stop(
        "No database path set. Provide `path=` or set option('gopheR.db_path') / env var GOPHER_DB_PATH.",
        call. = FALSE
      )
    }
  }

  gopher_path <- file.path(path, db)
  if (!file.exists(gopher_path)) {
    stop(glue::glue("No database file found at {gopher_path}"), call. = FALSE)
  }

  gopher_path
}



#' Open a database connection
#'
#' @param path,db Passed to [gopher_db_path()].
#' @param read_only Logical. If `TRUE`, attempts to open the SQLite DB in read-only mode.
#'
#' @returns A DBI connection.
#' @export

gopher_con <- function(path = NULL, db = "gopheR_db.sqlite", read_only = FALSE) {
  dbfile <- gopher_db_path(path = path, db = db)

  if (isTRUE(read_only)) {
    # SQLite URI read-only. Requires SQLite to support URI filenames.
    # Works in most modern builds.
    uri <- paste0("file:", normalizePath(dbfile, winslash = "/"), "?mode=ro")
    return(DBI::dbConnect(RSQLite::SQLite(), uri, extended_types = TRUE))
  }

  DBI::dbConnect(RSQLite::SQLite(), dbfile, extended_types = TRUE)
}



#' Evaluate a function with a database connection
#'
#' @param .f A function with first argument `con`.
#' @param ... Additional arguments passed to `.f`.
#' @param path,db,read_only Passed to [gopher_con()].
#'
#' @returns The return value of `.f`.
#' @export
with_gopher_con <- function(.f, ..., path = NULL, db = "gopheR_db.sqlite", read_only = FALSE) {
  con <- gopher_con(path = path, db = db, read_only = read_only)
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






#' Extract native id from a UID like "mag:M001" (R-side)
#'
#' @param uid uid to strip
#'
#' @export

uid_native <- function(uid) {
  sub("^.*?:", "", uid)
}
