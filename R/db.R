#' Get path to a gopheR database file
#'
#' Resolves the active database using a three-step fallback chain:
#'
#' 1. **Explicit argument** --`path` (directory) + `db` (filename) passed directly
#' 2. **Options override** --`options(gopheR.db_path, gopheR.db_file)`, useful for
#'    temporarily pointing at a test database (`use_db()` sets these)
#' 3. **Den auto-detection** --walks up from the working directory looking for
#'    `den.yaml`; if found, reads the `database:` field and resolves the path
#'    relative to the den root
#'
#' @param path Character scalar. Directory containing the database file.
#'   Bypasses all fallback logic when provided.
#' @param db Character scalar. Database filename. Only used when `path` is also
#'   provided.
#'
#' @returns A length-1 character vector: full path to the database file.
#' @export

gopher_db_path <- function(path = NULL, db = NULL) {

  # 1. Explicit argument
  if (!is.null(path)) {
    db   <- db %||% getOption("gopheR.db_file", "gopheR_db.sqlite")
    full <- file.path(path, db)
    if (!file.exists(full)) {
      cli::cli_abort(c(
        "Database file not found.",
        "x" = "{.path {full}}"
      ))
    }
    return(full)
  }

  # 2. Options override (use_db() / use_den() / legacy manual setup)
  opt_path <- getOption("gopheR.db_path", "")
  opt_file <- getOption("gopheR.db_file", "")
  if (nzchar(opt_path) && nzchar(opt_file)) {
    full <- file.path(opt_path, opt_file)
    if (!file.exists(full)) {
      cli::cli_abort(c(
        "Database file not found (set via options).",
        "x" = "{.path {full}}",
        "i" = "Call {.fn use_den} to clear the override and fall back to the den."
      ))
    }
    return(full)
  }

  # 3. Den auto-detection
  den_root <- find_den_root(getwd())
  if (!is.null(den_root)) {
    yaml_path <- file.path(den_root, "den.yaml")
    lines     <- readLines(yaml_path, warn = FALSE)
    db_line   <- grep("^database:", lines, value = TRUE)
    if (length(db_line) == 1L) {
      db_name <- trimws(sub("^database:", "", db_line))
      full    <- file.path(den_root, db_name)
      if (file.exists(full)) return(full)
    }
    cli::cli_abort(c(
      "Found {.path {yaml_path}} but could not resolve the database file.",
      "i" = "Check the {.field database} field in {.file den.yaml}."
    ))
  }

  # 4. Legacy env var fallback
  env_path <- Sys.getenv("GOPHER_DB_PATH", unset = "")
  env_file <- Sys.getenv("GOPHER_DB_FILE", unset = getOption("gopheR.db_file", "gopheR_db.sqlite"))
  if (nzchar(env_path)) {
    full <- file.path(env_path, env_file)
    if (!file.exists(full)) {
      cli::cli_abort(c(
        "Database file not found (set via environment variables).",
        "x" = "{.path {full}}"
      ))
    }
    return(full)
  }

  cli::cli_abort(c(
    "Could not find a gopheR database.",
    "i" = "Options: open a den {.file .Rproj}, call {.fn use_db}, or set {.envvar GOPHER_DB_PATH}."
  ))
}


#' Temporarily use a specific database
#'
#' Sets `options(gopheR.db_path, gopheR.db_file)` to override den auto-detection.
#' Useful for pointing at a test database or a database outside the active den.
#' Call [use_den()] to clear the override and return to den auto-detection.
#'
#' @param path Character. Full path to a `.den` or `.sqlite` file, or a directory
#'   path when `db` is also provided.
#' @param db Character. Filename, if `path` is a directory.
#'
#' @returns Invisibly returns the resolved database path.
#' @export
#'
#' @examples
#' \dontrun{
#' use_db("~/projects/test_project/test.den")
#' use_db("~/projects/test_project", "test.den")
#' use_den()  # revert to den auto-detection
#' }

use_db <- function(path, db = NULL) {
  if (is.null(db)) {
    # path is the full file path
    if (!file.exists(path)) {
      cli::cli_abort("Database file not found: {.path {path}}")
    }
    options(gopheR.db_path = dirname(path))
    options(gopheR.db_file = basename(path))
    full <- path
  } else {
    full <- file.path(path, db)
    if (!file.exists(full)) {
      cli::cli_abort("Database file not found: {.path {full}}")
    }
    options(gopheR.db_path = path)
    options(gopheR.db_file = db)
  }
  cli::cli_alert_success("Active database: {.path {full}}")
  invisible(full)
}


#' Return to den auto-detection
#'
#' Clears the `gopheR.db_path` and `gopheR.db_file` options set by [use_db()],
#' allowing [gopher_db_path()] to fall back to den auto-detection via `den.yaml`.
#'
#' @returns Invisibly returns `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' use_db("~/projects/test_project/test.den")
#' # ... test work ...
#' use_den()  # back to the active den
#' }

use_den <- function() {
  options(gopheR.db_path = NULL)
  options(gopheR.db_file = NULL)
  den_root <- find_den_root()
  if (!is.null(den_root)) {
    cli::cli_alert_success("Using den: {.path {den_root}}")
  } else {
    cli::cli_alert_info("No den found in current directory --set one with {.fn use_db}.")
  }
  invisible(NULL)
}


#' Open a database connection
#'
#' @param db_path Full path to the database file. If `NULL`, resolved via
#'   [gopher_db_path()].
#' @param read_only Logical. If `TRUE`, opens in read-only mode.
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

with_gopher_con <- function(.f, ..., db_path = NULL, read_only = FALSE) {
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
#' @returns A data.frame.
#' @export

gopher_query <- function(con, sql, params = NULL) {
  if (is.null(params) || length(params) == 0) {
    return(DBI::dbGetQuery(con, sql))
  }
  DBI::dbGetQuery(con, sql, params = params)
}
