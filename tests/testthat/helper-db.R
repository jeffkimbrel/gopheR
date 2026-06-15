#' Create a test database from the starter database
#'
#' Copies the gopheR starter database to a temporary location for testing.
#' The starter database includes the full schema with example spec tables
#' (object_type, object_subtype, edge_spec, etc.) and empty data tables.
#'
#' @param path Directory to create the database in. If NULL, uses a temp dir.
#' @param db_name Name of the database file.
#' @param return_full_path If TRUE, returns full file path. If FALSE, returns list with dir and file.
#'
#' @return Path to the created test database file, or list with dir and file components.

create_test_db <- function(path = NULL, db_name = "test_gopher.den", return_full_path = TRUE) {

  if (is.null(path)) {
    path <- tempdir()
  }

  db_path <- file.path(path, db_name)

  # Remove if exists
  if (file.exists(db_path)) {
    unlink(db_path)
  }

  # Copy starter database from package
  starter_db <- system.file("extdata", "starter_db.den", package = "gopheR")

  if (!file.exists(starter_db)) {
    stop("Starter database not found in package. Expected at: ", starter_db)
  }

  file.copy(from = starter_db, to = db_path, overwrite = TRUE)

  # Populate spec tables required by tests
  populate_test_specs(db_path)

  if (return_full_path) {
    return(db_path)
  } else {
    return(list(dir = path, file = db_name, full_path = db_path))
  }
}


#' Seed test-only data into a fresh test database
#'
#' Adds a pre-existing test_user person so tests that pass default_user =
#' "test_user" don't trigger the interactive auto-create prompt. All spec
#' tables (object types, edge types, result keys, file roles) are already
#' populated in the starter database.
#'
#' @param db_path Full path to the test database.

populate_test_specs <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  DBI::dbWriteTable(con, "people", data.frame(
    person_id = "test_user",
    full_name = "Test User",
    email     = "test@example.com",
    is_active = 1L,
    stringsAsFactors = FALSE
  ), append = TRUE, row.names = FALSE)

  DBI::dbDisconnect(con)
  invisible(db_path)
}


#' Add test objects to database
#'
#' Convenience function to populate a test database with sample objects.
#'
#' @param db_path Path to the test database.
#' @param objects Data frame of objects to add.

add_test_objects <- function(db_path, objects) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  DBI::dbWriteTable(con, "object", objects, append = TRUE, row.names = FALSE)

  DBI::dbDisconnect(con)

  invisible(db_path)
}


#' Setup test environment with database options
#'
#' Configures options for testing gopheR functions. Call this at the
#' beginning of tests that use read_bundle() or write_bundle().
#'
#' @param db_info List returned from create_test_db(return_full_path = FALSE)
#'
#' @return Invisibly returns the db_info list.

setup_test_env <- function(db_info) {
  options(gopheR.db_path = db_info$dir)
  options(gopheR.db_file = db_info$file)
  invisible(db_info)
}


#' Clean up test environment
#'
#' Removes gopheR options and database files created during testing.
#'
#' @param db_info List with database info from create_test_db()

cleanup_test_env <- function(db_info) {
  options(gopheR.db_path = NULL)
  options(gopheR.db_file = NULL)

  if (file.exists(db_info$full_path)) {
    unlink(db_info$full_path)
  }

  backup_dir <- file.path(db_info$dir, "backups")
  if (dir.exists(backup_dir)) {
    unlink(backup_dir, recursive = TRUE)
  }

  invisible(NULL)
}
