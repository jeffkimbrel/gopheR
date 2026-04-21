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

create_test_db <- function(path = NULL, db_name = "test_gopher.sqlite", return_full_path = TRUE) {

  if (is.null(path)) {
    path <- tempdir()
  }

  db_path <- file.path(path, db_name)

  # Remove if exists
  if (file.exists(db_path)) {
    unlink(db_path)
  }

  # Copy starter database from package
  starter_db <- system.file("extdata", "starter_db.sqlite", package = "gopheR")

  if (!file.exists(starter_db)) {
    stop("Starter database not found in package. Expected at: ", starter_db)
  }

  file.copy(from = starter_db, to = db_path, overwrite = TRUE)

  if (return_full_path) {
    return(db_path)
  } else {
    return(list(dir = path, file = db_name, full_path = db_path))
  }
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
