test_that("backup_db creates backup with correct MD5", {
  # Setup - create test database
  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_backup.sqlite")
  file.copy(
    from = system.file("extdata", "starter_db.sqlite", package = "gopheR"),
    to = db_path,
    overwrite = TRUE
  )

  # Create backup
  backup_result <- backup_db(db_path = db_path, backup_dir = file.path(test_dir, "backups"))

  # Assert backup was created
  expect_true(file.exists(backup_result$backup_path))

  # Verify MD5s match
  src_md5 <- unname(tools::md5sum(db_path))
  bak_md5 <- unname(tools::md5sum(backup_result$backup_path))
  expect_equal(src_md5, bak_md5)

  # Cleanup
  unlink(db_path)
  unlink(backup_result$backup_path)
})


test_that("restore from backup works", {
  # Setup
  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_restore.sqlite")

  file.copy(
    from = system.file("extdata", "starter_db.sqlite", package = "gopheR"),
    to = db_path,
    overwrite = TRUE
  )

  # Add some data
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  test_obj <- data.frame(
    object_id = "test_obj",
    object_type = "genome",
    object_subtype = "MAG",
    label = NA,
    description = NA,
    created_by = NA,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "object", test_obj, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Create backup
  backup_result <- backup_db(db_path = db_path)

  # "Corrupt" database
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "DELETE FROM object;")
  DBI::dbDisconnect(con)

  # Restore from backup
  file.copy(
    from = backup_result$backup_path,
    to = db_path,
    overwrite = TRUE
  )

  # Verify restoration
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  objects_restored <- DBI::dbReadTable(con, "object")
  DBI::dbDisconnect(con)

  expect_equal(nrow(objects_restored), 1)
  expect_equal(objects_restored$object_id, "test_obj")

  # Cleanup
  unlink(db_path)
  unlink(dirname(backup_result$backup_path), recursive = TRUE)
})
