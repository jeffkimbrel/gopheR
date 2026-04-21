test_that("read_bundle validates and inserts objects successfully", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_read_bundle.xlsx")

  # Create a test bundle with valid objects
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "object")

  test_objects <- data.frame(
    object_id = c("obj1", "obj2", "obj3"),
    object_type = c("genome:MAG", "readset:paired_end", "assembly:metagenome"),
    label = c(NA, NA, NA),
    description = c(NA, NA, NA),
    created_by = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "object", test_objects, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Ingest bundle
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user")

  # Verify objects were inserted
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  inserted_objects <- DBI::dbReadTable(con, "object")
  DBI::dbDisconnect(con)

  expect_equal(nrow(inserted_objects), 3)
  expect_true("obj1" %in% inserted_objects$object_id)

  # Check that types were split correctly
  obj1 <- inserted_objects |> dplyr::filter(object_id == "obj1")
  expect_equal(obj1$object_type, "genome")
  expect_equal(obj1$object_subtype, "MAG")

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("read_bundle validate_only mode does not insert objects", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_validate_only.xlsx")

  # Create test bundle
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "object")

  test_objects <- data.frame(
    object_id = c("obj1"),
    object_type = c("genome:MAG"),
    label = c(NA),
    description = c(NA),
    created_by = c(NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "object", test_objects, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Validate only
  result <- read_bundle(bundle_path, validate_only = TRUE, default_user = "test_user")

  # Verify objects were NOT inserted
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  inserted_objects <- DBI::dbReadTable(con, "object")
  DBI::dbDisconnect(con)

  expect_equal(nrow(inserted_objects), 0)

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("read_bundle fails with invalid object type", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_invalid_type.xlsx")

  # Create bundle with invalid type
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "object")

  test_objects <- data.frame(
    object_id = c("obj1"),
    object_type = c("invalid_type"),
    label = c(NA),
    description = c(NA),
    created_by = c(NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "object", test_objects, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail validation
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user"),
    "Invalid object types"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("read_bundle returns summary results", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_summary.xlsx")

  # Create test bundle
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "object")

  test_objects <- data.frame(
    object_id = c("obj1", "obj2", "obj3"),
    object_type = c("genome:MAG", "genome:isolate", "readset:paired_end"),
    label = c(NA, NA, NA),
    description = c(NA, NA, NA),
    created_by = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "object", test_objects, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Ingest
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user")

  # Check result structure
  expect_true("results" %in% names(result))
  expect_true("objects" %in% names(result$results))

  obj_results <- result$results$objects
  expect_equal(obj_results$n_processed, 3)
  expect_equal(obj_results$n_inserted, 3)

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})
