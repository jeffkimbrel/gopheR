test_that("read_bundle ingests results successfully", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_results.xlsx")

  # First add workflow and object
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  test_workflow <- data.frame(
    workflow_id = "wf001",
    description = "Test workflow",
    created_by = NA,
    workflow_date = NA,
    stringsAsFactors = FALSE
  )
  test_object <- data.frame(
    object_id = "genome001",
    object_type = "genome",
    object_subtype = "MAG",
    label = NA,
    description = NA,
    created_by = NA,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "workflow", test_workflow, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "object", test_object, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Create bundle with results
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "result")

  test_results <- data.frame(
    object_id = c("genome001", "genome001"),
    workflow_id = c("wf001", "wf001"),
    key = c("completeness", "contamination"),
    value = c("95.2", "2.1"),
    unit = c("%", "%"),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "result", test_results, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Ingest bundle
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user")

  # Verify results were inserted
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  inserted_results <- DBI::dbReadTable(con, "result")
  DBI::dbDisconnect(con)

  expect_equal(nrow(inserted_results), 2)
  expect_true("completeness" %in% inserted_results$key)
  expect_true("contamination" %in% inserted_results$key)

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("result ingestion detects invalid keys", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_invalid_key.xlsx")

  # Add workflow and object
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  test_workflow <- data.frame(
    workflow_id = "wf001",
    description = "Test workflow",
    created_by = NA,
    workflow_date = NA,
    stringsAsFactors = FALSE
  )
  test_object <- data.frame(
    object_id = "genome001",
    object_type = "genome",
    object_subtype = "MAG",
    label = NA,
    description = NA,
    created_by = NA,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "workflow", test_workflow, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "object", test_object, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Create bundle with invalid key
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "result")

  test_results <- data.frame(
    object_id = c("genome001"),
    workflow_id = c("wf001"),
    key = c("invalid_key"),  # Not in key_spec
    value = c("100"),
    unit = c(NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "result", test_results, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user"),
    "Invalid result keys"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("result ingestion allows multiple entries for same object + key", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_multiple_results.xlsx")

  # Add workflows and object
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  test_workflows <- data.frame(
    workflow_id = c("wf_2022", "wf_2025"),
    description = c("GTDB 2022", "GTDB 2025"),
    created_by = c(NA, NA),
    workflow_date = c(NA, NA),
    stringsAsFactors = FALSE
  )
  test_object <- data.frame(
    object_id = "genome001",
    object_type = "genome",
    object_subtype = "MAG",
    label = NA,
    description = NA,
    created_by = NA,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "workflow", test_workflows, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "object", test_object, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Create bundle with same key from different workflows (append-only history)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "result")

  test_results <- data.frame(
    object_id = c("genome001", "genome001"),
    workflow_id = c("wf_2022", "wf_2025"),
    key = c("completeness", "completeness"),  # Same key, different workflows
    value = c("92.1", "95.8"),
    unit = c("%", "%"),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "result", test_results, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should succeed (append-only, multiple values allowed)
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user")

  # Verify both results were inserted
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  inserted_results <- DBI::dbReadTable(con, "result")
  DBI::dbDisconnect(con)

  expect_equal(nrow(inserted_results), 2)
  completeness_results <- inserted_results[inserted_results$key == "completeness", ]
  expect_equal(nrow(completeness_results), 2)

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})
