test_that("read_bundle ingests workflows successfully", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_workflows.xlsx")

  # Create bundle with workflows
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "workflow")

  test_workflows <- data.frame(
    workflow_id = c("wf001", "wf002"),
    description = c("Assembly workflow", "Binning workflow"),
    created_by = c(NA, NA),
    workflow_date = c(NA, NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "workflow", test_workflows, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Ingest bundle
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user")

  # Verify workflows were inserted
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  inserted_workflows <- DBI::dbReadTable(con, "workflow")
  DBI::dbDisconnect(con)

  expect_equal(nrow(inserted_workflows), 2)
  expect_true("wf001" %in% inserted_workflows$workflow_id)
  expect_true("wf002" %in% inserted_workflows$workflow_id)

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("workflow ingestion skips empty rows", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_workflows_empty.xlsx")

  # Create bundle with some empty rows (like write_bundle prefills)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "workflow")

  test_workflows <- data.frame(
    workflow_id = c("wf001", "workflow_0002", "workflow_0003"),
    description = c("Real workflow", "", NA),  # Last two are empty
    created_by = c(NA, NA, NA),
    workflow_date = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "workflow", test_workflows, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Ingest - should only process 1
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user")

  # Verify only 1 workflow inserted
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  inserted_workflows <- DBI::dbReadTable(con, "workflow")
  DBI::dbDisconnect(con)

  expect_equal(nrow(inserted_workflows), 1)
  expect_equal(inserted_workflows$workflow_id[1], "wf001")

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("workflow ingestion auto-creates people", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_workflows_people.xlsx")

  # Create bundle with created_by
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "workflow")

  test_workflows <- data.frame(
    workflow_id = c("wf001", "wf002"),
    description = c("Workflow 1", "Workflow 2"),
    created_by = c("alice", "bob"),
    workflow_date = c(NA, NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "workflow", test_workflows, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Ingest
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user")

  # Verify people were auto-created
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  people <- DBI::dbReadTable(con, "people")
  DBI::dbDisconnect(con)

  expect_equal(nrow(people), 2)
  expect_true("alice" %in% people$person_id)
  expect_true("bob" %in% people$person_id)
  expect_equal(people$is_active, c(1, 1))

  # Check that result includes auto-created people
  expect_true(length(result$results$auto_created_people) == 2)
  expect_true("alice" %in% result$results$auto_created_people)

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("workflow validation detects duplicate IDs", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_dup_workflows.xlsx")

  # Create bundle with duplicate workflow_id
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "workflow")

  test_workflows <- data.frame(
    workflow_id = c("wf001", "wf001"),  # Duplicate!
    description = c("Workflow 1", "Workflow 2"),
    created_by = c(NA, NA),
    workflow_date = c(NA, NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "workflow", test_workflows, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user"),
    "Duplicate workflow_ids"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("workflow validation detects existing IDs in database", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_existing_workflow.xlsx")

  # Add existing workflow
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  existing_wf <- data.frame(
    workflow_id = "existing_wf",
    description = "Already exists",
    created_by = NA,
    workflow_date = NA,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "workflow", existing_wf, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Try to add workflow with same ID
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "workflow")

  test_workflows <- data.frame(
    workflow_id = c("existing_wf"),
    description = c("New workflow"),
    created_by = c(NA),
    workflow_date = c(NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "workflow", test_workflows, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user"),
    "Workflow IDs already exist in database"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("edges can reference workflows from same bundle", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_workflow_edge_ref.xlsx")

  # Create bundle with workflow, objects, and edge
  wb <- openxlsx::createWorkbook()

  # Workflow
  openxlsx::addWorksheet(wb, "workflow")
  workflows <- data.frame(
    workflow_id = c("wf001"),
    description = c("Assembly"),
    created_by = c(NA),
    workflow_date = c(NA),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "workflow", workflows, startRow = 1, colNames = TRUE)

  # Objects
  openxlsx::addWorksheet(wb, "object")
  objects <- data.frame(
    object_id = c("read1", "asm1"),
    object_type = c("readset:paired_end", "assembly:metagenome"),
    label = c(NA, NA),
    description = c(NA, NA),
    created_by = c(NA, NA),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "object", objects, startRow = 1, colNames = TRUE)

  # Edge referencing the workflow
  openxlsx::addWorksheet(wb, "edge")
  edges <- data.frame(
    parent_id = c("read1"),
    child_id = c("asm1"),
    edge_type = c("assembled_from"),
    workflow_id = c("wf001"),  # References workflow in bundle
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "edge", edges, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should succeed
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user")

  # Verify edge has workflow_id
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  inserted_edges <- DBI::dbReadTable(con, "edge")
  DBI::dbDisconnect(con)

  expect_equal(nrow(inserted_edges), 1)
  expect_equal(inserted_edges$workflow_id[1], "wf001")

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})
