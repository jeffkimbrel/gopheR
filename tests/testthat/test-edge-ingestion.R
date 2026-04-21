test_that("read_bundle ingests edges successfully", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_edges.xlsx")

  # First add some objects
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  test_objects <- data.frame(
    object_id = c("read1", "read2", "asm1"),
    object_type = c("readset", "readset", "assembly"),
    object_subtype = c("paired_end", "paired_end", "metagenome"),
    label = c(NA, NA, NA),
    description = c(NA, NA, NA),
    created_by = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "object", test_objects, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Create bundle with edges
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "edge")

  test_edges <- data.frame(
    parent_id = c("read1", "read2"),
    child_id = c("asm1", "asm1"),
    edge_type = c("assembled_from", "assembled_from"),
    workflow_id = c(NA, NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "edge", test_edges, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Ingest bundle
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE)

  # Verify edges were inserted
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  inserted_edges <- DBI::dbReadTable(con, "edge")
  DBI::dbDisconnect(con)

  expect_equal(nrow(inserted_edges), 2)
  expect_true("read1" %in% inserted_edges$parent_id)
  expect_true("asm1" %in% inserted_edges$child_id)

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("edges can reference newly inserted objects", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_new_objects_edges.xlsx")

  # Create bundle with BOTH objects and edges
  wb <- openxlsx::createWorkbook()

  # Add object sheet
  openxlsx::addWorksheet(wb, "object")
  test_objects <- data.frame(
    object_id = c("read1", "asm1"),
    object_type = c("readset:paired_end", "assembly:metagenome"),
    label = c(NA, NA),
    description = c(NA, NA),
    created_by = c(NA, NA),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "object", test_objects, startRow = 1, colNames = TRUE)

  # Add edge sheet referencing the new objects
  openxlsx::addWorksheet(wb, "edge")
  test_edges <- data.frame(
    parent_id = c("read1"),
    child_id = c("asm1"),
    edge_type = c("assembled_from"),
    workflow_id = c(NA),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "edge", test_edges, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Ingest bundle (objects first, then edges in same transaction)
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE)

  # Verify both were inserted
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  objects <- DBI::dbReadTable(con, "object")
  edges <- DBI::dbReadTable(con, "edge")
  DBI::dbDisconnect(con)

  expect_equal(nrow(objects), 2)
  expect_equal(nrow(edges), 1)
  expect_equal(edges$parent_id[1], "read1")
  expect_equal(edges$child_id[1], "asm1")

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("edge validation fails with missing parent", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_missing_parent.xlsx")

  # Add one object
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  test_objects <- data.frame(
    object_id = c("asm1"),
    object_type = c("assembly"),
    object_subtype = c("metagenome"),
    label = c(NA),
    description = c(NA),
    created_by = c(NA),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "object", test_objects, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Create edge referencing non-existent parent
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "edge")
  test_edges <- data.frame(
    parent_id = c("nonexistent"),
    child_id = c("asm1"),
    edge_type = c("assembled_from"),
    workflow_id = c(NA),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "edge", test_edges, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE),
    "Parent IDs not found"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("edge validation fails with invalid edge type", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_invalid_edge_type.xlsx")

  # Add objects
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  test_objects <- data.frame(
    object_id = c("read1", "asm1"),
    object_type = c("readset", "assembly"),
    object_subtype = c("paired_end", "metagenome"),
    label = c(NA, NA),
    description = c(NA, NA),
    created_by = c(NA, NA),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "object", test_objects, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Create edge with invalid type
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "edge")
  test_edges <- data.frame(
    parent_id = c("read1"),
    child_id = c("asm1"),
    edge_type = c("invalid_edge"),
    workflow_id = c(NA),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "edge", test_edges, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE),
    "Invalid edge types"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("edge validation fails with incompatible object types", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_incompatible_types.xlsx")

  # Add objects of incompatible types
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  test_objects <- data.frame(
    object_id = c("genome1", "asm1"),
    object_type = c("genome", "assembly"),
    object_subtype = c("MAG", "metagenome"),
    label = c(NA, NA),
    description = c(NA, NA),
    created_by = c(NA, NA),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "object", test_objects, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Try to create assembled_from edge with genome as parent (should be readset)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "edge")
  test_edges <- data.frame(
    parent_id = c("genome1"),
    child_id = c("asm1"),
    edge_type = c("assembled_from"),
    workflow_id = c(NA),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "edge", test_edges, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE),
    "Invalid edge combination"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("edge validation detects duplicates", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_dup_edges.xlsx")

  # Add objects
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  test_objects <- data.frame(
    object_id = c("read1", "asm1"),
    object_type = c("readset", "assembly"),
    object_subtype = c("paired_end", "metagenome"),
    label = c(NA, NA),
    description = c(NA, NA),
    created_by = c(NA, NA),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "object", test_objects, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Create bundle with duplicate edge
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "edge")
  test_edges <- data.frame(
    parent_id = c("read1", "read1"),
    child_id = c("asm1", "asm1"),
    edge_type = c("assembled_from", "assembled_from"),
    workflow_id = c(NA, NA),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "edge", test_edges, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE),
    "Duplicate edges"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})
