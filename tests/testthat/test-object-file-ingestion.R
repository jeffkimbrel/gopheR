test_that("read_bundle ingests object_files successfully", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_object_files.xlsx")

  # First add workflow and objects
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  test_workflow <- data.frame(
    workflow_id = "wf001",
    description = "Test workflow",
    created_by = NA,
    workflow_date = NA,
    stringsAsFactors = FALSE
  )
  test_objects <- data.frame(
    object_id = c("genome001", "readset001"),
    object_type = c("genome", "readset"),
    object_subtype = c("MAG", "paired_end"),
    label = c(NA, NA),
    description = c(NA, NA),
    created_by = c(NA, NA),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "workflow", test_workflow, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "object", test_objects, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Create bundle with object_files
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "object_file")

  test_files <- data.frame(
    object_id = c("genome001", "readset001", "readset001"),
    file_role = c("genome_fasta", "fastq_r1", "fastq_r2"),
    file_path = c("/data/genome001.fasta", "/data/readset001_R1.fastq", "/data/readset001_R2.fastq"),
    file_format = c("fasta", "fastq", "fastq"),
    workflow_id = c("wf001", "wf001", "wf001"),
    checksum = c("abc123", "def456", "ghi789"),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "object_file", test_files, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Ingest bundle
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user")

  # Verify files were inserted
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  inserted_files <- DBI::dbReadTable(con, "object_file")
  DBI::dbDisconnect(con)

  expect_equal(nrow(inserted_files), 3)
  expect_true("genome_fasta" %in% inserted_files$file_role)
  expect_true("fastq_r1" %in% inserted_files$file_role)

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("object_file ingestion detects invalid file_role for object_type", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_invalid_file_role.xlsx")

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

  # Create bundle with invalid file_role for genome (using readset role)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "object_file")

  test_files <- data.frame(
    object_id = c("genome001"),
    file_role = c("fastq_r1"),  # This is for readsets, not genomes!
    file_path = c("/data/genome001.fastq"),
    file_format = c("fastq"),
    workflow_id = c("wf001"),
    checksum = c(NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "object_file", test_files, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user"),
    "Invalid file_role 'fastq_r1' for object_type 'genome'"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("object_file ingestion detects duplicate file_path", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_dup_file_path.xlsx")

  # Add workflow and objects
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  test_workflow <- data.frame(
    workflow_id = "wf001",
    description = "Test workflow",
    created_by = NA,
    workflow_date = NA,
    stringsAsFactors = FALSE
  )
  test_objects <- data.frame(
    object_id = c("genome001", "genome002"),
    object_type = c("genome", "genome"),
    object_subtype = c("MAG", "MAG"),
    label = c(NA, NA),
    description = c(NA, NA),
    created_by = c(NA, NA),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "workflow", test_workflow, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "object", test_objects, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Create bundle with duplicate file_path
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "object_file")

  test_files <- data.frame(
    object_id = c("genome001", "genome002"),
    file_role = c("genome_fasta", "genome_fasta"),
    file_path = c("/data/genome.fasta", "/data/genome.fasta"),  # Duplicate!
    file_format = c("fasta", "fasta"),
    workflow_id = c("wf001", "wf001"),
    checksum = c(NA, NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "object_file", test_files, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user"),
    "Duplicate file_path"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("object_file ingestion detects file_path already in database", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_existing_file_path.xlsx")

  # Add workflow, object, and existing file
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
  existing_file <- data.frame(
    object_id = "genome001",
    file_role = "genome_fasta",
    file_path = "/data/existing.fasta",
    file_format = "fasta",
    workflow_id = "wf001",
    checksum = NA,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "workflow", test_workflow, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "object", test_object, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "object_file", existing_file, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Try to add file with same path
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "object_file")

  test_files <- data.frame(
    object_id = c("genome001"),
    file_role = c("protein_fasta"),
    file_path = c("/data/existing.fasta"),  # Already in DB!
    file_format = c("fasta"),
    workflow_id = c("wf001"),
    checksum = c(NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "object_file", test_files, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user"),
    "Duplicate file_path already in database"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})
