test_that("read_bundle ingests people successfully", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_people.xlsx")

  # Create bundle with people
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "people")

  test_people <- data.frame(
    person_id = c("alice", "bob"),
    full_name = c("Alice Smith", "Bob Jones"),
    email = c("alice@example.com", "bob@example.com"),
    successor_person_id = c(NA, NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "people", test_people, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Ingest bundle
  result <- read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user")

  # Verify people were inserted
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  inserted_people <- DBI::dbReadTable(con, "people")
  DBI::dbDisconnect(con)

  expect_equal(nrow(inserted_people), 2)
  expect_true("alice" %in% inserted_people$person_id)
  expect_true("bob" %in% inserted_people$person_id)
  expect_equal(inserted_people$is_active, c(1, 1))  # Should default to 1

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("people ingestion detects duplicate person_ids in bundle", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_dup_people.xlsx")

  # Create bundle with duplicate person_id
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "people")

  test_people <- data.frame(
    person_id = c("alice", "alice"),  # Duplicate!
    full_name = c("Alice Smith", "Alice Jones"),
    email = c("alice@example.com", "alice2@example.com"),
    successor_person_id = c(NA, NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "people", test_people, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user"),
    "Duplicate person_ids"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("people ingestion detects existing person_ids in database", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_existing_person.xlsx")

  # Add existing person to database
  con <- DBI::dbConnect(RSQLite::SQLite(), db_info$full_path)
  existing_person <- data.frame(
    person_id = "alice",
    full_name = "Alice Existing",
    email = "alice@existing.com",
    is_active = 1,
    successor_person_id = NA,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "people", existing_person, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)

  # Try to add person with same person_id
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "people")

  test_people <- data.frame(
    person_id = c("alice"),
    full_name = c("Alice New"),
    email = c("alice@new.com"),
    successor_person_id = c(NA),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "people", test_people, startRow = 1, colNames = TRUE)
  openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)

  # Should fail
  expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user"),
    "Person IDs already exist in database"
  )

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("write_bundle creates people sheet when requested", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_people_sheet.xlsx")

  # Create bundle with people_sheet = TRUE (uses options set by setup_test_env)
  write_bundle(bundle_path, people_sheet = TRUE, overwrite = TRUE)

  # Verify people sheet exists and has correct columns
  wb <- openxlsx::loadWorkbook(bundle_path)
  expect_true("people" %in% names(wb))

  people_data <- openxlsx::readWorkbook(wb, sheet = "people")
  expect_true("person_id" %in% names(people_data))
  expect_true("full_name" %in% names(people_data))
  expect_true("email" %in% names(people_data))
  expect_true("successor_person_id" %in% names(people_data))
  expect_false("is_active" %in% names(people_data))  # Should be excluded
  expect_false("created_at" %in% names(people_data))  # Should be excluded

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("write_bundle excludes people sheet by default", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_no_people_sheet.xlsx")

  # Create bundle without people_sheet parameter (defaults to FALSE, uses options)
  write_bundle(bundle_path, overwrite = TRUE)

  # Verify people sheet does NOT exist
  wb <- openxlsx::loadWorkbook(bundle_path)
  expect_false("people" %in% names(wb))

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})
