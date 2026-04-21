test_that("valid objects pass validation", {
  # Setup
  db_path <- create_test_db()
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Create valid test data
  test_objects <- data.frame(
    object_id = c("obj1", "obj2", "obj3"),
    object_type = c("genome", "readset", "assembly"),
    object_subtype = c("MAG", "paired_end", "metagenome"),
    label = c(NA, NA, NA),
    description = c(NA, NA, NA),
    created_by = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  # Test
  result <- validate_objects_with_con(test_objects, con)

  # Assert
  expect_true(result$valid)
  expect_equal(result$message, "All validations passed")

  # Cleanup
  DBI::dbDisconnect(con)
  unlink(db_path)
})


test_that("invalid object type fails validation", {
  # Setup
  db_path <- create_test_db()
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Create test data with invalid type
  test_objects <- data.frame(
    object_id = c("obj1"),
    object_type = c("invalid_type"),
    object_subtype = c(NA),
    label = c(NA),
    description = c(NA),
    created_by = c(NA),
    stringsAsFactors = FALSE
  )

  # Test
  result <- validate_objects_with_con(test_objects, con)

  # Assert
  expect_false(result$valid)
  expect_match(result$message, "Invalid object types")
  expect_match(result$message, "invalid_type")

  # Cleanup
  DBI::dbDisconnect(con)
  unlink(db_path)
})


test_that("invalid object subtype fails validation", {
  # Setup
  db_path <- create_test_db()
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Create test data with invalid subtype
  test_objects <- data.frame(
    object_id = c("obj1"),
    object_type = c("genome"),
    object_subtype = c("invalid_subtype"),
    label = c(NA),
    description = c(NA),
    created_by = c(NA),
    stringsAsFactors = FALSE
  )

  # Test
  result <- validate_objects_with_con(test_objects, con)

  # Assert
  expect_false(result$valid)
  expect_match(result$message, "Invalid subtype")
  expect_match(result$message, "invalid_subtype")
  expect_match(result$message, "genome")

  # Cleanup
  DBI::dbDisconnect(con)
  unlink(db_path)
})


test_that("duplicate object_ids in bundle fail validation", {
  # Setup
  db_path <- create_test_db()
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Create test data with duplicate IDs
  test_objects <- data.frame(
    object_id = c("obj1", "obj1", "obj2"),
    object_type = c("genome", "genome", "readset"),
    object_subtype = c("MAG", "isolate", "paired_end"),
    label = c(NA, NA, NA),
    description = c(NA, NA, NA),
    created_by = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  # Test
  result <- validate_objects_with_con(test_objects, con)

  # Assert
  expect_false(result$valid)
  expect_match(result$message, "Duplicate object_ids in bundle")
  expect_match(result$message, "obj1")

  # Cleanup
  DBI::dbDisconnect(con)
  unlink(db_path)
})


test_that("duplicate object_ids against database fail validation", {
  # Setup
  db_path <- create_test_db()

  # Add an existing object to the database
  existing_object <- data.frame(
    object_id = "existing_obj",
    object_type = "genome",
    object_subtype = "MAG",
    label = NA,
    description = NA,
    created_by = NA,
    stringsAsFactors = FALSE
  )
  add_test_objects(db_path, existing_object)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Try to add object with same ID
  test_objects <- data.frame(
    object_id = c("existing_obj", "new_obj"),
    object_type = c("genome", "readset"),
    object_subtype = c("isolate", "paired_end"),
    label = c(NA, NA),
    description = c(NA, NA),
    created_by = c(NA, NA),
    stringsAsFactors = FALSE
  )

  # Test
  result <- validate_objects_with_con(test_objects, con)

  # Assert
  expect_false(result$valid)
  expect_match(result$message, "Object IDs already exist in database")
  expect_match(result$message, "existing_obj")

  # Cleanup
  DBI::dbDisconnect(con)
  unlink(db_path)
})


test_that("objects without subtypes are valid", {
  # Setup
  db_path <- create_test_db()
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Create test data without subtypes
  test_objects <- data.frame(
    object_id = c("obj1", "obj2"),
    object_type = c("study", "site"),
    object_subtype = c(NA, NA),
    label = c(NA, NA),
    description = c(NA, NA),
    created_by = c(NA, NA),
    stringsAsFactors = FALSE
  )

  # Test
  result <- validate_objects_with_con(test_objects, con)

  # Assert
  expect_true(result$valid)
  expect_equal(result$message, "All validations passed")

  # Cleanup
  DBI::dbDisconnect(con)
  unlink(db_path)
})
