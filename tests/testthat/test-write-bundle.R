test_that("write_bundle creates Excel file with required sheets", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  out_xlsx <- file.path(test_dir, "test_bundle.xlsx")

  # Write bundle
  result <- write_bundle(out_xlsx, open_bundle = FALSE)

  # Assert file was created
  expect_true(file.exists(out_xlsx))

  # Check sheets exist
  wb <- openxlsx::loadWorkbook(out_xlsx)
  sheet_names <- names(wb)

  expect_true("spec" %in% sheet_names)
  expect_true("object" %in% sheet_names)
  expect_true("workflow" %in% sheet_names)
  expect_true("edge" %in% sheet_names)

  # Cleanup
  unlink(out_xlsx)
  cleanup_test_env(db_info)
})


test_that("write_bundle object sheet has correct columns", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  out_xlsx <- file.path(test_dir, "test_bundle2.xlsx")

  # Write bundle
  write_bundle(out_xlsx, open_bundle = FALSE)

  # Read object sheet
  object_data <- openxlsx::read.xlsx(out_xlsx, sheet = "object")

  # Check essential columns
  expect_true("object_id" %in% names(object_data))
  expect_true("object_type" %in% names(object_data))

  # Cleanup
  unlink(out_xlsx)
  cleanup_test_env(db_info)
})
