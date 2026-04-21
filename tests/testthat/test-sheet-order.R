test_that("write_bundle creates sheets in logical order with people_sheet", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_sheet_order.xlsx")

  # Create bundle with people_sheet = TRUE
  write_bundle(bundle_path, people_sheet = TRUE, overwrite = TRUE)

  # Read sheet names
  wb <- openxlsx::loadWorkbook(bundle_path)
  sheet_names <- names(wb)

  # Verify order (excluding spec which is hidden)
  visible_sheets <- setdiff(sheet_names, "spec")
  expect_equal(visible_sheets[1], "people")
  expect_equal(visible_sheets[2], "workflow")
  expect_equal(visible_sheets[3], "object")
  expect_equal(visible_sheets[4], "edge")
  expect_equal(visible_sheets[5], "result")
  expect_equal(visible_sheets[6], "object_file")

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})


test_that("write_bundle creates sheets in logical order without people_sheet", {
  # Setup
  test_dir <- tempdir()
  db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
  setup_test_env(db_info)
  bundle_path <- file.path(test_dir, "test_sheet_order_no_people.xlsx")

  # Create bundle without people_sheet
  write_bundle(bundle_path, people_sheet = FALSE, overwrite = TRUE)

  # Read sheet names
  wb <- openxlsx::loadWorkbook(bundle_path)
  sheet_names <- names(wb)

  # Verify order (excluding spec which is hidden)
  visible_sheets <- setdiff(sheet_names, "spec")
  expect_equal(visible_sheets[1], "workflow")
  expect_equal(visible_sheets[2], "object")
  expect_equal(visible_sheets[3], "edge")
  expect_equal(visible_sheets[4], "result")
  expect_equal(visible_sheets[5], "object_file")
  expect_false("people" %in% visible_sheets)

  # Cleanup
  unlink(bundle_path)
  cleanup_test_env(db_info)
})
