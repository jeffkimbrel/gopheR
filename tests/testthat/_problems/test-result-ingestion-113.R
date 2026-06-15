# Extracted from test-result-ingestion.R:113

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gopheR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
test_dir <- tempdir()
db_info <- create_test_db(path = test_dir, return_full_path = FALSE)
setup_test_env(db_info)
bundle_path <- file.path(test_dir, "test_invalid_key.xlsx")
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
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "object_result")
test_results <- data.frame(
    object_id = c("genome001"),
    workflow_id = c("wf001"),
    key = c("invalid_key"),  # Not in object_result_spec
    value = c("100"),
    unit = c(NA),
    stringsAsFactors = FALSE
  )
openxlsx::writeData(wb, "object_result", test_results, startRow = 1, colNames = TRUE)
openxlsx::saveWorkbook(wb, bundle_path, overwrite = TRUE)
expect_error(
    read_bundle(bundle_path, validate_only = FALSE, backup = FALSE, default_user = "test_user"),
    "Invalid result keys"
  )
