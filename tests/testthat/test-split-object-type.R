test_that("split_object_type splits combined types correctly", {
  # Create test data with combined type:subtype
  test_df <- data.frame(
    object_id = c("obj1", "obj2", "obj3"),
    object_type = c("genome:MAG", "readset:paired_end", "assembly:metagenome"),
    stringsAsFactors = FALSE
  )

  result <- split_object_type(test_df)

  expect_equal(result$object_type, c("genome", "readset", "assembly"))
  expect_equal(result$object_subtype, c("MAG", "paired_end", "metagenome"))
})


test_that("split_object_type handles types without subtypes", {
  # Create test data without subtypes
  test_df <- data.frame(
    object_id = c("obj1", "obj2"),
    object_type = c("study", "site"),
    stringsAsFactors = FALSE
  )

  result <- split_object_type(test_df)

  expect_equal(result$object_type, c("study", "site"))
  expect_equal(result$object_subtype, c(NA_character_, NA_character_))
})


test_that("split_object_type handles mixed data", {
  # Mix of with and without subtypes
  test_df <- data.frame(
    object_id = c("obj1", "obj2", "obj3"),
    object_type = c("genome:MAG", "study", "readset:paired_end"),
    stringsAsFactors = FALSE
  )

  result <- split_object_type(test_df)

  expect_equal(result$object_type, c("genome", "study", "readset"))
  expect_equal(result$object_subtype, c("MAG", NA_character_, "paired_end"))
})


test_that("split_object_type returns unchanged if no object_type column", {
  # Data without object_type column
  test_df <- data.frame(
    object_id = c("obj1", "obj2"),
    some_other_column = c("a", "b"),
    stringsAsFactors = FALSE
  )

  result <- split_object_type(test_df)

  expect_equal(result, test_df)
})


test_that("split_object_type preserves other columns", {
  # Data with multiple columns
  test_df <- data.frame(
    object_id = c("obj1", "obj2"),
    object_type = c("genome:MAG", "readset:paired_end"),
    label = c("Genome 1", "Readset 1"),
    description = c("A genome", "A readset"),
    stringsAsFactors = FALSE
  )

  result <- split_object_type(test_df)

  expect_equal(result$object_id, c("obj1", "obj2"))
  expect_equal(result$label, c("Genome 1", "Readset 1"))
  expect_equal(result$description, c("A genome", "A readset"))
  expect_equal(result$object_type, c("genome", "readset"))
  expect_equal(result$object_subtype, c("MAG", "paired_end"))
})
