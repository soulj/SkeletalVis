library(testthat)

test_that("getMetaData handles missing file", {
  skeletalvis_path <- withr::local_tempdir()  # Use a temporary directory for tests

  # Ensure accessions.txt does not exist
  metadata_filepath <- file.path(skeletalvis_path, "accessions.txt")
  if (file.exists(metadata_filepath)) {
    file.remove(metadata_filepath)
  }

  # Expect an error when the file is missing
  expect_error(
    get_comparisons(skeletalvis_path),
    "The file 'accessions.txt' does not exist in the directory"
  )
})

test_that("getMetaData returns correct structure and columns", {
  skeletalvis_path <- withr::local_tempdir()

  # Create mock data for accessions.txt
  mock_data <- data.frame(
    accession = c("E-MTAB-4304", "E-MTAB-4305"),
    comparison = c("1", "2"),
    stringsAsFactors = FALSE
  )

  # Write mock data to accessions.txt
  metadata_filepath <- file.path(skeletalvis_path, "accessions.txt")
  write.table(mock_data, metadata_filepath, sep = "\t", row.names = FALSE, quote = FALSE)

  # Retrieve metadata and check structure
  result <- get_comparisons(skeletalvis_path)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("accession", "comparison", "datasetID"))

  # Verify datasetID column values
  expect_equal(result$datasetID, c("E-MTAB-4304_1", "E-MTAB-4305_2"))
})

test_that("getMetaData includes expected number of rows", {
  skeletalvis_path <- withr::local_tempdir()

  # Create mock data with multiple rows
  mock_data <- data.frame(
    accession = c("E-MTAB-4304", "E-MTAB-4305", "E-MTAB-4306"),
    comparison = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )

  # Write mock data to accessions.txt
  metadata_filepath <- file.path(skeletalvis_path, "accessions.txt")
  write.table(mock_data, metadata_filepath, sep = "\t", row.names = FALSE, quote = FALSE)

  # Retrieve metadata and check the number of rows
  result <- get_comparisons(skeletalvis_path)
  expect_equal(nrow(result), 3)
})
