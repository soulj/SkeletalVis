library(testthat)
library(dplyr)
library(arrow)

test_that("get_experiment handles missing files", {

  # Make a random temp directory
  skeletalvis_path <- tempfile(pattern=paste0(sample(LETTERS, 8, replace = TRUE), collapse = ""))  # Use a temporary directory for tests
  dir.create(skeletalvis_path)

  # Test for missing foldChangeTable.feather file
  expect_error(
    get_experiment(skeletalvis_path, "E-MTAB-4304_1"),
    "The file 'foldChangeTable.feather' does not exist in the specified directory"
  )

  # Create an empty feather file to simulate the existence of foldChangeTable.feather
  foldchange_path <- file.path(skeletalvis_path, "foldChangeTable.feather")
  arrow::write_feather(data.frame(ID=LETTERS[1:3],"E-MTAB-4304_1" = c(1,2,3),check.names = FALSE), foldchange_path)

  # Test for missing pvalTable.feather file
  expect_error(
    get_experiment(skeletalvis_path, "E-MTAB-4304_1"),
    "The file 'pvalTable.feather' does not exist in the specified directory:"
  )
})

test_that("get_experiment handles missing ID column in Feather files", {
  skeletalvis_path <- tempdir()

  # Create empty feather files without an ID column
  foldchange_data <- data.frame(Sample1 = numeric(), Sample2 = numeric())
  pvalue_data <- data.frame(GeneName = character(), Pval1 = numeric(), Pval2 = numeric())

  foldchange_path <- file.path(skeletalvis_path, "foldChangeTable.feather")
  pvalue_path <- file.path(skeletalvis_path, "pvalTable.feather")

  arrow::write_feather(foldchange_data, foldchange_path)
  arrow::write_feather(pvalue_data, pvalue_path)

  # Expect error about missing ID column
  expect_error(
    get_experiment(skeletalvis_path, "E-MTAB-4304_1"),
    "The Feather file does not contain an 'ID' column."
  )
})

test_that("get_experiment returns expected structure and values", {
  skeletalvis_path <- tempdir()

  # Create mock data for feather files
  foldchange_data <- data.frame(ID = c("GeneA", "GeneB"), `E-MTAB-4304_1` = c(1.5, -2.3),check.names = FALSE)
  pvalue_data <- data.frame(GeneName = c("GeneA", "GeneB"), `E-MTAB-4304_1` = c(0.05, 0.01),check.names = FALSE)

  foldchange_path <- file.path(skeletalvis_path, "foldChangeTable.feather")
  pvalue_path <- file.path(skeletalvis_path, "pvalTable.feather")

  arrow::write_feather(foldchange_data, foldchange_path)
  arrow::write_feather(pvalue_data, pvalue_path)

  # Check that the function returns the expected data frame structure with 'data.frame' output type
  result <- get_experiment(skeletalvis_path, "E-MTAB-4304_1")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("ID", "E-MTAB-4304_1_log2FoldChange", "E-MTAB-4304_1_FDR"))

  # Check that the result values match the mock data
  expect_equal(result$ID, c("GeneA", "GeneB"))
  expect_equal(result$`E-MTAB-4304_1_log2FoldChange`, c(1.5, -2.3))
  expect_equal(result$`E-MTAB-4304_1_FDR`, c(0.05, 0.01))
})

test_that("get_experiment returns error for dataset ID not found", {
  skeletalvis_path <- tempdir()

  # Create mock data for feather files with a different dataset ID
  foldchange_data <- data.frame(ID = c("GeneA", "GeneB"), `E-MTAB-4304_2` = c(1.5, -2.3),check.names = FALSE)
  pvalue_data <- data.frame(GeneName = c("GeneA", "GeneB"), `E-MTAB-4304_2` = c(0.05, 0.01), check.names = FALSE)

  foldchange_path <- file.path(skeletalvis_path, "foldChangeTable.feather")
  pvalue_path <- file.path(skeletalvis_path, "pvalTable.feather")

  arrow::write_feather(foldchange_data, foldchange_path)
  arrow::write_feather(pvalue_data, pvalue_path)

  # Expect error for dataset ID not present in feather files
  expect_error(
    get_experiment(skeletalvis_path, "E-MTAB-4304_1"),
    "No results found for the dataset id: E-MTAB-4304_1"
  )
})
