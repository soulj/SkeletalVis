  # Create a temporary directory to hold the self-contained test data
  skeletalvis_path <- withr::local_tempdir()

  # Create mock data
  exp_data <- data.frame(ID = c("ExpA","ExpB"), Description=c("A","B"))
  comparisons <- data.frame(accession=c("ExpA","ExpB"), comparison=c("A","B"), comparisonsText=c("A","B"))

  exp_data_path <- file.path(skeletalvis_path, "expTable.txt")
  comparisons_path <- file.path(skeletalvis_path, "accessions.txt")

  write.table(exp_data, file = exp_data_path, sep = ",", row.names = FALSE)
  write.table(comparisons, file = comparisons_path, sep = "\t", row.names = FALSE)


test_that("search in specific columns returns correct results", {

  # Search only in the 'Description' column
  result <- search_skeletalvis(skeletalvis_path, "A", columns = "Description")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$ID, "ExpA")
})

test_that("search term not found returns empty data frame", {
result <- search_skeletalvis(skeletalvis_path, "NONEXISTENT")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("non-existent columns return an error", {
  expect_error(search_skeletalvis(skeletalvis_path, "GeneA", columns = "NonExistentColumn"),
               "The following columns do not exist in the expTable")
})

test_that("empty search term returns an error", {
  expect_error(search_skeletalvis(skeletalvis_path, ""), "Please provide a valid search term.")
})

test_that("missing expTable.txt file returns an error", {

  # Generate a unique folder name in the system temporary directory
  skeletalvis_empty <- withr::local_tempdir()


  expect_error(search_skeletalvis(skeletalvis_empty, "GeneA"),
               "The file 'expTable.txt' does not exist in the specified directory.")
})

test_that("case-insensitive search works", {

  result <- search_skeletalvis(skeletalvis_path, "expa")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(result$result == "A"))
})
