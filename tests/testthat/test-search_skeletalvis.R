test_that("search in specific columns returns correct results", {
  skeletalvis <- test_path("data","skeletalvis")

  # Search only in the 'Description' column
  result <- search_skeletalvis(skeletalvis, "Cdc73 knockout", columns = "Description")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$ID, "GSE90890")
})

test_that("search term not found returns empty data frame", {
  skeletalvis <- test_path("data","skeletalvis")

  result <- search_skeletalvis(skeletalvis, "NONEXISTENT")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("non-existent columns return an error", {
  skeletalvis <- test_path("data","skeletalvis")

  expect_error(search_skeletalvis(skeletalvis, "SOX9", columns = "NonExistentColumn"),
               "The following columns do not exist in the expTable")
})

test_that("empty search term returns an error", {
  skeletalvis <- test_path("data","skeletalvis")

  expect_error(search_skeletalvis(skeletalvis, ""), "Please provide a valid search term.")
})

test_that("missing expTable.txt file returns an error", {
  skeletalvis <- tempdir()  # Empty temporary directory without the CSV file

  expect_error(search_skeletalvis(skeletalvis, "SOX9"),
               "The file 'expTable.txt' does not exist in the specified directory.")
})

test_that("case-insensitive search works", {
  skeletalvis <- test_path("data","skeletalvis")

  result <- search_skeletalvis(skeletalvis, "chd1")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(result$Perturbation == "CHD1"))
})

test_that("search returns correct rows with multiple matching columns", {
  skeletalvis <- test_path("data","skeletalvis")

  # Searching across both 'GeneSymbol' and 'Description'
  result <- search_skeletalvis(skeletalvis, "Cartilage", columns = c("Perturbation", "Tissue"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
})
