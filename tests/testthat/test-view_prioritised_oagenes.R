test_that("view_priortised_oagenes reads file correctly", {
  # Create a temporary directory
  temp_dir <-  withr::local_tempdir()

  # Define a temporary file path for oagenes.csv
  temp_file <- file.path(temp_dir, "oatargets.txt")

  # Create mock data
  test_data <- data.frame(Gene = c("IER3", "SOCS3"),
                          Rank = c(1, 2),
                          PredictedEffect = c("Detrimental","Detrimental"),
                          Category_sm = c("Unknown", "Unknown"),
                          Category_ab = c("Predicted_Tractable_ab_Medium_to_low_confidence",
                                          "Predicted_Tractable_ab_Medium_to_low_confidence"))

  # Write the mock data
  write.table(test_data, temp_file, sep = "\t", row.names = FALSE, quote = FALSE)

  # Run the function
  result <- view_curated_oagenes(temp_dir)

  # Check that result is a dataframe
  expect_s3_class(result, "data.frame")

  # Check that the column names match expected names
  expected_columns <- colnames(test_data)
  expect_equal(colnames(result), expected_columns)

})

test_that("view_priortised_oagenes throws an error if file does not exist", {
  expect_error(view_curated_oagenes(tempdir()), "The file 'oatargets.txt' does not exist")
})
