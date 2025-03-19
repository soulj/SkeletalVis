test_that("view_oagenes reads file correctly", {
  # Create a temporary directory
  temp_dir <-  withr::local_tempdir()

  # Define a temporary file path for oagenes.csv
  temp_file <- file.path(temp_dir, "oatargets.txt")

  # Create mock data
  test_data <- data.frame(
    PMID = c(123456, 789012),
    Gene = c("GeneA", "GeneB"),
    `Effect on gene product` = c("Increase", "Decrease"),
    Model = c("OA Model 1", "OA Model 2"),
    `Susceptibility observed` = c("Protective", "Detrimental"),
    `Inferred gene effect` = c("Protective", "Detrimental"),
    Delivery = c("Joint", "Cartilage"),
    Species = c("Mouse", "Human"),
    pub_date = c("2020-05-01", "2021-06-15"),
    LastAuthor = c("Smith", "Doe"),
    Type = c("Genetic", "Exogenous"),
    Intervention = c("Knockout", "Overexpression"),
    simpleModel = c("Spontaneous", "Surgical"),
    effectConsensus = c("Protective", "Ambiguous"),
    NumStudies = c(5, 3)
  )

  # Write the mock data to CSV
  write.table(test_data, temp_file, sep = "\t", row.names = FALSE, quote = FALSE)

  # Run the function
  result <- view_oagenes(temp_dir)

  # Check that result is a dataframe
  expect_s3_class(result, "data.frame")

  # Check that the column names match expected names
  expected_columns <- colnames(test_data)
  expect_equal(colnames(result), expected_columns)

  # Clean up temporary file
  unlink(temp_file)
})

test_that("view_oagenes throws an error if file does not exist", {
  expect_error(view_oagenes(tempdir()), "The file 'oatargets.txt' does not exist")
})
