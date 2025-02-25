
test_that("plot_similarity handles missing columns", {
  # Create a data frame missing the 'zscore' column
  similarity_table_missing_zscore <- data.frame(
    ID = c("sample1", "sample2", "sample3"),
    cosine = c(0.8, 0.6, 0.4)
  )

  # Expect an error due to missing 'zscore' column
  expect_error(
    plot_similarity(similarity_table_missing_zscore),
    "The similarity table must contain the following columns: 'datasetID', 'cosine', and 'zscore'."
  )
})

test_that("plot_similarity handles invalid top_n inputs", {
  # Create a valid similarity table
  similarity_table <- data.frame(
    datasetID = c("sample1", "sample2", "sample3"),
    cosine = c(0.8, 0.6, 0.4),
    zscore = c(2.5, 1.8, 0.5)
  )

  # Expect an error when top_n is not a positive integer
  expect_error(plot_similarity(similarity_table, top_n = -1), "The 'top_n' parameter must be a positive integer.")
  expect_error(plot_similarity(similarity_table, top_n = "three"), "The 'top_n' parameter must be a positive integer.")
})

test_that("plot_similarity returns a ggplot object for valid inputs", {
  # Create a valid similarity table
  similarity_table <- data.frame(
    datasetID = c("sample1", "sample2", "sample3"),
    cosine = c(0.8, 0.6, 0.4),
    zscore = c(2.5, 1.8, 0.5)
  )

  # Generate the plot and check that it's a ggplot object
  p <- plot_similarity(similarity_table, top_n = 2)
  expect_s3_class(p, "ggplot")
})

test_that("plot_similarity correctly labels top N items", {
  # Create a valid similarity table with several entries
  similarity_table <- data.frame(
    datasetID = paste0("sample", 1:10),
    cosine = runif(10, 0, 1),
    zscore = runif(10, 0, 5)
  )

  # Generate the plot with top_n = 3 and extract the labels
  p <- plot_similarity(similarity_table, top_n = 3)
  data_used <- ggplot2::ggplot_build(p)$data[[2]]

  # Check if there are exactly 3 labels
  expect_equal(nrow(data_used), 3)
})
