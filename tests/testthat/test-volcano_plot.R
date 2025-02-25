test_that("volcano_plot generates a ggplot object", {
  data <- data.frame(
    GeneID = c("gene1", "gene2", "gene3", "gene4"),
     log2FoldChange = c(2, -3, 0.5, 1.2),
    p_value = c(0.001, 0.05, 0.2, 0.03)
  )

  plot <- volcano_plot( data = data )

  expect_s3_class(plot, "ggplot")
})

test_that("volcano_plot applies fold change and p-value thresholds", {
  data <- data.frame(
    GeneID = c("gene1", "gene2", "gene3", "gene4"),
    log2FoldChange = c(2, -3, 0.5, 1.2),
    p_value = c(0.001, 0.05, 0.2, 0.03)
  )

  plot <- volcano_plot( data = data,
    logFC_threshold = 1,
    FDR_threshold = 0.05
  )

  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(sum(plot_data$colour == "dodgerblue3"), 1)  # Down-regulated
  expect_equal(sum(plot_data$colour == "firebrick3"), 2)  # Up-regulated
})

test_that("volcano_plot handles data with no significant points", {
  data <- data.frame(
    GeneID = c("gene1", "gene2", "gene3", "gene4"),
     log2FoldChange = c(2, -3, 0.5, 1.2),
    p_value = c(0.1, 0.5, 0.2, 0.3)
  )
  plot <- volcano_plot(
    data = data,
    logFC_threshold = 1,
    FDR_threshold = 0.05
  )

  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(sum(plot_data$colour == "dodgerblue3"), 0)  # No up-regulated points
  expect_equal(sum(plot_data$colour == "firebrick3"), 0)  # No down-regulated points
})

test_that("volcano_plot assigns custom point size and label size", {
  data <- data.frame(
    GeneID = c("gene1", "gene2", "gene3"),
    log2FoldChange = c(1.5, -2, 0),
    p_value = c(0.01, 0.02, 0.03)
  )

  plot <- volcano_plot(
    data = data,
    point_size = 4,
    lab_size = 6
  )

  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(unique(plot_data$size), 4)
})

test_that("volcano_plot handles Inf and NA values in p-values", {
  data <- data.frame(
    GeneID = c("gene1", "gene2", "gene3"),
    log2FoldChange = c(2, -1.5, 1.2),
    p_value = c(0.001, Inf, NA)
  )

  plot <- volcano_plot(
    data = data
  )

  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_true(all(is.finite(plot_data$y)))
  expect_false(any(is.infinite(plot_data$y)))
})

test_that("volcano_plot throws error for missing required columns", {
  data <- data.frame(
    log2FoldChange = c(1.5, -2, 0),
    p_value = c(0.01, 0.02, 0.03)

  )

  expect_error(
    volcano_plot(
      data = data)
  )
})
