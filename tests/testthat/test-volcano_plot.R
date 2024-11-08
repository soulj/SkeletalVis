test_that("volcano_plot generates a ggplot object", {
  data <- data.frame(
    GeneID = c("gene1", "gene2", "gene3", "gene4"),
    p_value = c(0.001, 0.05, 0.2, 0.03),
    log2FoldChange = c(2, -3, 0.5, 1.2)
  )

  plot <- volcano_plot(
    data = data,
    id_col = "GeneID",
    pval_col = "p_value",
    fc_col = "log2FoldChange"
  )

  expect_s3_class(plot, "ggplot")
})

test_that("volcano_plot applies fold change and p-value thresholds", {
  data <- data.frame(
    GeneID = paste0("gene", 1:5),
    p_value = c(0.001, 0.02, 0.03, 0.06, 0.5),
    log2FoldChange = c(2, -2.5, 0.3, -3, 0)
  )

  plot <- volcano_plot(
    data = data,
    id_col = "GeneID",
    pval_col = "p_value",
    fc_col = "log2FoldChange",
    logFCThreshold = 1,
    adjPValThreshold = 0.05
  )

  plot_data <- ggplot_build(plot)$data[[1]]
  expect_equal(sum(plot_data$colour == "dodgerblue3"), 1)  # Down-regulated
  expect_equal(sum(plot_data$colour == "firebrick3"), 1)  # Up-regulated
})

test_that("volcano_plot handles data with no significant points", {
  data <- data.frame(
    GeneID = paste0("gene", 1:5),
    p_value = c(0.2, 0.3, 0.15, 0.25, 0.4),
    log2FoldChange = c(0.1, 0.5, -0.3, -0.6, 0.2)
  )

  plot <- volcano_plot(
    data = data,
    id_col = "GeneID",
    pval_col = "p_value",
    fc_col = "log2FoldChange",
    logFCThreshold = 1,
    adjPValThreshold = 0.05
  )

  plot_data <- ggplot_build(plot)$data[[1]]
  expect_equal(sum(plot_data$colour == "dodgerblue3"), 0)  # No up-regulated points
  expect_equal(sum(plot_data$colour == "firebrick3"), 0)  # No down-regulated points
})

test_that("volcano_plot assigns custom point size and label size", {
  data <- data.frame(
    GeneID = c("gene1", "gene2", "gene3"),
    p_value = c(0.01, 0.02, 0.03),
    log2FoldChange = c(1.5, -2, 0)
  )

  plot <- volcano_plot(
    data = data,
    id_col = "GeneID",
    pval_col = "p_value",
    fc_col = "log2FoldChange",
    point.size = 4,
    lab.size = 6
  )

  plot_data <- ggplot_build(plot)$data[[1]]
  expect_equal(unique(plot_data$size), 4)
})

test_that("volcano_plot handles Inf and NA values in p-values", {
  data <- data.frame(
    GeneID = c("gene1", "gene2", "gene3"),
    p_value = c(0.001, Inf, NA),
    log2FoldChange = c(2, -1.5, 1.2)
  )

  plot <- volcano_plot(
    data = data,
    id_col = "GeneID",
    pval_col = "p_value",
    fc_col = "log2FoldChange"
  )

  plot_data <- ggplot_build(plot)$data[[1]]
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
      data = data,
      id_col = "GeneID",
      pval_col = "p_value",
      fc_col = "log2FoldChange"
    ),
    "Columns missing from the data"
  )
})
