#' Create a Waterfall Plot for log2FoldChange Data
#'
#' This function generates a waterfall plot using ggplot2, where each bar represents
#' the `log2FoldChange` for a unique `SkeletalVisID`, ordered by value.
#'
#' @param data A data frame containing `SkeletalVisID`, `log2FoldChange`, and `FDR` columns.
#' @param fdr_threshold A numeric value to indicate the significance threshold for `FDR`.
#'                      Entries below this threshold are considered significant.
#' @return A ggplot2 object representing the waterfall plot.
#' @examples
#' # Example data
#' data <- data.frame(
#'   SkeletalVisID = c("E-GEOD-69110_1", "GSE155118_1", "E-GEOD-54461_8"),
#'   log2FoldChange = c(7.15, 3.11, -1.28),
#'   FDR = c(0, 0, 2.25e-195)
#' )
#' # Create waterfall plot
#' waterfall_plot(data, fdr_threshold = 0.05)
waterfall_plot <- function(data, fdr_threshold = 0.05) {

  # Input validation
  if (!all(c("SkeletalVisID", "log2FoldChange", "FDR") %in% colnames(data))) {
    stop("Data frame must contain 'SkeletalVisID', 'log2FoldChange', and 'FDR' columns.")
  }

  # Add a column to flag significance based on the FDR threshold
  data$Significant <- data$FDR < fdr_threshold

  # Order data by log2FoldChange for the waterfall effect
  data <- data[order(data$log2FoldChange, decreasing = TRUE), ]
  data$SkeletalVisID <- factor(data$SkeletalVisID, levels = data$SkeletalVisID)

  # Create the waterfall plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = SkeletalVisID, y = log2FoldChange, fill = Significant)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2:: scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "grey"), name = "Significant") +
    ggplot2::labs(
      title = "Waterfall Plot of log2FoldChange",
      x = "SkeletalVisID",
      y = "log2FoldChange"
    ) +
    cowplot::theme_cowplot() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "top"
    )

  return(plot)
}
