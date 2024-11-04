# R/volcano_plot.R

#' Volcano Plot of Gene Expression Data
#'
#' @param data A data frame containing gene expression data.
#' @param pval_col The column name for p-values.
#' @param fc_col The column name for fold changes.
#' @return A ggplot object representing the volcano plot.
#' @export
#' @examples
#' volcano_plot(my_data, pval_col = "p_value", fc_col = "fold_change")

volcano_plot <- function(data, pval_col, fc_col) {
  library(ggplot2)
  ggplot(data, aes_string(x = fc_col, y = pval_col)) +
    geom_point(alpha = 0.5) +
    theme_minimal() +
    labs(x = "Log2 Fold Change", y = "-Log10 P-value") +
    ggtitle("Volcano Plot")
}
