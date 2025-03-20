#' Plot ranked cosine similarity scores with top n labels
#'
#' Takes a similarity table with columns for ID, cosine, and zscore values,
#' and generates a plot showing the rank of the z-scores on a log scale. Labels for the top
#' n IDs with the highest z-scores are displayed on the plot.
#'
#' @param similarity_table A data frame with columns: `ID` (identifier for each item),
#'                         `cosine` (cosine similarity score), and `zscore` (z-score).
#' @param top_n An integer indicating the number of top IDs by zscore to label in the plot.
#'              Default is 10.
#'
#' @return A ggplot object displaying the ranked z-scores on a log scale, with labels
#'         for the top n items by zscore.
#' @export
#' @examples
#' data(query)
#' skeletalvis <- load_skeletalvis(demo=TRUE)
#' similarity_results <- experiment_similarity(skeletalvis, query)
#'
#'# Plot similarity table with labels for top 5 items
#' plot_similarity(similarity_results, top_n = 5)
plot_similarity <- function(similarity_table, top_n = 10) {
  # Validate that the similarity_table has the required columns
  required_columns <- c("datasetID", "cosine", "zscore")
  if (!all(required_columns %in% colnames(similarity_table))) {
    stop("The similarity table must contain the following columns: 'datasetID', 'cosine', and 'zscore'.")
  }

  # Ensure top_n is a positive integer
  if (!is.numeric(top_n) || top_n <= 0) {
    stop("The 'top_n' parameter must be a positive integer.")
  }

  # Calculate ranks and prepare data for labeling top N items by zscore
  similarity_table <- similarity_table %>%
    dplyr::mutate(rank = rank(-.data$zscore))  # Rank in descending order of zscore
  top_labels <- similarity_table %>%
    dplyr::arrange(dplyr::desc(.data$zscore)) %>%
    dplyr::slice_head(n = top_n)

  # Create the plot
  ggplot2::ggplot(similarity_table, ggplot2::aes(x = .data$zscore, y = rank)) +
    ggplot2::geom_point(color = "blue") +
    ggplot2::scale_y_log10() +  # Use a log scale for the y-axis
    ggplot2::labs(
      x = "Z-Score",
      y = "Rank of Z-Score (Log Scale)"
    ) +
    cowplot::theme_cowplot(font_size = 16) +  # Apply cowplot theme
    # Add ggrepel labels for the top N items by zscore
    ggrepel::geom_text_repel(data = top_labels, ggplot2::aes(label = .data$datasetID), color = "red", seed = 42, min.segment.length = 0)
}
