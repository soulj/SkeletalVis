#' Create Heatmap from Gene Fold Change Data with Annotations and FDR Significance
#'
#' This function generates a heatmap from gene fold change data, using specified columns for annotations
#' and stars to denote FDR significance levels.
#'
#' @param data Data frame containing gene fold change data.
#' @param value_column Name of the column to use for heatmap values (e.g., "log2FoldChange").
#' @param annotation_columns Character vector of column names to use for annotations (e.g., c("Tissue", "Species")).
#' @param fdr_column Name of the FDR column for significance (e.g., "FDR"). Defaults to "FDR".
#' @param heatmap_title Optional title for the heatmap.
#' @return A heatmap plot displaying the specified value column with annotations and FDR significance stars.

#' @export
create_gene_heatmap <- function(data, value_column = "log2FoldChange", annotation_columns = NULL, fdr_column = "FDR", heatmap_title = "Gene Fold Change Heatmap") {

  # Ensure the value column, FDR column, and annotation columns exist in the data
  if (!(value_column %in% colnames(data))) stop("Specified value column does not exist in the data.")
  if (!(fdr_column %in% colnames(data))) stop("Specified FDR column does not exist in the data.")
  if (!all(annotation_columns %in% colnames(data))) stop("One or more annotation columns do not exist in the data.")

  # Convert FDR values to significance stars
  data <- data %>%
    dplyr::mutate(Significance = dplyr::case_when(
      !!sym(fdr_column) < 0.001 ~ "***",
      !!sym(fdr_column) < 0.01 ~ "**",
      !!sym(fdr_column) < 0.05 ~ "*",
      TRUE ~ ""
    ))

  # Pivot data to create heatmap matrix
  heatmap_matrix <- data %>%
    dplyr::select(datasetID, Gene, !!sym(value_column)) %>%
    tidyr::pivot_wider(names_from = datasetID, values_from = !!sym(value_column)) %>%
    tibble::column_to_rownames("Gene")

  # Create a matrix of significance stars for overlay
  star_matrix <- data %>%
    dplyr::select(datasetID, Gene, Significance) %>%
    tidyr::pivot_wider(names_from = datasetID, values_from = Significance) %>%
    tibble::column_to_rownames("Gene")

  # Generate annotation data and assign reproducible colors
  if (!is.null(annotation_columns)) {
    annotation_data <- data %>%
      dplyr::select(datasetID, dplyr::all_of(annotation_columns)) %>%
      dplyr::distinct(datasetID, .keep_all = TRUE) %>%
      tibble::column_to_rownames("datasetID")

    # Generate colors for each annotation column using viridis for reproducibility
    annotation_colors <- lapply(annotation_data, function(column) {
      unique_vals <- unique(column)
      colors <- viridis::viridis(length(unique_vals), option = "D")
      names(colors) <- unique_vals
      colors
    })

    # Create heatmap annotation object
    heatmap_annotation <- ComplexHeatmap::HeatmapAnnotation(
      df = annotation_data,
      col = annotation_colors,
      annotation_name_side = "left"
    )
  } else {
    heatmap_annotation <- NULL
  }

  # Create the heatmap with stars for FDR significance overlay
  heatmap <- ComplexHeatmap::Heatmap(
    as.matrix(heatmap_matrix),
    name = value_column,
    col = circlize::colorRamp2(c(min(data[[value_column]], na.rm = TRUE),
                       0,
                       max(data[[value_column]], na.rm = TRUE)),
                     c("blue", "white", "red")),
    top_annotation = heatmap_annotation,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    show_row_names = TRUE,
    show_column_names = TRUE,
    column_title = heatmap_title,
    cell_fun = function(j, i, x, y, width, height, fill) {
      grid::grid.text(star_matrix[i, j], x, y, gp = grid::gpar(fontsize = 16, col = "black"))
    }
  )

  # Draw the heatmap
  ComplexHeatmap::draw(heatmap)

  return(heatmap)
}
