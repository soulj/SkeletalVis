#' Get gene differential expression results for genes of interest
#'
#' Extracts the fold change and p-values across the SkeletalVis database for the specified genes.
#'
#' @param skeletalvis The path to the SkeletalVis data folder.
#' @param gene_symbols The human gene symbols to extract fold change and FDR values for.
#' @param return_fdr Return FDR values (FALSE by default).
#' @param add_meta_data Add metadata such as species, tissue, description of overall experiment and specific comparison
#' @return A tibble containing gene expression results for the specified genes.
#' @export
#'
#' @examples
#' skeletalvis <- load_skeletalvis(demo=TRUE)
#'
#' gene_results <- get_gene_fold_changes(skeletalvis, c("SOX9","ACAN"))
#'
#' head(gene_results)
get_gene_fold_changes <- function(skeletalvis, gene_symbols, return_fdr = TRUE, add_meta_data = TRUE) {

  foldchange_filepath <- file.path(skeletalvis, "foldChangeTable.feather")

  if (!file.exists(foldchange_filepath)) stop(sprintf("The file 'foldChangeTable.feather' does not exist in the specified directory: %s", skeletalvis))

  if (add_meta_data){

    exp_table <- get_exp_table(skeletalvis)
    metadata <- get_comparisons(skeletalvis)
  }

  if (return_fdr) {
    pvalue_filepath <- file.path(skeletalvis, "pvalTable.feather")
    if (!file.exists(pvalue_filepath)) stop(sprintf("The file 'pvalTable.feather' does not exist in the specified directory: %s", skeletalvis))
    pvalues <- arrow::read_feather(pvalue_filepath)
  }

  # Load the Feather file
  fold_changes <- arrow::read_feather(foldchange_filepath)

  # Check if the data contains the ID column
  if (!"ID" %in% colnames(fold_changes)) {
    stop("The Feather file does not contain an 'ID' column.")
  }

  # Check if the data contains the ID column
  if (!any(gene_symbols %in% fold_changes$ID)) {
    stop("None of the specified gene symbols were found in the fold change table.")
  }

  # Process fold change and FDR for each gene symbol
  gene_data <- lapply(gene_symbols, function(gene_symbol) {
    fc <- fold_changes %>%
      dplyr::filter(.data$ID == gene_symbol) %>%
      dplyr::select(-"ID") %>%
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "datasetID",
                          values_to = "log2FoldChange") %>%
      dplyr::mutate(Gene = gene_symbol) # Add a column to label gene

    if (return_fdr) {
      fdr <- pvalues %>%
        dplyr::filter(.data$GeneName == gene_symbol) %>%
        dplyr::select(-"GeneName") %>%
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = "datasetID",
                            values_to = "FDR")
      fc <- dplyr::left_join(fc, fdr, by = "datasetID")
    }

    fc
  })

  # Combine data for all genes with a single datasetID column
  fold_changes <- dplyr::bind_rows(gene_data)

  # Arrange by FDR or log2FoldChange
  if (return_fdr) {
    fold_changes <- fold_changes %>%
      dplyr::arrange(.data$FDR)
  } else {
    fold_changes <- fold_changes %>%
      dplyr::arrange(.data$log2FoldChange)
  }

  # Rearrange columns to place Gene as the last column
  fold_changes <- fold_changes %>%
    dplyr::relocate("Gene", .after = dplyr::last_col())

  # Add metadata if requested
  if (add_meta_data) {
    metadata <- get_comparisons(skeletalvis)[,-2]
    fold_changes <- dplyr::left_join(fold_changes, metadata, by = "datasetID")
    fold_changes <- dplyr::left_join(fold_changes, exp_table, by = c("accession" = "ID"))
  }

  return(fold_changes)
}
