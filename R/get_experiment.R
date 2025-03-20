#' Get the gene differential expression results for an experiment of interest
#'
#' This function loads the differential expression data (log2 foldchanges and FDR values) for a given dataset id
#'
#' @param skeletalvis The path to the SkeletalVis folder.
#' @param dataset_id The dataset ID to extract results for.
#' @return A data frame containing differential expression results for the specified dataset ID.
#'
#' @examples
#' skeletalvis <- load_skeletalvis(demo=TRUE)
#' experiment_results <- get_experiment(skeletalvis, "GSE12860_6")
#' @export

get_experiment <- function(skeletalvis, dataset_id) {


  foldchange_filepath <- file.path(skeletalvis, "foldChangeTable.feather")

  if (!file.exists(foldchange_filepath)) stop(sprintf("The file 'foldChangeTable.feather' does not exist in the specified directory: %s",skeletalvis))

    pvalue_filepath <- file.path(skeletalvis, "pvalTable.feather")

  if (!file.exists(pvalue_filepath)) stop(sprintf("The file 'pvalTable.feather' does not exist in the specified directory: %s",skeletalvis))

  pvalues <- arrow::read_feather(pvalue_filepath)


  # Load the feather file
  fold_changes <- arrow::read_feather(foldchange_filepath)

  # Check if the data contains the ID column
  if (!"ID" %in% colnames(fold_changes)) {
    stop("The Feather file does not contain an 'ID' column.")
  }

  # Check if any results were found
  if (!all(dataset_id %in% colnames(fold_changes))) {
    stop("No results found for the dataset id: ", dataset_id)
  }

  # Filter the data for the gene of interest
  fold_changes <- fold_changes %>%
    dplyr::select("ID", dplyr::any_of(dataset_id))

  colnames(fold_changes)[2:ncol(fold_changes)] <- paste0(colnames(fold_changes)[2:ncol(fold_changes)],"_log2FoldChange")

    pvalues <- pvalues %>%
      dplyr::select("GeneName", dplyr::any_of(dataset_id))

    colnames(pvalues)[2:ncol(pvalues)] <- paste0(colnames(pvalues)[2:ncol(pvalues)],"_FDR")

    fold_changes <- dplyr::left_join(fold_changes, pvalues, by = c("ID"="GeneName"))

    colname <- names(fold_changes)[2]

    fold_changes <- fold_changes %>%
      dplyr::filter(!is.na(.data[[colname]])) %>%
      dplyr::arrange(dplyr::desc(.data[[colname]]))


  return(fold_changes)
}
