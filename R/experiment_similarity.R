# Calculate Cosine Similarity
cos.sim <- function(i, X) {

  X <- na.omit(X[, c(1,i,ncol(X))])

  A <- X[, 2]
  B <- X[, 3]

  return(sum(A * B, na.rm = TRUE) / sqrt(sum(A^2, na.rm = TRUE) * sum(B^2, na.rm = TRUE)))
}

#' Get cosine similarity for a query dataset against the skeletalvis database
#'
#' Computes the cosine similarity of the log2 fold changes of a given query dataset
#' against the SkeletalVis fold changes, returning a data frame of experiment similarities.
#'
#' @param skeletalvis The path to the SkeletalVis data folder.
#' @param dataset A dataframe with human gene symbols and log2 fold changes.
#' @param add_meta_data Add metadata such as species, tissue, description of overall experiment and specific comparison
#'
#' @return A data frame containing cosine similarity values, IDs, and z-scores.
#'
#' @examples
#' skeletalvis <- load_skeletalvis(demo=TRUE)
#'
#' # Create a query dataset (this should be a data frame with the first column as gene IDs)
#' query_dataset <- data.frame(ID = c("SOX9", "ACAN"), fold_change = c(2.5, -1.8))
#'
#' # Get cosine similarities
#' similarity_results <- experiment_similarity(
#'   skeletalvis = skeletalvis,
#'   dataset = query_dataset,
#' )
#'
#' # View results
#' head(similarity_results)
#'
#' @export
experiment_similarity <- function(skeletalvis, dataset, add_meta_data = TRUE) {

  foldchange_filepath <- file.path(skeletalvis, "foldChangeTable.feather")

  if (!file.exists(foldchange_filepath)) stop(sprintf("The file 'foldChangeTable.feather' does not exist in the specified directory: %s",skeletalvis))

  fold_change_table <- arrow::read_feather(foldchange_filepath)  %>%
    dplyr::select("ID", dplyr::everything())

  colnames(dataset)[1:2] <- c("ID","queryFC")

  fold_change_table <- merge(fold_change_table, dataset, by.x="ID", by.y="ID")

  # Apply cosine similarity calculation for each column in the fold change table (excluding last column)
  cosine <- pbapply::pbsapply(seq_along(fold_change_table)[c(-1,-ncol(fold_change_table))], cos.sim, fold_change_table)

  # Create a data frame with results
  sim <- data.frame(ID = colnames(fold_change_table)[c(-1,-ncol(fold_change_table))], cosine)

  # Scale the similarity scores (z-score)
  sim$zscore <- scale(sim$cosine)[,1]

  colnames(sim)[1] <- "datasetID"

  sim <- sim[ order(sim$zscore, decreasing=TRUE),]

  # Add metadata if requested
  if (add_meta_data) {
    metadata <- get_comparisons(skeletalvis)[,-2]
    sim <- dplyr::left_join(sim, metadata, by = "datasetID")

    exp_table <- get_exp_table(skeletalvis)
    sim <- dplyr::left_join(sim, exp_table, by = c("accession" = "ID"))
  }

  return(sim)
}
