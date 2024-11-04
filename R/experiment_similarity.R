# Load necessary libraries
library(dplyr)

#' Calculate Cosine Similarity
#'
#' This function calculates the cosine similarity between two vectors.
#'
#' @param i Index of the column in the fold change table to compare against.
#' @param X The fold change data frame.
#' @param query The query vector for comparison.
#' @return Cosine similarity value between the query and the selected dataset.
#'
cos.sim <- function(i, X, query) {
  B <- X[, c(1,i), drop = FALSE]
  merged <- merge(query, B, by=1)

  A <- merged[, 2]
  B <- merged[, 3]

  # Calculate cosine similarity
  return(sum(A * B, na.rm = TRUE) / sqrt(sum(A^2, na.rm = TRUE) * sum(B^2, na.rm = TRUE)))
}

#' Get Cosine Similarity for a Query Dataset Against Fold Change Table
#'
#' This function computes the cosine similarity of a given query dataset
#' against a fold change table, returning a data frame of similarities.
#'
#' @param dataset A numeric vector representing the fold change values for the query dataset.
#' @param foldchangeTable A data frame containing fold change values with gene IDs in the first column.
#' @param accessions A data frame containing additional information for merging, with a column named "combined".
#' @param datasetName A name for the dataset being queried (default is "query").
#' @return A data frame containing cosine similarity values, IDs, and z-scores.
#'
#' @examples
#' # Load necessary library
#' library(feather)
#'
#' # Load your feather file
#' foldchange_data <- read_feather("data/mydata.feather")
#'
#' # Create a query dataset (this should be a data frame with the first column as gene IDs)
#' query_dataset <- data.frame(ID = c("BRCA1", "TP53"), fold_change = c(2.5, -1.8))
#'
#' # Create a data frame of accessions to merge (example format)
#' accessions <- data.frame(combined = c("BRCA1", "TP53"), info = c("Gene A", "Gene B"))
#'
#' # Get cosine similarities
#' similarity_results <- getSim(
#'   dataset = query_dataset$fold_change,
#'   foldchangeTable = foldchange_data,
#'   accessions = accessions
#' )
#'
#' # View results
#' print(similarity_results)
#'
#' @export
experiment_similarity <- function(dataset, foldchangeTable) {
  # Apply cosine similarity calculation for each column in the fold change table (excluding last column)
  cosine <- sapply(seq_along(foldchangeTable)[-1], cos.sim, foldchangeTable, dataset)

  # Create a data frame with results
  sim <- data.frame(ID = colnames(foldchangeTable)[-1], cosine)

  # Scale the similarity scores (z-score)
  sim$zscore <- scale(sim$cosine)

  sim <- sim[ order(sim$zscore, decreasing=TRUE),]

  return(sim)
}
