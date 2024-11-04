# Function to get results for a gene of interest
#' Get Results for a Gene of Interest
#'
#' This function loads a Feather file and extracts results for a specified gene.
#'
#' @param feather_file The path to the Feather file.
#' @param gene_symbols The gene symbol to extract results for.
#' @return A data frame containing results for the specified genes.
#' @export
#'
#' @examples
#' gene_results <- get_gene_fold_changes("data/mydata.feather", "BRCA1")

get_gene_fold_changes <- function(feather_file, gene_symbols) {

  # Load the Feather file
  data <- feather::read_feather(feather_file)

  # Check if the data contains the ID column
  if (!"ID" %in% colnames(data)) {
    stop("The Feather file does not contain an 'ID' column.")
  }

  # Filter the data for the gene of interest
  gene_data <- data %>%
    dplyr::filter(ID %in% gene_symbols)

  # Check if any results were found
  if (nrow(gene_data) == 0) {
    stop("No results found for genes: ", gene_symbols)
  }

  return(gene_data)
}
