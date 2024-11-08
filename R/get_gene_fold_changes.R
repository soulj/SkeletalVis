# Function to get results for a gene of interest
#' Get Results for a Gene of Interest
#'
#' This function loads a Feather file and extracts results for a specified gene.
#'
#' @param skeletalvis The path to the Feather file.
#' @param gene_symbol The gene symbol to extract results for.
#' @return A data frame containing results for the specified genes.
#' @export
#'
#' @examples
#' gene_results <- get_gene_fold_changes("skeletalvis", "SOX9")

get_gene_fold_changes <- function(skeletalvis, gene_symbol, returnFDR= FALSE, addMetaData=TRUE) {


  foldchange_filepath <- file.path(skeletalvis, "foldChangeTable.feather")

  if (!file.exists(foldchange_filepath)) stop(sprintf("The file 'foldChangeTable.feather' does not exist in the specified directory: %s",skeletalvis))
  if(addMetaData==TRUE) {

    metadata_filepath <- file.path(skeletalvis, "expTable.txt")

    if (!file.exists(metadata_filepath)) stop(sprintf("The file 'expTable.txt' does not exist in the specified directory: %s",skeletalvis))
    metadata <- read.csv(metadata_filepath)
  }


  if(returnPvalues==TRUE){

    pvalue_filepath <- file.path(skeletalvis, "pvalTable.feather")

    if (!file.exists(pvalue_filepath)) stop(sprintf("The file 'pvalTable.feather' does not exist in the specified directory: %s",skeletalvis))

    pvalues <- feather::read_feather(pvalue_filepath)

  }

  # Load the Feather file
  foldChanges <- feather::read_feather(foldchange_filepath)

  # Check if the data contains the ID column
  if (!"ID" %in% colnames(foldChanges)) {
    stop("The Feather file does not contain an 'ID' column.")
  }

  # Filter the data for the gene of interest
  foldChanges <- foldChanges %>%
    dplyr::filter(ID %in% gene_symbol) %>%
    select(-ID) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "SkeletalVisID",
                        values_to = "log2FoldChange")

  if(returnPvalues==TRUE){

  pvalues <- pvalues %>%
    dplyr::filter(GeneName %in% gene_symbol) %>%
    select(-GeneName) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "SkeletalVisID",
                        values_to = "FDR")

  foldChanges <- left_join(foldChanges,pvalues,by = "SkeletalVisID")

  foldChanges <- foldChanges %>%
    arrange(FDR)

  } else {
    foldChanges <- foldChanges %>%
      arrange(log2FoldChange)
  }

  # Check if any results were found
  if (nrow(foldChanges) == 0) {
    stop("No results found for the gene: ", gene_symbol)
  }

  if(addMetaData) {
    foldChanges$Accession <- stringr::word(foldChanges$SkeletalVisID, 1,1, sep="_")
    foldChanges <- left_join(foldChanges, metadata, by = c("Accession"="ID"))
  }

  return(foldChanges)
}
