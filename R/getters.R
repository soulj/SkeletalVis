#' Retrieve metadata for the SkeletalVis database
#'
#' @description
#' `get_comparisons`Retieves the metadata file containing accession and comparison information
#' for the skeletalvis database.
#'
#' @param skeletalvis The path to the SkeletalVis data folder.
#' @param accession Optionally an experiment accession number for the experiment of interest to filter the metadata by.
#'
#' @return A dataframe containing metadata.
#'
#' @examples
#' # Get the path to the directory with the accessions.txt file
#' skeletalvis_path <- load_skeletalvis(demo=TRUE)
#'
#' # Retrieve the metadata
#' metadata <- get_comparisons(skeletalvis_path)
#'
#' # View the metadata
#' head(metadata)
#'
#' metadata <- get_comparisons(skeletalvis_path, "GSE85761")
#' @export
get_comparisons <- function(skeletalvis, accession = NULL) {

  # Construct the file path to the metadata file
  metadata_filepath <- file.path(skeletalvis, "accessions.txt")

  # Check if the file exists, and stop with an error if not
  if (!file.exists(metadata_filepath)) stop(sprintf("The file 'accessions.txt' does not exist in the directory: %s. Please run load_skeletalvis first.", skeletalvis))

  # Read the metadata file
  metadata <- read.delim(metadata_filepath)

  # Create a unique dataset identifier by combining 'accession' and 'comparison'
  metadata$datasetID <- paste(metadata$accession, metadata$comparison, sep = "_")

  if(!is.null(accession)){
    metadata <- metadata[ metadata$accession %in% accession, ]
  }

  return(metadata)
}


#' Retrieve the experiment table for the SkeletalVis database
#'
#' @description
#' Retrieves the experiment table file containing experiment accessions and descriptions from the skeletalvis database.
#'
#' @param skeletalvis The path to the skeletalvis data folder.
#'
#' @return A dataframe containing the experiment information
#'
#' @examples
#' # Get the path to the directory with the accessions.txt file
#' skeletalvis_path <- load_skeletalvis(demo=TRUE)
#'
#' # Retrieve the metadata
#' exptable <- get_exp_table(skeletalvis_path)
#'
#' # View the metadata
#' head(exptable)
#' @export
get_exp_table <- function(skeletalvis) {
  # Make path to the experiment info
  exp_table_filepath <- file.path(skeletalvis, "expTable.txt")

  # Check it exists
  if (!file.exists(exp_table_filepath)) stop(sprintf("The file 'expTable.txt' does not exist in the specified directory: %s", skeletalvis))

  # Read in the file
  exp_table <- read.csv(exp_table_filepath)

  return(exp_table)
}


