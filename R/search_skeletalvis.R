#' Search for experiments in SkeletalVis
#'
#' This function allows users to search for experiments matching a search term the skeletalvis database.
#' The search can be done across the entire expTable or user-specified columns.
#'
#' @param skeletalvis Folder with the skeletalvis data
#' @param search_term A string representing the term to search for.
#' @param columns Optional; A character vector of column names to limit the search. If NULL (default), the entire table is searched.
#'
#' @return A data frame containing experiments that match the search term in the specified columns or the whole table.
#' @export
#' @examples
#'
#' skeletalvis <- load_skeletalvis()

#' # Search across all columns for rows containing "SOX9"
#' result <- search_skeletalvis(skeletalvis, "SOX9")
#'
#' # Search only in the 'Perturbation' and 'Description' columns
#' result <- search_skeletalvis(skeletalvis, "SOX9", columns = c("Perturbation", "Description"))
search_skeletalvis <- function(skeletalvis, search_term, columns = NULL) {

  exp_filepath <- file.path(skeletalvis, "expTable.txt")

  if (!file.exists(exp_filepath)) stop("The file 'expTable.txt' does not exist in the specified directory.")

  exp_table <- read.csv(exp_filepath, header = TRUE, stringsAsFactors = FALSE)

  # Check if the search_term is a non-empty string
  if (missing(search_term) || !is.character(search_term) || nchar(search_term) == 0) {
    stop("Please provide a valid search term.")
  }

  # If no columns are specified, search across all columns
  if (is.null(columns)) {
    columns <- colnames(exp_table)
  }

  # Check if the specified columns exist in the exp_table
  if (!all(columns %in% colnames(exp_table))) {
    wrongColumns <- paste(columns[!columns %in% colnames(exp_table)],collapse = " ")
    stop(sprintf("The following columns do not exist in the expTable: %s",wrongColumns))
  }

  # Function to check if the search term is present in a given column
  match_search_term <- function(col) {
    grepl(search_term, col, ignore.case = TRUE)  # Case-insensitive matching
  }

  # Apply the matching function to the specified columns and return rows where the term matches
  matches <- apply(exp_table[, columns, drop = FALSE], 1, function(row) {
    any(sapply(row, match_search_term))
  })

  # Catch no matches with the search terms
  if(length(which(matches))==0) message("No matches found in the expTable.")

  # Return the subset of rows that match the search term
  return(exp_table[matches,])
  }
