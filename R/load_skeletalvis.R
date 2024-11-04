#' Download Feather File if Not Present
#'
#' This function checks if the specified Feather file exists in the local directory.
#' If it does not exist, it downloads the file from the given URL.
#'
#' @param url The URL to download the Feather file from.
#' @param destfile The destination file path where the Feather file will be saved.
#' @return The path to the Feather file.
#' @export
#'
#' @examples
#' download_feather_file("https://example.com/data/mydata.feather", "data/mydata.feather")

download_feather_file <- function(url, destfile="foldChangeTable.feather") {
  # Check if the file already exists
  if (!file.exists(destfile)) {
    # Download the file
    message("Downloading SkeletalVis data from ", url)

    # Create the directory if it doesn't exist
    dir.create(dirname(destfile), showWarnings = FALSE, recursive = TRUE)

    # Use httr to download the file
    tryCatch({
      httr::GET(url, httr::write_disk(destfile, overwrite = TRUE))
      message("Download complete: ", destfile)
    }, error = function(e) {
      stop("Error downloading the file: ", e$message)
    })
  } else {
    message("Feather file already exists: ", destfile)
  }

  return(destfile)
}



