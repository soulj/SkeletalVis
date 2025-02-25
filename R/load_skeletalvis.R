#' Find the skeletalvis data folder and download if not present
#'
#' This function checks the default skeletalvis data location for the presence of the expected data and metadata files.
#' If these files do  not exist, it downloads the files from the inbuilt URLs.
#'
#' @return The path to the skeletalvis folder.
#' @export
#'
#' @examples
#' load_skeletalvis()

# Main function to load SkeletalVis data
load_skeletalvis <- function() {
  # Use rappdirs to determine the application data directory
  app_dir <- rappdirs::user_data_dir("SkeletalVis")


  urls <- c("https://www.dropbox.com/scl/fi/cslaigrypuw671zn3sirf/accessions.txt?rlkey=x9swp43drswlfqd9uyt3g8elp&st=4023lmdh&dl=1",
            "https://www.dropbox.com/scl/fi/9ln8ibaxt82jqfecy5fa7/expTable.txt?rlkey=tl6ddptgkbykat0ctfk8q48sj&st=4dch8mgi&dl=1",
            "https://www.dropbox.com/scl/fi/84fazktk4p9cz9ozwt90y/pvalTable.feather?rlkey=y5wniezmcqbnffqlt6g88ve9q&st=u51wb8l0&dl=1",
            "https://www.dropbox.com/scl/fi/hfaxb7fo006jfqmzup6te/foldChangeTable.feather?rlkey=1hoyoebu1zaeecqhridz7tdfl&st=hmr138on&dl=1",
            "https://www.dropbox.com/scl/fi/jld4c5b42xhpii8q3loxs/network.RDS?rlkey=34mqiokg5v3itajt4e73qfb3k&st=nmf6l1hr&dl=1",
            "https://www.dropbox.com/scl/fi/ildzl4ifuvu1hrk0bhxic/oatargets.txt?rlkey=80oka1lckmjqk0lwt1ca0e4qf&st=mzke93f2&dl=1")

  names(urls) <- c("accessions.txt","expTable.txt","pvalTable.feather",
 "foldChangeTable.feather","network.RDS","oatargets.txt")

  # Iterate over each URL and filename to check/download
  downloaded_files <- lapply(names(urls), function(filename) {
    destfile <- file.path(app_dir, filename)
    check_and_download_file(url = urls[[filename]], destfile = destfile)
  })

  return(app_dir)
}

# Helper function to check and download a file if not present
check_and_download_file <- function(url, destfile, verbose=FALSE) {
  # Check if the file already exists
  if (!file.exists(destfile)) {
    message("Downloading ",basename(destfile))

    # Create the directory if it doesn't exist
    destfolder <- dirname(destfile)
    if (!dir.exists(destfolder)) {
      dir.create(destfolder, showWarnings = TRUE, recursive = TRUE)
    }

    # Use httr to download the file
    tryCatch({
      httr::GET(url, httr::write_disk(destfile, overwrite = TRUE))
      message("Download complete: ", destfile)
    }, error = function(e) {
      stop("Error downloading the file from ", url, ": ", e$message)
    })
  } else {
    if(verbose) message("Data already exists at: ", destfile)
  }

  return(destfile)
}
