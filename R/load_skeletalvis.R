#' Find the skeletalvis data folder and download if not present
#'
#' This function checks the default skeletalvis data location for the presence of the expected data and metadata files.
#' If these files do not exist, it downloads the files from the inbuilt URLs.
#'
#' @param verbose Logical. If TRUE, prints messages about file existence and download status. Default is FALSE.
#' @param ask Logical. If TRUE, prompts the user for confirmation before downloading files. Default is TRUE.
#' @param demo Logical. If TRUE, loads the in built demo data used for building the vignette. Default is FALSE
#' @return The path to the skeletalvis folder.
#' @export
#'
#' @examples
#' # Load the demo SkeletalVis data.
#' skeletalvis <- load_skeletalvis(demo=TRUE)
#'
#' # Download full dataset
#' if (interactive()) {
#' skeletalvis <- load_skeletalvis()
#'}


# Main function to load SkeletalVis data
load_skeletalvis <- function(verbose=FALSE, ask=TRUE, demo=FALSE) {


  if(demo){

    demo_folder <- system.file("extdata", package = "SkeletalVis")
    return(demo_folder)

  }



  # Determine the application data directory
  app_dir <- tools::R_user_dir("SkeletalVis")


  urls <- c("https://www.dropbox.com/scl/fi/cslaigrypuw671zn3sirf/accessions.txt?rlkey=x9swp43drswlfqd9uyt3g8elp&st=4023lmdh&dl=1",
            "https://www.dropbox.com/scl/fi/9ln8ibaxt82jqfecy5fa7/expTable.txt?rlkey=tl6ddptgkbykat0ctfk8q48sj&st=4dch8mgi&dl=1",
            "https://www.dropbox.com/scl/fi/84fazktk4p9cz9ozwt90y/pvalTable.feather?rlkey=y5wniezmcqbnffqlt6g88ve9q&st=u51wb8l0&dl=1",
            "https://www.dropbox.com/scl/fi/hfaxb7fo006jfqmzup6te/foldChangeTable.feather?rlkey=1hoyoebu1zaeecqhridz7tdfl&st=hmr138on&dl=1",
            "https://www.dropbox.com/scl/fi/jld4c5b42xhpii8q3loxs/network.RDS?rlkey=34mqiokg5v3itajt4e73qfb3k&st=nmf6l1hr&dl=1",
            "https://www.dropbox.com/scl/fi/ildzl4ifuvu1hrk0bhxic/oatargets.txt?rlkey=80oka1lckmjqk0lwt1ca0e4qf&st=mzke93f2&dl=1")

  names(urls) <- c("accessions.txt","expTable.txt","pvalTable.feather",
 "foldChangeTable.feather","network.RDS","oatargets.txt")

  # Iterate over each URL and filename to check/download, ask for permission to download
  downloaded_files <- lapply(names(urls), function(filename) {
    destfile <- file.path(app_dir, filename)
    check_and_download_file(url = urls[[filename]], destfile = destfile, verbose=verbose, ask=ask)
  })

  return(app_dir)
}

# Helper function to check and download a file if not present
check_and_download_file <- function(url, destfile, verbose=FALSE, ask=TRUE) {
  if (!file.exists(destfile)) {
    if (ask) {
      response <- readline(prompt = paste("Do you want to download", basename(destfile), "? (yes/no): "))
      if (tolower(response) != "yes") {
        stop("User aborted the download.")
      }
    }

    message("Downloading ", basename(destfile))

    destfolder <- dirname(destfile)
    if (!dir.exists(destfolder)) {
      dir.create(destfolder, showWarnings = TRUE, recursive = TRUE)
    }

    tryCatch({
      httr::GET(url, httr::write_disk(destfile, overwrite = TRUE))
      message("Download complete: ", destfile)
    }, error = function(e) {
      stop("Error downloading the file from ", url, ": ", e$message)
    })
  } else {
    if (verbose) message("Data already exists at: ", destfile)
  }

  return(destfile)
}
