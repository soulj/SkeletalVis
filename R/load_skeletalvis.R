download_file <- function(url, destfile, verbose=FALSE) {
  destfolder <- dirname(destfile)
  if (!dir.exists(destfolder)) {
    dir.create(destfolder, showWarnings = TRUE, recursive = TRUE)
  }

  tryCatch({
    httr::GET(url, httr::write_disk(destfile, overwrite = TRUE),  httr::progress())
    if (verbose) message("Download complete: ", destfile)
  }, error = function(e) {
    stop("Error downloading the file from ", url, ": ", e$message)
  })
}

#' Load SkeletalVis data
#'
#' Checks the default SkeletalVis data location for the differential expression data and metadata files.
#' If any files are missing, it lists them and ask permission to download the missing files.
#'
#' @param verbose Logical. If TRUE, prints messages about file existence and download status. Default is TRUE
#' @param ask Logical. If TRUE, prompts the user for confirmation before downloading missing files. Default is TRUE.
#' @param demo Logical. If TRUE, uses built in demo data suitable for testing the package functions. Default is FALSE

#' @return The path to the SkeletalVis data folder.
#'
#' @examples
#' # Load the demo SkeletalVis data.
#' skeletalvis <- load_skeletalvis(demo = TRUE)
#'
#' # Download full dataset (only if running interactively)
#' if (interactive()) {
#'   skeletalvis <- load_skeletalvis()
#' }
#'
#' @export
load_skeletalvis <- function(verbose=TRUE, ask=TRUE, demo=FALSE) {

  if (demo) {
    demo_folder <- system.file("extdata", package = "SkeletalVis")
    return(demo_folder)
  }

  # Determine the application data directory
  app_dir <- tools::R_user_dir("SkeletalVis")

  # Define file URLs and corresponding filenames
  urls <- c("https://www.dropbox.com/scl/fi/cslaigrypuw671zn3sirf/accessions.txt?rlkey=x9swp43drswlfqd9uyt3g8elp&st=4023lmdh&dl=1",
            "https://www.dropbox.com/scl/fi/9ln8ibaxt82jqfecy5fa7/expTable.txt?rlkey=tl6ddptgkbykat0ctfk8q48sj&st=4dch8mgi&dl=1",
            "https://www.dropbox.com/scl/fi/84fazktk4p9cz9ozwt90y/pvalTable.feather?rlkey=y5wniezmcqbnffqlt6g88ve9q&st=u51wb8l0&dl=1",
            "https://www.dropbox.com/scl/fi/hfaxb7fo006jfqmzup6te/foldChangeTable.feather?rlkey=1hoyoebu1zaeecqhridz7tdfl&st=hmr138on&dl=1",
            "https://www.dropbox.com/scl/fi/jld4c5b42xhpii8q3loxs/network.RDS?rlkey=34mqiokg5v3itajt4e73qfb3k&st=nmf6l1hr&dl=1",
            "https://www.dropbox.com/scl/fi/ildzl4ifuvu1hrk0bhxic/oatargets.txt?rlkey=80oka1lckmjqk0lwt1ca0e4qf&st=mzke93f2&dl=1",
            "https://www.dropbox.com/scl/fi/s8fw2d3ygzho2yrvgx42w/oatargets_prioritised.txt?rlkey=v45j97nh7yhyr9d2e1frb8hit&st=kfodmks3&dl=1")

  names(urls) <- c("accessions.txt", "expTable.txt", "pvalTable.feather",
                   "foldChangeTable.feather", "network.RDS", "oatargets.txt",
                   "oatargets_prioritised.txt")

  # Check which files are missing
  missing_files <- names(urls)[!file.exists(file.path(app_dir, names(urls)))]

  if (length(missing_files) > 0) {
    message("The following files are missing:")
    message(paste(missing_files, collapse = "\n"))

    if (ask) {
      response <- readline(prompt = "Do you want to download all missing files? (yes/no): ")
      if (tolower(response) != "yes") {
        stop("User aborted the download.")
      }
    }

    # Download each missing file
    for (filename in missing_files) {
      destfile <- file.path(app_dir, filename)
      download_file(url = urls[filename], destfile = destfile, verbose = verbose)
    }
  } else {
    if (verbose) message("All files are already present in ", app_dir)
  }

  return(app_dir)
}
