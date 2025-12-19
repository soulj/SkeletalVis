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
#' @param force_update Logical. If TRUE, re-downloads all data files even if present. Default is FALSE

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
load_skeletalvis <- function(verbose=TRUE, ask=TRUE, demo=FALSE, force_update=FALSE) {
  if (demo) {
    demo_folder <- system.file("extdata", package = "SkeletalVis")
    return(demo_folder)
  }

  # Determine the application data directory
  app_dir <- tools::R_user_dir("SkeletalVis")
  if (!dir.exists(app_dir)) dir.create(app_dir, recursive = TRUE)

  # Define file URLs and corresponding filenames
  urls <- c("https://www.dropbox.com/scl/fi/4ckfh1im2q6pjxk8hp5fa/accessions.txt?rlkey=yd92tuvnqrswapa9h750dpfa6&st=86a425y0&dl=1",
            "https://www.dropbox.com/scl/fi/175m6gy3jgc20gd4s7doi/expTable.txt?rlkey=e9i2roq11hhz1z00nieymmjgw&st=ohnvotz3&dl=1",
            "https://www.dropbox.com/scl/fi/zbparwfen1q8s91gxuz7l/pvalTable.feather?rlkey=88uuykvn405or12fvuygjsp7k&st=90awp8t4&dl=1",
            "https://www.dropbox.com/scl/fi/0teuruu92odvqmc1pdsdx/foldChangeTable.feather?rlkey=mkxjvpbcw9fjqfz06c74hvegl&st=ux11svzr&dl=1",
            "https://www.dropbox.com/scl/fi/gugx8t8ckinfxv9u0y82a/network.RDS?rlkey=s7n1egn2z5meco1gc48lrnoq1&st=2i7l5eum&dl=1",
            "https://www.dropbox.com/scl/fi/0435zvm67wqmum6fiz224/oatargets.txt?rlkey=oav851eppxdao9ixv9zx42ly6&st=wr63lfou&dl=1",
            "https://www.dropbox.com/scl/fi/xp9n8peak0czv5pkefozr/oatargets_prioritised.txt?rlkey=mdp1jlnf6ueckqe93pj1kh9ab&st=uo7wa5v1&dl=1")
  names(urls) <- c("accessions.txt", "expTable.txt", "pvalTable.feather",
                   "foldChangeTable.feather", "network.RDS", "oatargets.txt",
                   "oatargets_prioritised.txt")

  # Metadata file to track URLs and versions
  metadata_file <- file.path(app_dir, ".skeletalvis_metadata.rds")

  # Load existing metadata if available
  if (file.exists(metadata_file)) {
    metadata <- readRDS(metadata_file)
  } else {
    metadata <- list(urls = list(), downloaded = list())
  }

  # Check for missing files and URL changes
  missing_files <- character()
  updated_files <- character()

  for (filename in names(urls)) {
    filepath <- file.path(app_dir, filename)
    current_url <- urls[filename]
    stored_url <- metadata$urls[[filename]]

    if (!file.exists(filepath)) {
      missing_files <- c(missing_files, filename)
    } else if (is.null(stored_url) || stored_url != current_url) {
      # URL has changed or no metadata exists
      updated_files <- c(updated_files, filename)
    }
  }

  # Determine what needs to be downloaded
  files_to_download <- character()

  if (force_update) {
    files_to_download <- names(urls)
    if (verbose) message("Update requested. All files will be re-downloaded.")
  } else {
    files_to_download <- c(missing_files, updated_files)
  }

  # Handle downloads
  if (length(files_to_download) > 0) {
    if (length(missing_files) > 0) {
      message("Missing files:")
      message(paste("  -", missing_files, collapse = "\n"))
    }
    if (length(updated_files) > 0) {
      message("Files with newer data available:")
      message(paste("  -", updated_files, collapse = "\n"))
    }

    if (ask) {
      prompt_msg <- sprintf("Download %d file(s)? (yes/no): ",
                            length(files_to_download))
      response <- readline(prompt = prompt_msg)
      if (tolower(response) != "yes") {
        if (length(missing_files) > 0) {
          stop("Aborting download")
        } else {
          message("Continuing with existing files.")
          return(app_dir)
        }
      }
    }

    # Download files
    for (filename in files_to_download) {
      destfile <- file.path(app_dir, filename)
      url <- urls[filename]

      if (verbose) {
        if (filename %in% updated_files) {
          message(sprintf("Updating %s...", filename))
        } else {
          message(sprintf("Downloading %s...", filename))
        }
      }

      download_file(url = url, destfile = destfile, verbose = verbose)

      # Update metadata
      metadata$urls[[filename]] <- url
      metadata$downloaded[[filename]] <- Sys.time()
    }

    # Save updated metadata
    saveRDS(metadata, metadata_file)

  } else {
    if (verbose) message("All files are already present")
  }

  return(app_dir)
}
