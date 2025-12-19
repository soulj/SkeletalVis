library(testthat)
library(withr)
library(mockery)

test_that("load_skeletalvis returns demo folder when demo=TRUE", {
  demo_path <- load_skeletalvis(demo = TRUE)
  expect_true(dir.exists(demo_path))
})

test_that("load_skeletalvis does not download files if they exist", {
  local_dir <- withr::local_tempdir()  # Creates a temporary directory for the test

  # Mock the application data directory
  stub(load_skeletalvis, "tools::R_user_dir", function(...) local_dir)


  # Create a dummy files to simulate existing data
  files <- c("accessions.txt","expTable.txt","pvalTable.feather",
             "foldChangeTable.feather","network.RDS","oatargets.txt","oatargets_prioritised.txt")

  lapply(files, function(x) file.create(file.path(local_dir, x)))

  # Create metadata file to simulate previously downloaded data
  urls <- c("https://www.dropbox.com/scl/fi/4ckfh1im2q6pjxk8hp5fa/accessions.txt?rlkey=yd92tuvnqrswapa9h750dpfa6&st=86a425y0&dl=1",
            "https://www.dropbox.com/scl/fi/175m6gy3jgc20gd4s7doi/expTable.txt?rlkey=e9i2roq11hhz1z00nieymmjgw&st=ohnvotz3&dl=1",
            "https://www.dropbox.com/scl/fi/zbparwfen1q8s91gxuz7l/pvalTable.feather?rlkey=88uuykvn405or12fvuygjsp7k&st=90awp8t4&dl=1",
            "https://www.dropbox.com/scl/fi/0teuruu92odvqmc1pdsdx/foldChangeTable.feather?rlkey=mkxjvpbcw9fjqfz06c74hvegl&st=ux11svzr&dl=1",
            "https://www.dropbox.com/scl/fi/gugx8t8ckinfxv9u0y82a/network.RDS?rlkey=s7n1egn2z5meco1gc48lrnoq1&st=2i7l5eum&dl=1",
            "https://www.dropbox.com/scl/fi/0435zvm67wqmum6fiz224/oatargets.txt?rlkey=oav851eppxdao9ixv9zx42ly6&st=wr63lfou&dl=1",
            "https://www.dropbox.com/scl/fi/xp9n8peak0czv5pkefozr/oatargets_prioritised.txt?rlkey=mdp1jlnf6ueckqe93pj1kh9ab&st=uo7wa5v1&dl=1")
  names(urls) <- files

  metadata <- list(
    urls = as.list(urls),
    downloaded = setNames(replicate(length(files), Sys.time(), simplify = FALSE), files)
  )
  saveRDS(metadata, file.path(local_dir, ".skeletalvis_metadata.rds"))

  expect_message(load_skeletalvis(verbose = TRUE, ask = FALSE), "All files are already present", all = FALSE)
})

test_that("load_skeletalvis downloads missing files", {
  local_dir <- withr::local_tempdir()

  # Mock the application data directory
  stub(load_skeletalvis, "tools::R_user_dir", function(...) local_dir)

  # Mock the download function to avoid real HTTP requests
  mock_download <- mock(file.path(local_dir, "accessions.txt"))
  stub(download_file, "httr::GET", mock_download)

  files <- c("expTable.txt","pvalTable.feather",
             "foldChangeTable.feather","network.RDS","oatargets.txt", "oatargets_prioritised.txt")

  lapply(files, function(x) file.create(file.path(local_dir, x)))

  expect_message(load_skeletalvis(verbose = TRUE, ask = FALSE), "accessions.txt", all = FALSE)
})

test_that("load_skeletalvis runs silently when verbose=FALSE", {

  # Creates a temporary directory for the test
  local_dir <- withr::local_tempdir()

  # Mock the application data directory
  stub(load_skeletalvis, "tools::R_user_dir", function(...) local_dir)

  # Create a dummy files to simulate existing data
  files <- c("accessions.txt","expTable.txt","pvalTable.feather",
             "foldChangeTable.feather","network.RDS","oatargets.txt","oatargets_prioritised.txt")

  lapply(files, function(x) file.create(file.path(local_dir, x)))

  # Create metadata file to simulate previously downloaded data
  urls <- c("https://www.dropbox.com/scl/fi/4ckfh1im2q6pjxk8hp5fa/accessions.txt?rlkey=yd92tuvnqrswapa9h750dpfa6&st=86a425y0&dl=1",
            "https://www.dropbox.com/scl/fi/175m6gy3jgc20gd4s7doi/expTable.txt?rlkey=e9i2roq11hhz1z00nieymmjgw&st=ohnvotz3&dl=1",
            "https://www.dropbox.com/scl/fi/zbparwfen1q8s91gxuz7l/pvalTable.feather?rlkey=88uuykvn405or12fvuygjsp7k&st=90awp8t4&dl=1",
            "https://www.dropbox.com/scl/fi/0teuruu92odvqmc1pdsdx/foldChangeTable.feather?rlkey=mkxjvpbcw9fjqfz06c74hvegl&st=ux11svzr&dl=1",
            "https://www.dropbox.com/scl/fi/gugx8t8ckinfxv9u0y82a/network.RDS?rlkey=s7n1egn2z5meco1gc48lrnoq1&st=2i7l5eum&dl=1",
            "https://www.dropbox.com/scl/fi/0435zvm67wqmum6fiz224/oatargets.txt?rlkey=oav851eppxdao9ixv9zx42ly6&st=wr63lfou&dl=1",
            "https://www.dropbox.com/scl/fi/xp9n8peak0czv5pkefozr/oatargets_prioritised.txt?rlkey=mdp1jlnf6ueckqe93pj1kh9ab&st=uo7wa5v1&dl=1")

  names(urls) <- files

  metadata <- list(
    urls = as.list(urls),
    downloaded = setNames(replicate(length(files), Sys.time(), simplify = FALSE), files)
  )
  saveRDS(metadata, file.path(local_dir, ".skeletalvis_metadata.rds"))

  expect_silent(load_skeletalvis(verbose = FALSE, ask = FALSE))
})
