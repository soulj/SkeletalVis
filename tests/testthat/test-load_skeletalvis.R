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
             "foldChangeTable.feather","network.RDS","oatargets.txt")

  lapply(files, function(x) file.create(file.path(local_dir, x)))

  expect_message(load_skeletalvis(verbose = TRUE, ask = FALSE), "Data already exists", all = FALSE)
})

test_that("load_skeletalvis downloads missing files", {
  local_dir <- withr::local_tempdir()

  # Mock the application data directory
  stub(load_skeletalvis, "tools::R_user_dir", function(...) local_dir)

  # Mock the download function to avoid real HTTP requests
  mock_download <- mock(file.path(local_dir, "accessions.txt"))
  stub(check_and_download_file, "httr::GET", mock_download)

  files <- c("expTable.txt","pvalTable.feather",
             "foldChangeTable.feather","network.RDS","oatargets.txt")

  lapply(files, function(x) file.create(file.path(local_dir, x)))

  expect_message(load_skeletalvis(verbose = TRUE, ask = FALSE), "Downloading", all = FALSE)
})

test_that("load_skeletalvis runs silently when verbose=FALSE", {

  # Creates a temporary directory for the test
  local_dir <- withr::local_tempdir()

  # Mock the application data directory
  stub(load_skeletalvis, "tools::R_user_dir", function(...) local_dir)

  # Create a dummy files to simulate existing data
  files <- c("accessions.txt","expTable.txt","pvalTable.feather",
             "foldChangeTable.feather","network.RDS","oatargets.txt")

  lapply(files, function(x) file.create(file.path(local_dir, x)))

  expect_silent(load_skeletalvis(verbose = FALSE, ask = FALSE))
})
