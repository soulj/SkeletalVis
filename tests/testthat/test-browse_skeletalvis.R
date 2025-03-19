library(testthat)
library(shiny)
library(DT)
library(mockery)

# Mock skeletalvis directory structure
test_that("browse_skeletalvis throws error for missing files", {
  skeletalvis <- withr::local_tempdir()
  expect_error(browse_skeletalvis(skeletalvis), "The file 'expTable.txt' does not exist in the specified directory.")

  # Create one of the required files but not the other
  write.csv(data.frame(a = 1:5), file = file.path(skeletalvis, "expTable.txt"), row.names = FALSE)
  expect_error(browse_skeletalvis(skeletalvis), "The file 'accessions.txt' does not exist in the specified directory.")

})


test_that("browse_skeletalvis returns simulated selected ID when required files exist", {
  skeletalvis <- withr::local_tempdir()

  # Create the required files in the temporary directory
  write.csv(data.frame(a = 1:5),
            file = file.path(skeletalvis, "expTable.txt"),
            row.names = FALSE)
  write.table(data.frame(accession = letters[1:5],
                         comparison = LETTERS[1:5]),
              file = file.path(skeletalvis, "accessions.txt"),
              sep = "\t",
              row.names = FALSE)

  # Define a fake version of shiny::runGadget that simulates a user selection.
  fake_runGadget <- function(ui, server, ...) {
    return("fake_selected_id")
  }

  # Stub out shiny::runGadget in the browse_skeletalvis function
  mockery::stub(browse_skeletalvis, "shiny::runGadget", fake_runGadget)

  # Call the function; it will use the stubbed shiny::runGadget and return "fake_selected_id"
  result <- browse_skeletalvis(skeletalvis)

  expect_equal(result, "fake_selected_id")
})
