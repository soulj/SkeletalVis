test_that("predownloaded file works", {
  expect_equal(download_skeletalvis(test_path("data", "testFoldChangeTable.feather")),
               "data/testFoldChangeTable.feather" )
})


