test_that("get similar dataset", {

  query <- data.frame(ID=c("COL2A1","SOX9","COL1A1","ACAN","MMP3","MMP13"),
                      foldChange=c(1.3,1.2,0.5,-3,3,0.85)
                      )
  foldChangeTable <- feather::read_feather(test_path("data", "testFoldChangeTable.feather"))
  expect_no_error(experiment_similarity(query,foldChangeTable))
})
