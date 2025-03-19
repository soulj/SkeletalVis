test_that("get similar dataset", {

  # Create a temporary directory to hold the self-contained test data
  skeletalvis_path <- withr::local_tempdir()

  # Create mock data for feather files
  foldchange_data <- data.frame(ID = c("GeneA", "GeneB"), "ExpA" = c(1.5, -2.3), "ExpB" = c(1.2, -2.1))
  pvalue_data <- data.frame(GeneName = c("GeneA", "GeneB"), "ExpA" = c(1.5, -2.3), "ExpB" = c(0.02, 0.6))
  exp_data <- data.frame(ID = c("ExpA","ExpB"), Description=c("A","B"))
  comparisons <- data.frame(accession=c("ExpA","ExpB"), comparison=c("A","B"), comparisonsText=c("A","B"))


  foldchange_path <- file.path(skeletalvis_path, "foldChangeTable.feather")
  pvalue_path <- file.path(skeletalvis_path, "pvalTable.feather")
  exp_data_path <- file.path(skeletalvis_path, "expTable.txt")
  comparisons_path <- file.path(skeletalvis_path, "accessions.txt")

  arrow::write_feather(foldchange_data, foldchange_path)
  arrow::write_feather(pvalue_data, pvalue_path)
  write.table(exp_data, file = exp_data_path, sep = ",", row.names = FALSE)
  write.table(comparisons, file = comparisons_path, sep = "\t", row.names = FALSE)

  query <- data.frame(ID=c("COL2A1","SOX9","COL1A1","ACAN","MMP3","MMP13"),
                      foldChange=c(1.3,1.2,0.5,-3,3,0.85)
                      )

  expect_no_error(experiment_similarity(skeletalvis_path, query))
})
