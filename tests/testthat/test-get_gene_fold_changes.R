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


test_that("get many fold changes", {
  # Each gene should produce 2 rows (one for each dataset), so two genes yields 4 rows
  result <- get_gene_fold_changes(skeletalvis_path, c("GeneA", "GeneB"))
  expect_equal(nrow(result), 4)
})

test_that("wrong gene name", {
  # Expect an error when a gene symbol is not present in the fold change table
  expect_error(get_gene_fold_changes(skeletalvis_path, "wrongGeneName"))
})

