test_that("get single gene fold change", {
  expect_no_error(get_gene_fold_changes(test_path("data", "testFoldChangeTable.feather"),"COL2A1"))
})

test_that("get many fold changes", {
  expect_no_error(get_gene_fold_changes(test_path("data", "testFoldChangeTable.feather"),c("COL2A1","SOX9")))
})

test_that("wrong gene name", {
  expect_error(get_gene_fold_changes(test_path("data", "testFoldChangeTable.feather"),"wrongGeneName"))
})
