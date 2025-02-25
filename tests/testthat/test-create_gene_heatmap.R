
# Sample dataset similar to the output of get_gene_fold_changes
sample_data <- data.frame(
  datasetID = c("E-GEOD-69110_1", "GSE155118_1", "GSE213277_1", "GSE155118_1","E-GEOD-69110_1","GSE213277_1"),
  Gene = c("SOX9", "SOX9", "ACAN", "ACAN","ACAN","SOX9"),
  log2FoldChange = c(7.15, 3.11, -5.98, 4.47,2,-3),
  FDR = c(0.001, 0.002, 0.003, 0.004,0.1,1),
  Accession = c("E-GEOD-69110", "GSE155118", "GSE213277", "GSE155118","E-GEOD-69110","GSE213277"),
  Species = c("Human", "Mouse", "Human", "Mouse","Cow","Horse"),
  Tissue = c("Cartilage", "Cartilage", "Stem Cell", "Cartilage","Bone","Tendon"),
  ExpType = c("Gene perturbation", "Gene perturbation", "Gene perturbation", "Gene perturbation", "Gene perturbation", "Gene perturbation")
)

test_that("Error if value_column does not exist in data", {
  expect_error(
    create_gene_heatmap(
      data = sample_data,
      value_column = "NonExistentColumn",
      annotation_columns = c("Species", "Tissue"),
      heatmap_title = "Test Heatmap"
    ),
    "Specified value column does not exist in the data."
  )
})

test_that("Error if any annotation_columns do not exist in data", {
  expect_error(
    create_gene_heatmap(
      data = sample_data,
      value_column = "log2FoldChange",
      annotation_columns = c("Species", "NonExistentColumn"),
      heatmap_title = "Test Heatmap"
    ),
    "One or more annotation columns do not exist in the data."
  )
})

test_that("Output is a ComplexHeatmap Heatmap object", {
  # Capture output
  heatmap_obj <- create_gene_heatmap(
    data = sample_data,
    value_column = "log2FoldChange",
    annotation_columns = c("Species", "Tissue"),
    heatmap_title = "Test Heatmap"
  )

  # Check that it returns a Heatmap object
  expect_true(inherits(heatmap_obj, "Heatmap"))
})
