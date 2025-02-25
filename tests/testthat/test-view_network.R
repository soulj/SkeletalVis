# Create a temporary directory and sample network for testing
setup_test_network <- function() {
  temp_dir <- tempdir()

  # Create a simple test network (ring graph)
  network <- igraph::make_star(5,mode = "undirected")
  V(network)$name <- c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE")
  saveRDS(network, file.path(temp_dir, "network.RDS"))  # Save network

  # Create a test OATargets dataset and save it
  oatargets <- data.frame(
    Gene = c("GeneA", "GeneB", "GeneC"),
    effectConsensus = c("Protective", "Detrimental", "No effect")
  )
  write.table(oatargets, file = file.path(temp_dir, "oatargets.txt"),quote = FALSE, row.names = FALSE, col.names = TRUE, sep="\t")

  return(temp_dir)
}

test_that("view_network correctly loads network file", {
  skeletalvis <- setup_test_network()
  expect_error(view_network("invalid_path", "GeneA"), "The file 'network.RDS' does not exist")
})

test_that("view_network handles missing gene", {
  skeletalvis <- setup_test_network()
  expect_error(view_network(skeletalvis, "NonExistentGene"), "The gene  NonExistentGene does not exist in the network")
})

test_that("view_network returns a visNetwork object", {
  skeletalvis <- setup_test_network()
  vis <- view_network(skeletalvis, "GeneA")
  expect_true(inherits(vis, "visNetwork"))
})

test_that("view_network correctly filters nodes when hide_unannotated is TRUE", {
  skeletalvis <- setup_test_network()
  vis <- view_network(skeletalvis, "GeneA", hide_unannotated = TRUE)

  # Extract node data
  nodes <- vis$x$nodes
  expect_false(any(nodes$effect == "not measured")) # Ensure no "not measured" nodes are present
})

test_that("view_network correctly assigns colors", {
  skeletalvis <- setup_test_network()
  vis <- view_network(skeletalvis, "GeneA")

  nodes <- vis$x$nodes
  expect_true("#009E73" %in% nodes$color.background)  # Protective
  expect_true("#D55E00" %in% nodes$color.background)  # Detrimental
  expect_true("#56B4E9" %in% nodes$color.background)  # No effect
})

test_that("view_network assigns default color to unmeasured nodes", {
  skeletalvis <- setup_test_network()
  vis <- view_network(skeletalvis, "GeneA")

  nodes <- vis$x$nodes
  expect_true(any(nodes$color.background == "white"))  # Unmeasured nodes should be white
})
