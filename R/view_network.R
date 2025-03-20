#' View the OATarget gene network
#'
#' This function visualises the protein-protein interaction network surrounding a specified gene, highlighting those genes that have been experimentally pertubated and the resulting effect of OA.
#'
#' @param skeletalvis A string specifying the path to the directory containing the "network.RDS" file.
#' @param query The name of the gene to visualise within the network.
#' @param hide_unannotated A logical value indicating whether to hide nodes with no measured effect (default: TRUE).
#' @param physics A logical value indicating whether to enable physics for the network (default: TRUE).
#'
#' @return A visNetwork interactive network'
#'
#'
#' @details
#' The function extracts the subnetwork surrounding the specified gene.
#' Nodes are coloured based on their effect ("Protective", "Detrimental", "Ambiguous", "No effect", or "Not measured").
#' If `hide_unannotated = TRUE`, nodes not yet studied are removed unless they are the target gene.
#'
#' @examples
#' # Specify the path to the skeletalvis directory
#' skeletalvis <- load_skeletalvis(demo=TRUE)
#'
#' # Visualize the network for a specific gene
#' vis <- view_network(skeletalvis, "COL2A1")
#'
#' # Show unannotated nodes
#' vis_filtered <- view_network(skeletalvis, "COL2A1", hide_unannotated = FALSE)
#'
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph V<-
#' @importFrom igraph E<-
#' @export
#'
view_network <- function(skeletalvis, query, hide_unannotated=TRUE, physics = TRUE) {


    network_filepath <- file.path(skeletalvis, "network.RDS")

    if (!file.exists(network_filepath)) stop(sprintf("The file 'network.RDS' does not exist in the specified directory: %s",skeletalvis))

    network <- readRDS(network_filepath)

    if(!query %in% V(network)$name) {
      stop(sprintf("The gene  %s does not exist in the network", query))
      return(NULL)
    }

    oatargets <- SkeletalVis::view_curated_oagenes(skeletalvis)

    oatargets <- oatargets[na.omit(match(V(network)$name,oatargets$Gene)),]

    V(network)$effect <- "not measured"
    V(network)$effect[ na.omit(match(oatargets$Gene,V(network)$name))] <- oatargets[ na.omit(match(V(network)$name,oatargets$Gene)),"effectConsensus"]


    nodes <- igraph::neighborhood(network, nodes = which(V(network)$name==query),order = 1)[[1]]

    if(hide_unannotated){

      keep <- which(V(network)[nodes]$effect!="not measured" | V(network)[nodes]$name==query)
      if(length(keep)==1){
        warning("No interaction partners with known effects, returning all partners")
      } else{
        nodes <- nodes[keep]
      }
    }

  subnetwork <- igraph::induced_subgraph(graph = network, vids = nodes)

  nodes <- data.frame(id = V(subnetwork)$name,
                      effect=V(subnetwork)$effect,
                      label = V(subnetwork)$name,
                      size = 10,
                      font.size = 18,
                      hidden=FALSE)

  nodes <- nodes %>%
  dplyr::mutate(color.background = dplyr::case_when(
      V(subnetwork)$effect == "not measured" ~ "white",
      V(subnetwork)$effect == "Protective" ~ "#009E73",
      V(subnetwork)$effect == "Ambiguous" ~ "#F0E442",
      V(subnetwork)$effect == "No effect" ~ "#56B4E9",
      TRUE ~ "#D55E00"
    ))

  nodes$size <- ifelse(V(subnetwork)$effect =="not measured", 5, 10)

  E(subnetwork)$colour <- "black"

  edges <- as.data.frame(igraph::as_edgelist(subnetwork))
  edges <- data.frame(from = edges[, 1], to = edges[, 2],id = paste0(edges[, 1],"/",edges[, 2]),
                      color = E(subnetwork)$colour,size=5,hidden=FALSE)

  if(igraph::vcount(subnetwork)>20) edges$size=3

  legendNodes <- data.frame(
    label = c("Not measured","Protective","Detrimental","Ambiguous","No effect"),
    color.background = c("white","#009E73","#D55E00","#F0E442","#56B4E9"),
    color.border ="black",
    font.size=35
  )

  network <- visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visLegend(width = 0.2, position = "left", useGroups = FALSE, addNodes = legendNodes) %>%
    visNetwork::visEdges(smooth = FALSE) %>%
    visNetwork::visLayout(improvedLayout = TRUE)

  if(!physics){
    network <- visNetwork::visPhysics(network, enabled = FALSE)
  }

  return(network)

}
