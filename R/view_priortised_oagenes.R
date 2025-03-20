#' View prioritised osteoarthritis associated genes
#'
#' Retrieves the machine learning model prioritised table of osteoarthritis joint damage in animals.
#' Genes are ranked by their probability to cause an OA damage phenotype when experimentally perturbed.
#' The predicted direction of effect and druggability of that gene is also provided.
#'
#' @param skeletalvis The path to the SkeletalVis folder.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{Gene}{The human gene symbol.}
#'   \item{Rank}{The rank of the priortisation scores from the machine learning model}
#'   \item{PredictedEffect}{The predicted effect this gene}
#'   \item{Category_sm}{The small molecular tractibility of this gene}
#'   \item{Category_ab}{The antibody tractibility of this gene}
#' }
#'
#'
#' @examples
#' skeletalvis <- load_skeletalvis(demo=TRUE)
#' oatargets <- view_prioritised_oagenes(skeletalvis)
#'
#' @export

view_prioritised_oagenes <- function(skeletalvis) {

  oagenes_filepath <- file.path(skeletalvis, "oatargets_prioritised.txt")

  if (!file.exists(oagenes_filepath)) stop(sprintf("The file 'oatargets_prioritised.txt' does not exist in the specified directory: %s", skeletalvis))

  oagenes <- read.delim(oagenes_filepath)

  return(oagenes)
}
