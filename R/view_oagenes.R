#' View Osteoarthritis damage associated genes
#'
#' This function retrieves the OATargets table of curated osteoarthritis damage associated genes
#'
#' @param skeletalvis The path to the skeletalvis folder.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{PMID}{The PubMed ID of the paper describing the observation.}
#'   \item{Gene}{The human gene symbol of the gene modulated in the study.}
#'   \item{Effect on gene product}{The effect of the gene modulation on the gene product.}
#'   \item{Model}{The type of OA model where the gene has been modulated.}
#'   \item{Susceptibility observed}{Increase (detrimental) or decrease (protective) in OA severity upon gene modulation.}
#'   \item{Inferred gene effect}{Based on the modulation effect, whether the gene is protective or detrimental in OA.}
#'   \item{Delivery}{The tissue specificity of the gene modulation.}
#'   \item{Species}{The species under study.}
#'   \item{pub_date}{The date of publication (epub).}
#'   \item{LastAuthor}{The last author of the publication.}
#'   \item{Type}{Genetic or exogenous modulation.}
#'   \item{Intervention}{For exogenous modulations, the method of gene modulation.}
#'   \item{simpleModel}{The broad type of OA model where the gene has been modulated.}
#'   \item{effectConsensus}{Consensus inferred gene effect across observations; 'ambiguous' if discrepancies exist.}
#'   \item{NumStudies}{The number of publications studying each gene.}
#' }
#'
#'
#' @examples
#' skeletalvis <- load_skeletalvis()
#' oatargets <- view_oagenes(skeletalvis)
#'
#' @export

view_oagenes <- function(skeletalvis) {

  oagenes_filepath <- file.path(skeletalvis, "oatargets.txt")

  if (!file.exists(oagenes_filepath)) stop(sprintf("The file 'oatargets.txt' does not exist in the specified directory: %s",skeletalvis))

  oagenes <- read.delim(oagenes_filepath)

  return(oagenes)
}
