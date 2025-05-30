% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_skeletalvis.R
\name{load_skeletalvis}
\alias{load_skeletalvis}
\title{Load SkeletalVis data}
\usage{
load_skeletalvis(verbose = TRUE, ask = TRUE, demo = FALSE)
}
\arguments{
\item{verbose}{Logical. If TRUE, prints messages about file existence and download status. Default is TRUE}

\item{ask}{Logical. If TRUE, prompts the user for confirmation before downloading missing files. Default is TRUE.}

\item{demo}{Logical. If TRUE, uses built in demo data suitable for testing the package functions. Default is FALSE}
}
\value{
The path to the SkeletalVis data folder.
}
\description{
Checks the default SkeletalVis data location for the differential expression data and metadata files.
If any files are missing, it lists them and ask permission to download the missing files.
}
\examples{
# Load the demo SkeletalVis data.
skeletalvis <- load_skeletalvis(demo = TRUE)

# Download full dataset (only if running interactively)
if (interactive()) {
  skeletalvis <- load_skeletalvis()
}

}
