#' Test dataset of PK Ratios for 40 subjects.
#'
#'
#' @format A data frame with 40 rows and 11 variables:
#' \describe{
#'   \item{IndividualID}
#'   \item{Population}
#'   \item{Gender}
#'   \item{Age}
#'   \item{Compound}
#'   \item{Dose}
#'   \item{Organ}
#'   \item{Compartment}
#'   \item{Simulated}
#'   \item{Observed}
#'   \item{Ratio}
#' }
"pkRatioData"

#' Test metaData for pkRatioData
#'
#'
#' @format A list of lists for the 11 variables of pkRatioData
#' Each variable includes:
#' \describe{
#'   \item{unit}
#'   \item{dimension}
#' }
"pkRatioMetaData"
