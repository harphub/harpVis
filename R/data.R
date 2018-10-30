#' Ensemble verification scores example data.
#'
#' Example verification for 3 ensembles: MEPS operational and two experimental
#' MEPS ensembles - one with and one without surface perturbations.
#'
#' @format A list with 2 data frames: \describe{
#'   \item{ens_summary_scores}{Summary scores for the ensembles.}
#'   \item{ens_threshold_scores}{Categorical scores for the ensembles based on
#'   threshold exceedence categories.}}
#'
#' @source MetCoOp / HIRLAM
"ens_verif_data"

#' Deterministic verification scores example data.
#'
#' Example verification for 2 deterministic models: The control member of
#' operational MEPS and operational Arome Arctic. The scores are based on
#' stations that are common to the two domains for September 2018.
#'
#' @format A list with 2 data frames: \describe{
#'   \item{det_summary_scores}{Summary scores for the forecasts}
#'   \item{det_threshold_scores}{Categorical scores for the forecasts based on
#'   threshold exceedence categories.}}
#'
#' @source MetCoOp / HIRLAM
"det_verif_data"
