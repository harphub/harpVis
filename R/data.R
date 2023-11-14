#' Ensemble verification scores example data.
#'
#' Example verification for 2 ensembles: meps and meps_obs_error. meps is the
#' operational NWP ensemble for the METCOOP consortium, and meps_obs_error is
#' the same data, but each ensemble member is perturbed to simulate the
#' impact of observation errors on the verification. The scores are based on
#' forecasts for 2 days in August 2023.
#'
#' @format A `harp_verif` list with 3 data frames: \describe{
#'   \item{ens_summary_scores}{Summary scores for the ensembles.}
#'   \item{ens_threshold_scores}{Categorical scores for the ensembles based on
#'   threshold exceedence categories.}
#'   \item{det_summary_scores}{Deterministic summary scores for each ensemble
#'   member}}
#'
#' @source MetCoOp
"verif_data_ens"

#' Deterministic verification scores example data.
#'
#' Example verification for 2 deterministic models: The control member of
#' operational MEPS and operational Arome Arctic. The scores are based on
#' stations that are common to the two domains for 2 days in August 2023.
#'
#' @format A `harp_verif` list with 2 data frames: \describe{
#'   \item{det_summary_scores}{Summary scores for the forecasts}
#'   \item{det_threshold_scores}{Categorical scores for the forecasts based on
#'   threshold exceedence categories.}}
#'
#' @source MetCoOp / MET Norway
"verif_data_det"

#' Grouped verification scores example data
#'
#' Example verification data grouped by station_group and type. The forecasts
#' are as \code{\link{verif_data_ens}}, but the verification is split into
#' groups.
#'
#' @format A `harp_verif` list with 1 data frame: \describe{
#'   \item{ens_summary_scores}{Summary scores for the ensembles.}}
#'
#' @source MetCoOp
"verif_data_grp"

#' Deterministic verification scores for vertical profile example data.
#'
#' Example verification for 2 deterministic models: The control member of
#' operational MEPS and operational Arome Arctic. The scores are based on
#' stations and pressure levels that are common to the two domains for 2 days in
#' August 2023.
#'
#' @format A `harp_verif` list with 1 data frame: \describe{
#'   \item{det_summary_scores}{Summary scores for the forecasts}}
#'
#' @source MetCoOp / MET Norway
"verif_data_prf"
