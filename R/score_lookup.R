#'Convert a score from a long name to a short name, or vice versa
#'
#'\code{score_lookup} looks for the input in a dictionary - if it corresponds to
#'a short name, the long name is returned, and if it corresponds to a long name,
#'the short name is returned. The argument is returned unchanged if it is not
#'found in the dictionary. If no argument is given, the dictionary is returned.
#'
#'@param x The name of the score to look up
#'
#'@return The short name or long name of the score
#'@export
#'
#' @examples
#' score_lookup("SEDI")
#' score_lookup("mr")
#' score_lookup("heidke_skill_score")
score_lookup <- function(x) {

  dictionary <- tibble::tribble(
    ~long_name, ~short_name,
    "threat_score"                                   , "ts",
    "hit_rate"                                       , "hr",
    "miss_rate"                                      , "mr",
    "false_alarm_rate"                               , "far",
    "false_alarm_ratio"                              , "fart",
    "heidke_skill_score"                             , "hss",
    "pierce_skill_score"                             , "pss",
    "kuiper_skill_score"                             , "kss",
    "percent_correct"                                , "pc",
    "frequency_bias"                                 , "fb",
    "equitable_threat_score"                         , "ets",
    "odds_ratio"                                     , "or",
    "log_odds_ratio"                                 , "lor",
    "odds_ratio_skill_score"                         , "orss",
    "extreme_dependency_score"                       , "eds",
    "symmetric_eds"                                  , "seds",
    "extreme_dependency_index"                       , "edi",
    "symmetric_edi"                                  , "sedi",
    "spread_skill_ratio"                             , "ssr",
    "crps_reliability"                               , "crps_rel",
    "crps_potential"                                 , "crps_pot",
    "fair_brier_score"                               , "fair_bs",
    "brier_score"                                    , "bs",
    "brier_skill_score"                              , "bss",
    "brier_score_reliability"                        , "bs_rel",
    "brier_score_resolution"                         , "bs_res",
    "brier_score_uncertainty"                        , "bs_unc"
  )

  if (missing(x)) {
    print(dictionary, n = nrow(dictionary))
    return(invisible(dictionary))
  }

  if (tolower(x) %in% dictionary[["long_name"]]) {
    dictionary[["short_name"]][dictionary[["long_name"]] == tolower(x)]
  } else if (tolower(x) %in% dictionary[["short_name"]]) {
    dictionary[["long_name"]][dictionary[["short_name"]] == tolower(x)]
  } else {
    x
  }
}
