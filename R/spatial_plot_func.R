#' Pick appropriate functions for spatial scores
#'
#' @param score_name A string containing the name
#'   of the score calculated from \code{verify_spatial}.
#'   This will return the appropriate plotting function
#'   to \code{plot_spatial_verif}.

spatial_plot_func <- function(
    score_name,
    ...) {

    result <- switch(score_name,
        "bias"   = "plot_spatial_line",
        "mse"    = "plot_spatial_line",
        "mae"    = "plot_spatial_line",
        "SAL"    = "plot_spatial_sal",
        "FSS"    = "plot_spatial_fss",
        "NACT"   = "plot_spatial_nact",
        "hira_me"    = "plot_spatial_nact",
        #"hira_pragm" = "plot_spatial_fss", #TODO: coming soon
        #"hira_crss"  = "plot_spatial_fss", #TODO: coming soon
        "hira_td"   = "plot_spatial_nact",
        stop("Unknown score_name ", score_name)
    )
    result
}
