#' Filter spatial scores to common cases only (based on parameter, forecast
#' start data, and leadtime). Used only as part of \code{plot_spatial_verif}.
#'
#' @param verif_data Output from \link[harpSpatial]{spatial_verify}. Must be a 
#' data frame. 

spatial_common_cases <- function(verif_data) {
  
  # add string of prm_fcdata_leadtime 
  if (!("ccs" %in% names(verif_data))) {
    verif_data <- verif_data %>% dplyr::mutate(ccs = paste(prm,fcdate,leadtime,sep="_"))
  }
  
  # Get ccs common to all models
  ccs_vec <- NULL
  for (cm in unique(verif_data$model)) {
    ccs <- verif_data %>% dplyr::filter(model == cm) %>% select(ccs)
    ccs <- unique(ccs$ccs)
    if (is.null(ccs_vec)) {
      ccs_vec <- ccs
    } else {
      ccs_vec <- base::intersect(ccs_vec,ccs)
    }
  }
  
  # Then filter to these common ccs
  verif_data <- verif_data %>% filter(ccs %in% ccs_vec) %>% select(-ccs)
  
  if (nrow(verif_data) == 0) {
    stop("No common data found in spatial data frame")
  }
  
  return(verif_data)
}
