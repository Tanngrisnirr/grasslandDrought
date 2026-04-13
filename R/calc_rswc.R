#' Calculate Relative Soil Water Content (RSWC)
#'
#' Calculates RSWC from soil water content measurements using the formula:
#' RSWC = (SWC - SWCmin) / (SWCmax - SWCmin)
#'
#' This normalization allows comparison across sites with different soil types.
#' Based on methodology from Picon-Cochard et al. (2021).
#'
#' @param swc Numeric vector. Soil water content values (m3/m3 or %).
#' @param swc_min Numeric. Minimum SWC observed (driest condition).
#'   If NULL, uses min(swc).
#' @param swc_max Numeric. Maximum SWC observed (field capacity).
#'   If NULL, uses max(swc).
#' @param na.rm Logical. Remove NA values when computing min/max? Default TRUE.
#'
#' @return Numeric vector of RSWC values (0-1 scale).
#'
#' @examples
#' # From raw soil moisture data
#' swc <- c(0.15, 0.22, 0.31, 0.18, 0.25)
#' rswc <- calc_rswc(swc)
#'
#' # With known field capacity and wilting point
#' rswc <- calc_rswc(swc, swc_min = 0.10, swc_max = 0.35)
#'
#' @export
calc_rswc <- function(swc, swc_min = NULL, swc_max = NULL, na.rm = TRUE) {

  if (is.null(swc_min)) {
    swc_min <- min(swc, na.rm = na.rm)
  }
  if (is.null(swc_max)) {
    swc_max <- max(swc, na.rm = na.rm)
  }

  if (swc_max == swc_min) {
    warning("swc_max equals swc_min, returning NA")
    return(rep(NA_real_, length(swc)))
  }

  rswc <- (swc - swc_min) / (swc_max - swc_min)

  return(rswc)
}


#' Add RSWC Column to Data Frame
#'
#' Convenience function to add RSWC as a new column to a data frame.
#'
#' @param data A data frame.
#' @param swc_col Character. Name of the SWC column.
#' @param new_col Character. Name for the new RSWC column. Default "RSWC".
#' @param ... Additional arguments passed to calc_rswc().
#'
#' @return Data frame with RSWC column added.
#'
#' @examples
#' \dontrun{
#' data <- add_rswc(data, swc_col = "SoilWaterContent")
#' }
#'
#' @export
add_rswc <- function(data, swc_col, new_col = "RSWC", ...) {
  data[[new_col]] <- calc_rswc(data[[swc_col]], ...)
  return(data)
}
