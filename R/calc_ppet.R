#' Calculate Climatic Water Balance (P-PET)
#'
#' Calculates the climatic water balance as Precipitation minus
#' Potential Evapotranspiration. Negative values indicate water deficit.
#'
#' @param precipitation Numeric vector. Precipitation values (mm).
#' @param pet Numeric vector. Potential evapotranspiration values (mm).
#'   Same length as precipitation.
#'
#' @return Numeric vector of P-PET values (mm). Negative = deficit.
#'
#' @examples
#' precip <- c(98, 27, 23.5, 79.5, 58)
#' pet <- c(37.5, 46.3, 68.7, 103.1, 110.2)
#' water_balance <- calc_ppet(precip, pet)
#'
#' @export
calc_ppet <- function(precipitation, pet) {

  if (length(precipitation) != length(pet)) {
    stop("precipitation and pet must have the same length")
  }

  p_pet <- precipitation - pet

  return(p_pet)
}


#' Calculate Cumulative Water Balance
#'
#' Calculates cumulative P-PET over a time period, useful for
#' assessing drought severity.
#'
#' @param precipitation Numeric vector. Precipitation values (mm).
#' @param pet Numeric vector. Potential evapotranspiration values (mm).
#' @param reset_negative Logical. Reset cumulative sum to 0 when it goes
#'   negative? Default FALSE (allows negative cumsum for drought tracking).
#'
#' @return Numeric vector of cumulative P-PET values.
#'
#' @examples
#' precip <- c(98, 27, 23.5, 79.5, 58)
#' pet <- c(37.5, 46.3, 68.7, 103.1, 110.2)
#' cum_balance <- calc_ppet_cumulative(precip, pet)
#'
#' @export
calc_ppet_cumulative <- function(precipitation, pet, reset_negative = FALSE) {

  p_pet <- calc_ppet(precipitation, pet)

  if (reset_negative) {
    # Reset to 0 when negative (bucket model)
    cum_ppet <- numeric(length(p_pet))
    cum_ppet[1] <- max(0, p_pet[1])
    for (i in 2:length(p_pet)) {
      cum_ppet[i] <- max(0, cum_ppet[i-1] + p_pet[i])
    }
  } else {
    cum_ppet <- cumsum(p_pet)
  }

  return(cum_ppet)
}


#' Add P-PET Column to Data Frame
#'
#' @param data A data frame.
#' @param precip_col Character. Name of precipitation column.
#' @param pet_col Character. Name of PET column.
#' @param new_col Character. Name for new P-PET column. Default "P_PET".
#'
#' @return Data frame with P-PET column added.
#' @export
add_ppet <- function(data, precip_col, pet_col, new_col = "P_PET") {
  data[[new_col]] <- calc_ppet(data[[precip_col]], data[[pet_col]])
  return(data)
}
