## Apply correction ##

#' Apply correction
#'
#' Applies a correction to an existing flight data frame to transform it into operative temperature
#'
#' @param flight_data A flight data frame with metadata that can be a collection of multiple flights binded together.
#' @param correction_factors A correction factor list obtain used the `get_correction_factors` function.
#'
#' @return The same flight data with an operative temperature column
#'
#' @export

apply_correction <- function(flight_data, correction_factors){

  # generate a new dataset
  fdf <- flight_data

  # generate a new minute column to match correction data
  fdf$minute <- fdf$minute_start

  # merge fdf with year, date and time correction factors
  fdf <- merge(fdf, correction_factors[[1]], by = c("year", "date", "minute"), all = TRUE)

  # apply year, date, time correction factor
  fdf$corr1 <- fdf$ref_temp + fdf$correction_factor

  # apply temperature correction
  fdf$corr2 <- - correction_factors[[2]] + (2 - correction_factors[[3]])*fdf$corr1

  # generate operative temperature column
  flight_data$op_temp <- fdf$corr2

  return(flight_data)

}
