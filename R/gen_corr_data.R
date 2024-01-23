## Generate validation data ##

#' Generate validation data
#'
#' Generates a validation dataset
#'
#' @param flight A flight data frame
#' @param splines An OTM splines object generated with `gen_otm_splines`
#'
#' @return A validation dataset
#'
#' @export

gen_corr_data <- function(flight, splines){

  # function to determine decimal places
  decimalplaces <- function(x) {
    if (abs(x - round(x)) > .Machine$double.eps^0.5) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }

  # get splines for the same date the flight took place
  flight_splines <- splines %>%
    filter(date == mean(flight$date)) %>%
    dplyr::select(latitude, longitude, splines) %>%
    mutate(op_temp = rep(NA, nrow(.)))

  # get minute range when the flight took place
  min_range <- c(mean(flight$minute_start):mean(flight$minute_end))

  # loop to predict operative temperatures from spline models
  for(i in 1:nrow(flight_splines)){
    flight_splines$op_temp[i] <- mean(predict(flight_splines$splines[[i]], min_range)$y)
  }

  # estimate decimal digits from flight data
  digits <- decimalplaces(flight$latitude[1])

  # correct number of digits for latitude and longitude
  flight_splines$longitude <- as.numeric(format(flight_splines$longitude, nsmall = digits))
  flight_splines$latitude <- as.numeric(format(flight_splines$latitude, nsmall = digits))

  # build validation data
  validation_data <- merge(flight_splines, flight, by = c("latitude", "longitude"))

  # select columns of interest
  validation_data <- validation_data %>%
    mutate(minute = rep(mean(min_range), nrow(.))) %>%
    dplyr::select(latitude, longitude, minute, op_temp, ref_temp)

  return(validation_data)

}


