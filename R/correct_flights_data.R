
## Correct flights data ##

#' Correct flights data
#'
#' Correct flights data to transform IR-measured temperatures into operative temperatures
#'
#' @param flights_data A `tibble` of flights data obtained through the `rnp_flights_data`
#'    function. The `tibble` must contain columns for `longitude` and `latitude`.
#' @param otm_splines A complex `tibble` obtained using the `gen_otm_splines` function.
#'    The `tibble` must contain columns for `longitude` and `latitude` and all values
#'    must also be in the `flights_data`
#'
#' @return A processed `flights_data` `tibble` where IR-measured temperatures (`ir_temp`)
#'    has been corrected to operative temperatures (`op_temp`).
#'
#' @export

correct_flights_data <- function(flights_data, otm_splines){

  ## correlate data

  # add column to identify each flight based on doy and mod start
  flights_data$flight_id <- paste(flights_data$doy, flights_data$mod_start, sep = "_")

  # get list of unique flights
  flights_list <- unique(flights_data$flight_id)

  # function to determine decimal places
  decimalplaces <- function(x) {
    if (abs(x - round(x)) > .Machine$double.eps^0.5) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }

  # generate holder correlation data frame
  corr_data <- tibble(latitude = c(), longitude = c(), year = c(),
                      doy = c(), mod_start = c(), mod_end = c(), ir_temp = c(),
                      op_temp = c())

  # start loop to get correction data for each flight
  for(i in 1:length(flights_list)){

    # filter data for flight of interest
    flight <- flights_data %>% filter(flight_id == flights_list[i])

    # filter splines for the same doy as flight of interest
    flight_splines <- otm_splines %>% filter(doy == mean(flight$doy)) %>%
      mutate(op_temp = rep(NA, nrow(.)))

    # get the mod range when the flight took place
    mod_range <- c(mean(flight$mod_start):mean(flight$mod_end))

    # loop to predict operative temperatures from spline models
    for(j in 1:nrow(flight_splines)){

      flight_splines$op_temp[j] <- mean(predict(flight_splines$spline[[j]], mod_range)$y)

    }

    # estimate decimal digits from flight data
    digits <- decimalplaces(flight$latitude[1])

    # correct number of digits for latitude and longitude
    flight_splines$longitude <- as.numeric(format(flight_splines$longitude, nsmall = digits))
    flight_splines$latitude <- as.numeric(format(flight_splines$latitude, nsmall = digits))

    # generate correction data for the flight
    flight_corr_data <- merge(flight_splines,flight, by = c('latitude', 'longitude', 'year', 'doy')) %>%
      dplyr::select(latitude, longitude, year, doy, mod_start, mod_end, ir_temp, op_temp)

    # bind correlation data to
    corr_data <- rbind(corr_data, flight_corr_data)

  }

  ## get correction factors

  # year, date and time correction factors
  time_correction_factors <- corr_data %>%  group_by(year, doy, mod_start) %>%
    summarise(time_corr_factor = mean(op_temp - ir_temp))

  # merge date and time correction factors with correlation data
  corr_data <- as_tibble(merge(corr_data, time_correction_factors,
                               by = c("year", "doy", "mod_start"), all = TRUE))

  # apply time correction
  corr_data$ir_temp_corr <- corr_data$ir_temp + corr_data$time_corr_factor

  # fit linear model with corrected ir temperature data
  model <- lm(corr_data$ir_temp_corr ~ corr_data$op_temp)

  ## apply correction

  # merge flights data with time correction factors
  flights_data <- as_tibble(merge(flights_data, time_correction_factors),
                            by = c('year', 'doy', 'mod_start'), all = TRUE)

  # apply time correction
  flights_data$op_temp <- flights_data$ir_temp + flights_data$time_corr_factor

  # apply temperature correction
  flights_data$op_temp <- -model$coefficients[1] + (2 - model$coefficients[2])*flights_data$op_temp

  # select columns of interest
  flights_data <- flights_data %>%
    dplyr::select(longitude, latitude, year, doy, mod_start, mod_end, op_temp)

  return(flights_data)

}


