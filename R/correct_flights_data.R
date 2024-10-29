
## Correct flights data ##

#' Correct flights data
#'
#' Corrects flights data to transform IR-measured temperatures into operative temperatures
#'
#' @param flights_data A \code{tibble} of flights data obtained through the \code{rnp_flights_data}
#'    function. The \code{tibble} must contain columns for `longitude` and `latitude`.
#' @param otm_splines A nested \code{tibble} obtained using the \code{gen_otm_splines} function.
#'    The \code{tibble} must contain columns for \code{longitude} and \code{latitude} and all values
#'    must also be in the \code{flights_data}
#' @param time_correction A logical value (\code{TRUE} or \code{FALSE})
#'    indicating whether first correction step should be applied. If set to \code{TRUE},
#'    the function will add the mean, median or mode (based on \code{time_correction_metric},
#'    see below) bias between all surface and operative temperatures in tiles where
#'    OTMs were present to all surface temperature measurements within a flight.
#'    The goal of applying a time correction is to account for time-specific systematic
#'     biases between operative and surface temperatures which may be a by product of the
#'    inherent nature of thermal imaging (e.g., measurements being influenced by
#'    light conditions, sensor calibration. etc.). We recommend that
#'    \code{time_correction = TRUE} if \code{flight_specific_correction = FALSE}.
#' @param time_correction_metric A character string indicating the metric to be
#'    used to perform the time correction. The metric can be either \code{"mean"},
#'    \code{"median"} or \code{"mode"}.
#' @param flight_specific_correction A logical value (\code{TRUE} or \code{FALSE})
#'    indicating whether the second correction step should be applied to all flights
#'    or each flight specifically. If set to \code{TRUE} the second correction
#'    step will also incorporate the effects of day of the year and minute of day
#'    into the linear model from which the correction will be performed.
#'
#' @return A processed \code{flights_data} \code{tibble} where IR-measured temperatures (\code{ir_temp})
#'    has been corrected to operative temperatures (\code{op_temp}).
#'
#' @export

correct_flights_data <- function(flights_data, otm_splines, time_correction,
                                 time_correction_metric,
                                 flight_specific_correction){

  ## check parameters and assign unspecified parameter values

  # check if time correction is implemented
  if(missing(time_correction)){
    stop("Missing `time_correction`. Set to TRUE or FALSE")
  }else{
    # check if time correction is logical
    time_correction_present <- ifelse(isTRUE(time_correction),1,
                                      ifelse(isFALSE(time_correction), 1, 0))
    if(time_correction_present == 0){
      stop("`time_correction` must be TRUE or FALSE")
    }
  }

  # check if time correction metric is specified
  if(missing(time_correction_metric) & time_correction == FALSE){
    time_correction_metric <- NA
  }else if(missing(time_correction_metric) & time_correction == TRUE){
    stop("Missing `time_correction_metric`. Set to 'mean', 'median' or 'mode'")
  }else{
    # check if time correction metric is valid
    time_correction_metric_present <- ifelse(
      time_correction_metric %in% c("mean", "median", "mode"), 1, 0)
    if(time_correction_metric_present == 0){
      stop("`time_correction_metric` must be 'mean', 'median' or 'mode'")
    }
  }

  # check if flight specific correction is implemented
  if(missing(flight_specific_correction)){
    stop("Missing `flight_specific_correction` parameter. Set to TRUE or FALSE")
  }else{
    # check if time correction is logical
    flight_specific_correction_present<-ifelse(
      isTRUE(flight_specific_correction),1,
      ifelse(isFALSE(flight_specific_correction), 1, 0))
    if(flight_specific_correction_present == 0){
      stop("`flight_specific_correction` must be TRUE or FALSE")
    }
  }

  ## obtain correction data

  # add column to identify each flight based on doy and mod start
  flights_data$flight_id <- paste(flights_data$year, flights_data$doy,
                                  flights_data$mod_start, sep = "_")

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
    flight <- flights_data %>% filter(get("flight_id") == flights_list[i])

    # filter splines for the same doy as flight of interest
    flight_splines <- otm_splines %>% filter(get("doy") == mean(flight$doy)) %>%
      mutate(op_temp = NA)

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

  ## apply time correction

  if(isTRUE(time_correction)){

    # calculate time correction factors depending on metric specified
    if(time_correction_metric == "mode"){

      # define function to calculate mode
      find_mode <- function(x) {
        x <- round(x, digits = 1)
        ux <- unique(x)
        tab <- tabulate(match(x, ux))
        most_freq <- ux[tab == max(tab)]
        mode <- mean(most_freq, na.rm = T) # find mean if multiple modes
        return(mode)
      }

      # calculate time correction factors
      time_correction_factors <- corr_data %>%
        group_by(year, doy, mod_start) %>%
        summarise(time_corr_factor = find_mode(op_temp - ir_temp))

    }else if(time_correction_metric == "median"){

      # calculate time correction factors
      time_correction_factors <- corr_data %>%
        group_by(year, doy, mod_start) %>%
        summarise(time_corr_factor = median(op_temp - ir_temp))

    }else{

      # calculate time correction factors
      time_correction_factors <- corr_data %>%
        group_by(year, doy, mod_start) %>%
        summarise(time_corr_factor = mean(op_temp - ir_temp))

    }

    # merge date and time correction factors with correlation data
    corr_data <- as_tibble(merge(corr_data, time_correction_factors,
                                 by = c("year", "doy", "mod_start"),
                                 all = TRUE))

    # add time correction factors to correct surface data
    corr_data$ir_temp <- corr_data$ir_temp + corr_data$time_corr_factor

    # merge flights data with time correction factors
    flights_data <- as_tibble(merge(flights_data, time_correction_factors),
                              by = c('year', 'doy', 'mod_start'), all = TRUE)

    # add time correction to flights data
    flights_data$ir_temp <- flights_data$ir_temp + flights_data$time_corr_factor

  }

  ## apply second correction

  # if flight specific correction is applied
  if(isTRUE(flight_specific_correction)){

    # fit linear model accounting for doy and year
    model <- lm(ir_temp ~ op_temp + doy + mod_start, data = corr_data)

    # extract model parameters
    alpha <- model$coefficients[1]
    beta_op_temp <- model$coefficients[2]
    beta_doy <- model$coefficients[3]
    beta_mod <- model$coefficients[4]

    # apply correction to surface temperatures
    flights_data$op_temp <- (flights_data$ir_temp - alpha - beta_doy*flights_data$doy - beta_mod*flights_data$mod_start)/beta_op_temp

  }else{

    # run model for correction
    model <- lm(corr_data$ir_temp ~ corr_data$op_temp)

    # extract model parameters
    alpha <- model$coefficients[1]
    beta <- model$coefficients[2]

    # apply correction to surface temperatures
    flights_data$op_temp <- (flights_data$ir_temp - alpha)/beta

  }

  # select columns of interest
  flights_data <- flights_data %>%
    dplyr::select(longitude, latitude, year, doy, mod_start, mod_end, op_temp)

  return(flights_data)

}

