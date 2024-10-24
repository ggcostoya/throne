
## Evaluate flight correction ##

#' Evaluate flight correction
#'
#' Allows users to evaluate the flight correction process
#'
#' @param flights_data A \code{tibble} of flights data obtained through the \code{rnp_flights_data}
#'    function. The \code{tibble} must contain columns for `longitude` and `latitude`.
#' @param otm_splines A nested \code{tibble} obtained using the \code{gen_otm_splines} function.
#'    The \code{tibble} must contain columns for \code{longitude} and \code{latitude} and all values
#'    must also be in the \code{flights_data}
#' @param summary If set to \code{TRUE}, the function returns a summary dataset of the correction
#'    process. If set to \code{FALSE} (default), the function returns a raw correction dataset
#'
#' @return If \code{summary = TRUE}, a summary dataset with information on the mean, median, standard deviation,
#'    skewness, minimum and maximum bias between surface and operative temperatures
#'    for every flight considered. If \code{summary = FALSE}, a raw correction dataset with
#'    all observations of surface temperature (\code{ir_temp}) and operative temperature
#'    (\code{op_temp}) for all tiles where OTMs were deployed for all flights considered.
#'
#' @export

eval_flights_correction <- function(flights_data, otm_splines, summary){

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

  # evaluate if "summary" is missing or not
  summary <- if(missing(summary)){

    return(corr_data)


  }else{

    # send mess
    print("Summary statistics of bias between op_temp and ir_temp provided")

    # define function to calculate skewness
    skew <- function(x) {
      x <- na.omit(x)
      n <- length(x)
      mean <- mean(x)
      sd <- sd(x)
      skw <- (n / ((n - 1) * (n - 2))) * sum(((x - mean) / sd)^3)
      return(skw)
    }

    # generate evaluation summary statistics
    eval_summary <- corr_data %>% group_by(year, doy, mod_start, mod_end) %>%
      summarise(mean_bias = mean(op_temp - ir_temp, na.rm = T),
                median_bias = median(op_temp - ir_temp, na.rm = T),
                sd_bias = sd(op_temp - ir_temp, na.rm = T),
                skewness_bias = skew(op_temp - ir_temp),
                max_bias = max(op_temp - ir_temp, na.rm = T),
                min_bias = min(op_temp - ir_temp, na.rm = T),
                max_abs_bias = max(abs(op_temp - ir_temp), na.rm = T),
                min_abs_bias = min(abs(op_temp - ir_temp), na.rm = T)) %>%
      ungroup()

    return(eval_summary)

  }

}


