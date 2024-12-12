## Correct flights data ##

#' Correct flights data
#'
#' Corrects flights data to transform IR-measured temperatures into operative temperatures
#'
#' @param flights_data A data frame of flights data obtained through the \code{rnp_flights_data}
#'    function.
#' @param otm_splines A nested \code{tibble} obtained using the \code{gen_otm_splines} function.
#' @param time_correction A logical value (\code{TRUE} or \code{FALSE})
#'    indicating whether first correction step should be applied. If set to \code{TRUE},
#'    the function will add the mean, median or mode (based on \code{time_correction_metric},
#'    see below) bias between all surface and operative temperatures in tiles where
#'    OTMs were present to all surface temperature measurements within a flight.
#'    The goal of applying a time correction is to account for day and time-specific
#'    systematic biases between operative and surface temperatures which may be a
#'    by product of the inherent nature of thermal imaging (e.g., measurements being
#'    influenced by light conditions, sensor calibration. etc.).
#'    \code{time_correction = TRUE} if \code{flight_specific_correction = FALSE} is
#'    recommended.
#' @param time_correction_metric A character string indicating the metric to be
#'    used to perform the time correction. The metric can be either \code{"mean"},
#'    \code{"median"} or \code{"mode"}.For most cases, \code{"mean"} is recommended
#'    but \code{"median"} could be used if outliers are detected. To evaluate
#'    the best method, use the function \code{eval_flights_correction} also
#'    included in \code{throne}.
#' @param flight_specific_correction A logical value (\code{TRUE} or \code{FALSE})
#'    indicating whether the second correction step should be applied to all flights
#'    or each flight specifically. If set to \code{TRUE} the second correction
#'    step will also incorporate the effects of day of the year and minute of day
#'    into the linear model from which the correction will be performed.
#' @return A processed \code{flights_data} \code{tibble} where IR-measured surface
#'    temperatures (\code{surf_temp}) has been corrected to operative
#'    temperatures (\code{op_temp}).
#' @export

correct_flights_data <- function(flights_data, otm_splines,
                                 time_correction, time_correction_metric,
                                 flight_specific_correction){

  # checks
  if(missing(time_correction)){stop("Missing `time_correction`, set to TRUE or FALSE")}
  if(!is.logical(time_correction)){stop("`time_correction` must be TRUE or FALSE")}
  if(missing(time_correction_metric) & time_correction == FALSE){time_correction_metric <- NA}
  if(missing(time_correction_metric) & time_correction == TRUE){
    stop("`time_correction` is applied but `time_correction_metric` is missing,
         set to 'mean', 'median' or 'mode'")}
  if(!missing(time_correction_metric) & time_correction == FALSE){time_correction <- TRUE}
  if(time_correction == TRUE & !(time_correction_metric %in% c("mean", "median", "mode"))){
    stop("`time_correction_metric` must be 'mean', 'median' or 'mode'")}
  if(missing(flight_specific_correction)){
    stop("Missing `flight_specific_correction`, set to TRUE or FALSE")}
  if(!is.logical(flight_specific_correction)){
    stop("`flight_specific_correction` must be TRUE or FALSE")}

  # add column to identify each flight based on doy and mod start
  flights_data$flight_id <- paste(flights_data$year, flights_data$doy,
                                  flights_data$mod_start, sep = "_")
  flights_list <- unique(flights_data$flight_id)

  # find closest x and y in flights for each OTM
  unique_x <- unique(flights_data$x)
  unique_y <- unique(flights_data$y)
  unique_otms_xy <- unique(otm_splines[,c("otm_id", "x", "y")])
  for(i in 1:nrow(unique_otms_xy)){
    which_x <- which.min(abs(unique_otms_xy$x[i] - unique_x))
    which_y <- which.min(abs(unique_otms_xy$y[i] - unique_y))
    unique_otms_xy$x[i] <- unique_x[which_x]
    unique_otms_xy$y[i] <- unique_y[which_y]
  }

  # re-assign x and y coordinates to OTM splines
  otm_splines <- otm_splines[, !(names(otm_splines) %in% c("x", "y"))]
  otm_splines <- tibble::as_tibble(
    merge(otm_splines, unique_otms_xy, by = "otm_id"))

  # generate holder correlation data frame
  corr_data <- data.frame(x = c(), x = c(), year = c(), doy = c(),
                          mod_start = c(), mod_end = c(),
                          surf_temp = c(), op_temp = c())

  # start loop to get correction data for each flight
  for(i in 1:length(flights_list)){

    # filter data for flight of interest and spines on doy when flight took place
    flight <- flights_data[flights_data$flight_id == flights_list[i],]
    flight_splines <- otm_splines[otm_splines$doy == mean(flight$doy),]
    flight_splines$op_temp <- NA # generate op_temp column
    mod_range <- c(mean(flight$mod_start):mean(flight$mod_end)) # get mod range of flight

    # predict operative temperatures during mod range of flight
    for(j in 1:nrow(flight_splines)){
      flight_splines$op_temp[j] <- mean(stats::predict(flight_splines$spline[[j]], mod_range)$y)
    }

    # merge to get correction data, select columns of interest and bind to holder
    flight_corr_data <- merge(flight, flight_splines, by = c("year", "doy", "x", "y"))
    flight_corr_data <- flight_corr_data[,c("x","y","year","doy","mod_start",
                                            "mod_end","surf_temp", "op_temp")]
    corr_data <- rbind(corr_data, flight_corr_data)

  }

  # calculate time correction factors if specified with user metric
  if(isTRUE(time_correction)){
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
      time_correction_factors <- corr_data |>
        dplyr::group_by(get("year"), get("doy"), get("mod_start")) |>
        dplyr::summarise(time_corr_factor = find_mode(get("op_temp") - get("surf_temp"))) |>
        dplyr::ungroup()
      colnames(time_correction_factors) <- c("year","doy","mod_start","time_corr_factor")
    }else if(time_correction_metric == "median"){
      time_correction_factors <- corr_data |>
        dplyr::group_by(get("year"), get("doy"), get("mod_start")) |>
        dplyr::summarise(time_corr_factor = stats::median(get("op_temp") - get("surf_temp"))) |>
        dplyr::ungroup()
      colnames(time_correction_factors) <- c("year","doy","mod_start","time_corr_factor")
    }else{
      time_correction_factors <- corr_data |>
        dplyr::group_by(get("year"), get("doy"), get("mod_start")) |>
        dplyr::summarise(time_corr_factor = mean(get("op_temp") - get("surf_temp"))) |>
        dplyr::ungroup()
      colnames(time_correction_factors) <- c("year","doy","mod_start","time_corr_factor")
    }

    # merge time correction factors with correction data and apply correction
    corr_data <- merge(corr_data, time_correction_factors,
                       by = c("year", "doy", "mod_start"), all = TRUE)
    corr_data$surf_temp <- corr_data$surf_temp + corr_data$time_corr_factor

    # merge time correction factors with correction data and apply correction
    flights_data <- merge(flights_data, time_correction_factors,
                          by = c('year', 'doy', 'mod_start'), all = TRUE)
    flights_data$surf_temp <- flights_data$surf_temp + flights_data$time_corr_factor

  }

  # apply flight specific correction if specified by user
  if(isTRUE(flight_specific_correction)){
    # fit linear model extract coefficients and apply correction if flight specific correction is TRUE
    model <- stats::lm(surf_temp ~ op_temp + doy + mod_start, data = corr_data)
    alpha <- model$coefficients[1]
    beta_op_temp <- model$coefficients[2]
    beta_doy <- model$coefficients[3]
    beta_mod <- model$coefficients[4]
    flights_data$op_temp <- (flights_data$surf_temp - alpha -
                               beta_doy*flights_data$doy - beta_mod*flights_data$mod_start)/beta_op_temp

  }else{
    # run model for correction if flight specific correction is FALSE
    model <- stats::lm(surf_temp ~ op_temp, data = corr_data)
    alpha <- model$coefficients[1]
    beta <- model$coefficients[2]
    flights_data$op_temp <- (flights_data$surf_temp - alpha)/beta

  }

  # select columns of interest and return
  flights_data <- flights_data[, c("x", "y", "year", "doy", "mod_start", "mod_end", "op_temp")]
  return(flights_data)

}





