## Evaluate flight correction ##

#' Evaluate flight correction
#'
#' Allows users to evaluate the flight correction process
#'
#' @param flights_data A data frame of flights data obtained through the \code{rnp_flights_data}
#'    function.
#' @param otm_splines A nested \code{tibble} obtained using the \code{gen_otm_splines} function.
#' @param summary If set to \code{TRUE}, the function returns a summary dataset of the correction
#'    process. If set to \code{FALSE} (default), the function returns a raw correction dataset
#'
#' @return If \code{summary = TRUE}, a summary dataset with information on the mean, median,
#'    mode, standard deviation, skewness, minimum and maximum bias between surface and operative temperatures
#'    for every flight considered. If \code{summary = FALSE}, the function returns
#'    a raw correction dataset with all observations of surface temperature
#'    (\code{surf_temp}) and operative temperature (\code{op_temp}) for all tiles
#'    where OTMs were deployed for all flights considered.
#'
#' @export

eval_flights_correction <- function(flights_data, otm_splines, summary){

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
    flight_corr_data <- flight_corr_data[,c("x","y","year","doy",
                                            "mod_start","mod_end",
                                            "surf_temp", "op_temp")]
    corr_data <- rbind(corr_data, flight_corr_data)

  }

  # if summary is missing return corr_data if not summarise
  summary <- if(missing(summary)){return(corr_data)
  }else{
    # send message
    message("Summary statistics of bias between op_temp and surf_temp provided")
    # define function to calculate skewness
    skew <- function(x) {
      x <- stats::na.omit(x)
      n <- length(x)
      mean <- mean(x)
      sd <- sd(x)
      skw <- (n / ((n - 1) * (n - 2))) * sum(((x - mean) / sd)^3)
      return(skw)
    }
    # define function to calculate mode
    find_mode <- function(x) {
      x <- stats::na.omit(x)
      x <- round(x, digits = 1)
      ux <- unique(x)
      tab <- tabulate(match(x, ux))
      most_freq <- ux[tab == max(tab)]
      mode <- mean(most_freq, na.rm = T) # find mean if multiple modes
      return(mode)
    }
    # generate evaluation summary statistics
    eval_summary <- corr_data |>
      dplyr::group_by(get("year"), get("doy"), get("mod_start"), get("mod_end")) |>
      dplyr::summarise(mean_bias = mean(get("op_temp") - get("surf_temp"), na.rm = T),
                       median_bias = stats::median(get("op_temp") - get("surf_temp"), na.rm = T),
                       mode_bias = find_mode(get("op_temp") - get("surf_temp")),
                       sd_bias = stats::sd(get("op_temp") - get("surf_temp"), na.rm = T),
                       skewness_bias = skew(get("op_temp") - get("surf_temp")),
                       max_bias = max(get("op_temp") - get("surf_temp"), na.rm = T),
                       min_bias = min(get("op_temp") - get("surf_temp"), na.rm = T),
                       max_abs_bias = max(abs(get("op_temp") - get("surf_temp")), na.rm = T),
                       min_abs_bias = min(abs(get("op_temp") - get("surf_temp")), na.rm = T))|>
      dplyr::ungroup()
    names(eval_summary)[1:4] <- c("year", "doy", "mod_start", "mod_end")
    return(eval_summary)
  }
}
