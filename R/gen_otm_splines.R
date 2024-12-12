## Generate OTM splines

#' Generate OTM splines
#'
#' Generate a unique spline model for each OTM on each date (Julian) while the OTM was measuring
#'
#' @param otm_data An OTM data \code{tibble} obtained via the function
#'    \code{rnp_otms_data}.
#' @param knot_p The number of knots the spline model should have to describe the
#'    temperature dynamics of the OTM for a given day taken as a percentage of
#'    the total number of operative measurements in a given day.
#'
#' @return A nested data frame (\code{tibble}) with columns for \code{otm_id},
#'    \code{year}, \code{doy} and \code{spline}. The spline column is nested and
#'    contains a OTM, year and DOY \code{smooth.spline} model with the specified
#'    number of knots.
#'
#' @export

gen_otm_splines <- function(otm_data, knot_p){

  # check knot_p is valid
  if(!dplyr::between(knot_p, 0, 1)){stop("`knot_p` must be between 0 & 1")}

  # define empty object to extract unique OTM + doy data and store object
  otm_splines <- otm_data[, !(names(otm_data) %in% c("op_temp", "mod"))]
  otm_splines <- unique(otm_splines)
  otm_spline_list <- c()

  # set up progress bar
  message("Generating OTM & doy-specific spline models...")
  pb <- utils::txtProgressBar(min=1,max=nrow(otm_splines),style=3)

  # start loop to calculate splines
  for(i in 1:nrow(otm_splines)){

    # select columns of interest, order and prepare data for splining
    otm_x <- otm_data[otm_data$otm_id == otm_splines$otm_id[i] &
                        otm_data$year == otm_splines$year[i] &
                        otm_data$doy == otm_splines$doy[i],
                      c("op_temp", "mod")]
    otm_x <- otm_x[order(otm_x$mod),] # order by minute of the day
    otm_x <- otm_x[!is.na(otm_x$mod),] # remove NA observations if any

    # determine number of knots to include in spine models
    knot_n <- ceiling(length(otm_x$mod) * knot_p)

    # if number ratio between knot_n and number of observations is less than 0.33 set spline as NA
    if(knot_n > 3){
      spline <- stats::smooth.spline(x = otm_x$mod, y = otm_x$op_temp, nknots = knot_n, cv = FALSE)
      otm_spline_list <- c(otm_spline_list, list(spline))
    }else{ # if there is only one observation spline is NA
      otm_spline_list <- c(otm_spline_list, NA)
    }

    # print progress
    Sys.sleep(0.1)
    utils::setTxtProgressBar(pb, i)

  }
  message("Spline generation complete")

  # add spline to the otm_splines tibble
  otm_splines$spline <- otm_spline_list

  # return
  return(tibble::as_tibble(otm_splines))

}


