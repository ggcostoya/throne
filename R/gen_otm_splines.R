## Generate OTM splines

#' Generate OTM splines
#'
#' Generate a unique spline model for each OTM on each date (Julian) while the OTM was measuring
#'
#' @param otm_data An OTM data frame that can (and should) include metadata
#' @param knot_p The number of knots the spline should have taken as a percentage of all observations made in a given day.
#'
#' @return An OTM splines complex tibble with otm_id, date, {metadata} and a nested spline model objects column.
#'
#' @export

gen_otm_splines <- function(otm_data, knot_p){

  # define empty object to extract unique OTM + date data
  otm_splines <- otm_data %>%
    dplyr::select(otm_id, year, date, latitude, longitude) %>%
    unique() %>% as_tibble()

  # define object to store splines as a list
  otm_spline_list <- c()

  # start loop
  for(i in 1:nrow(otm_splines)){

    # extract OTM temperature and minute data for each day
    otm_x <- otm_data %>%
      filter(otm_id == otm_splines$otm_id[i]) %>%
      filter(date == otm_splines$date[i]) %>%
      filter(year == otm_splines$year[i]) %>%
      dplyr::select(op_temp, minute)

    # sort temperature by minute
    otm_x <- otm_x[order(otm_x$minute),]

    # remove NA observations
    otm_x <- otm_x %>% filter(!is.na(minute))

    # evaluate if kno_p was define
    knot_p <- ifelse(missing(knot_p), 1, knot_p)

    # determine number of knots
    knot_n <- ceiling(length(otm_x$minute) * knot_p)

    # if number of observations is > 1
    if(knot_n > 1){

      # fit spline model
      spline <- smooth.spline(x = otm_x$minute, y = otm_x$op_temp, nknots = knot_n)

      # add spline to spline list
      otm_spline_list <- c(otm_spline_list, list(spline))

    }else{ # if there is only one observation spline is NA

      otm_spline_list <- c(otm_spline_list, NA)

    }

  }

  # add splines to the otm_splines object
  otm_splines <- otm_splines %>% mutate(splines = otm_spline_list)

  # some posterior clean up
  otm_splines$latitude <- as.numeric(otm_splines$latitude)
  otm_splines$longitude <- as.numeric(otm_splines$longitude)

  return(otm_splines)

}
