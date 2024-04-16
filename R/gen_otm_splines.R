## Generate OTM splines

#' Generate OTM splines
#'
#' Generate a unique spline model for each OTM on each date (Julian) while the OTM was measuring
#'
#' @param otm_data An OTM data `tibble` obtained via the function `rnp_otms_data`.
#'    It must include columns describing the OTM id (`otm_id`), the day of the
#'    year (`doy`), the minute of the day (`mod`) in which each operative temperature
#'    measurement (`op_temp`) was made. The function can run without `latitude` and
#'    `longitude` columns, but including them is necessary for the `correct_flight_data`,
#'    `match_data` and `predict_thermal_landscape` functions.
#' @param knot_p The number of knots the spline model should have to describe the
#'    temperature dynamics of the OTM for a given day taken as a percentage of
#'    the total number of operative measurements in a given day.
#'
#' @return A complex `tibble` with columns for `otm_id`, `year`, `doy` and `spline`.
#'    The spline column is nested and contains a OTM, year and DOY `smooth.spline` model
#'    with the specified number of knots.
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#'
#' # filter data for a specific OTM in a given doy
#' otm <- otms_data %>% filter(otm_id == "OTM12", doy == 237)
#'
#' # generate the spline model
#' otm_spline <- gen_otm_splines(otm_data = otm, knot_p = 1/7.5)
#'
#' # obtain a prediction
#' pred <- tibble(mod = seq(0,1440,by = 1),
#' op_temp = predict(otm_spline$spline[[1]], seq(0,1440,by = 1))$y)
#'
#' # plotting
#' ggplot() +
#'   geom_point(data = otm, aes(x = mod, y = op_temp), size = 4, alpha = 0.1) +
#'   geom_line(data = pred, aes(x = mod, y = op_temp), linewidth = 2, col = "red") +
#'   xlab("Minute of the day (MOD)") + ylab("Operative Temperature (C)")
#'
#' @export

gen_otm_splines <- function(otm_data, knot_p){

  ## checks

  # check that OTM data has the necessary columns
  if("otm_id" %in% colnames(otm_data) == FALSE){stop("`otm_id` column is missing in `otm_data`")}
  if("op_temp" %in% colnames(otm_data) == FALSE){stop("`op_temp` column is missing in `otm_data`")}
  if("mod" %in% colnames(otm_data) == FALSE){stop("`mod` column is missing in `otm_data`")}
  if("doy" %in% colnames(otm_data) == FALSE){stop("`doy` column is missing in `otm_data`")}
  if("latitude" %in% colnames(otm_data) ==  FALSE){warning("`latitude` column is missing in `otm_data`. The splines will be generated but complete coordinates are needed to predict thermal landscapes!")}
  if("longitude" %in% colnames(otm_data) ==  FALSE){warning("`longitude` column is missing in `otm_data`. The splines will be generated but complete coordinates are needed to predict thermal landscapes!")}

  # check knot_p is valid
  if(!between(knot_p, 0, 1)){stop("`knot_p` must be between 0 & 1")}

  # define empty object to extract unique OTM + doy data
  otm_splines <- otm_data %>% dplyr::select(!c(op_temp, mod)) %>% unique()

  # define object to store splines as a list
  otm_spline_list <- c()

  # start loop
  for(i in 1:nrow(otm_splines)){

    # extract OTM temperature and minute data for each day
    otm_x <- otm_data %>%
      filter(otm_id == otm_splines$otm_id[i]) %>%
      filter(year == otm_splines$year[i]) %>%
      filter(doy == otm_splines$doy[i]) %>%
      dplyr::select(op_temp, mod)

    # make sure operative temperatures are sorted by `mod`
    otm_x <- otm_x[order(otm_x$mod),]

    # remove NA observations
    otm_x <- otm_x %>% filter(!is.na(mod))

    # determine number of knots to include in spine models
    knot_n <- ceiling(length(otm_x$mod) * knot_p)

    # if number of observations is > 1
    if(knot_n > 2){

      # fit spline model
      spline <- smooth.spline(x = otm_x$mod, y = otm_x$op_temp, nknots = knot_n, cv = FALSE)

      # add spline to spline list
      otm_spline_list <- c(otm_spline_list, list(spline))

    }else{ # if there is only one observation spline is NA

      otm_spline_list <- c(otm_spline_list, NA)

    }

  }

  # add spline to the otm_splines tibble
  otm_splines <- otm_splines %>% mutate(spline = otm_spline_list)


  # return
  return(as_tibble(otm_splines))

}
