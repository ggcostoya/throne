## Predict thermal landscape ##

#' Predict thermal landscape
#'
#' @param matches A matches \code{tibble} obtained using the \code{match_data} function.
#' @param otm_splines A complex \code{tibble} obtained using the \code{gen_otm_splines} function.
#' @param doy An integer or vector of integers indicating the day(s) of the year
#'    (\code{doy}) for which the thermal landscape should be predicted. All elements within
#'    \code{doy} must be within the range between when the OTMs started and finished
#'    recording operative temperatures.
#' @param mod An integer of vector of integers indicating the minute(s) of the
#'    day (MOD) for which the thermal landscape should be predicted. All elements
#'    within \code{mod} must fall within the 0 to 1440 range. If \code{mod} includes
#'    values when the OTMs were not recording it will return \code{NA} and provide a
#'    diagnostics message
#'
#' @return A \code{tibble} with a predicted thermal landscape with \code{longitude}, \code{latitude},
#'    \code{doy}, \code{mod} and predicted operative temperature columns (\code{pred_op_temp}).
#'
#' @export

predict_thermal_landscape <- function(matches, otm_splines, doy, mod){

  ## checks

  # function to extract minimum and maximum mods possible
  extract_min_mod <- function(splines){

    # filter data for min doy
    min_doy_splines <- splines %>% filter(doy == min(doy))
    max_doy_splines <- splines %>% filter(doy == max(doy))

    # define empty lists for mins and maxs
    mins <- c()
    maxs <- c()

    # loop to estimate all minimum mods
    for(i in 1:length(min_doy_splines)){mins <- c(mins, min(min_doy_splines$spline[[i]]$x))}

    # loop to estimate all max mods
    for(i in 1:length(max_doy_splines)){maxs <- c(maxs, max(max_doy_splines$spline[[i]]$x))}


    return(c(max(mins), min(maxs)))

  }

  # extract minimum and maximum mods
  min_max_doy <- extract_min_mod(otm_splines)

  # determine maximum and minimum doy
  max_doy <- max(otm_splines$doy)
  min_doy <- min(otm_splines$doy)

  # check if doy falls within range
  if(all(doy %in% unique(otm_splines$doy)) != TRUE){stop(paste("Some `doy` not available, it needs to fall between", min_doy, "and", max_doy, sep = " "))}

  # check if mod is correct
  if(all(mod > 0 | mod < 60 * 24)!= TRUE){stop("Some `mod` falls outside of range, they need to be between 0 and 1440")}

  # generate prediction dataset
  landscape <- matches %>% dplyr::select(!error)

  # get list of unique OTMs and dates to predict
  otm_ids = unique(landscape$otm_id)
  otm_pred <- expand.grid(otm_ids, doy, mod)
  colnames(otm_pred) <- c("otm_id", "doy", "mod")
  otm_pred <- otm_pred %>% filter(!is.na(otm_id)) %>% mutate(pred_op_temp = NA)

  # loop to predict temperatures
  for(i in 1:nrow(otm_pred)){

    # filter OTM specific spline
    otm_specific_spline <- otm_splines %>%
      filter(doy == otm_pred$doy[i]) %>%
      filter(otm_id == otm_pred$otm_id[i])

    # predict temperature
    otm_pred$pred_op_temp[i] <- predict(otm_specific_spline$spline[[1]], otm_pred$mod[i])$y

  }

  # merge OTM prediction with landscape prediction
  landscape_pred <- merge(landscape, otm_pred, by = "otm_id", all = TRUE)

  # prepare prediction for return
  landscape_pred <- landscape_pred %>% dplyr::select(!otm_id) %>% as_tibble()

  ## posterior check

  # remove predictions that fall outside of range
  landscape_pred$pred_op_temp <- ifelse(landscape_pred$doy == min_doy & landscape_pred$mod < min_max_doy[1],
                                        NA, landscape_pred$pred_op_temp)
  landscape_pred$pred_op_temp <- ifelse(landscape_pred$doy == max_doy & landscape_pred$mod > min_max_doy[2],
                                        NA, landscape_pred$pred_op_temp)

  # check how many predictions were removed
  removed <- length(landscape_pred$pred_op_temp[is.na(landscape_pred$pred_op_temp)])

  if(removed > 0){print(paste(round(100 *(removed/nrow(landscape_pred)), digits = 2), "% of predictions were removed due to falling outside of the time when OTMs were recording"))}

  # return
  return(landscape_pred)

}



