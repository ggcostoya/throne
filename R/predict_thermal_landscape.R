## Predict thermal landscape ##

#' Predict thermal landscape
#'
#' Predicts a thermal landscape by integrating information from an OTM-tile match
#' object and an \code{otm_splines} object for a specified range of days of the
#' year (\code{doy}) and minutes of the day (\code{mod}).
#'
#' @param matches A matches data frame obtained using the \code{match_data} function.
#' @param otm_splines A nested \code{tibble} obtained using the \code{gen_otm_splines} function.
#' @param doy An integer or vector of integers indicating the day(s) of the year
#'    (\code{doy}) for which the thermal landscape should be predicted. All elements within
#'    \code{doy} must be within the range between when the OTMs started and finished
#'    recording operative temperatures.
#' @param mod A numeric vector of length >=1 indicating the minute(s) of the
#'    day (\code{mod}) for which the thermal landscape should be predicted. All elements
#'    within \code{mod} must fall within the 0 to 1440 range. If \code{mod} includes
#'    values when the OTMs were not recording the function will remove them and provide
#'    a diagnostics message indicating the percentage of predictions removed.
#'
#' @return A data frame with a predicted thermal landscape with \code{x}, \code{y},
#'    (UTM coordinates), \code{doy},\code{mod} and predicted operative temperature
#'    columns (\code{pred_op_temp}).
#'
#' @export

predict_thermal_landscape <- function(matches, otm_splines, doy, mod){

  # get minimum and maximum doy
  min_doy <- min(otm_splines$doy, na.rm = T)
  max_doy <- max(otm_splines$doy, na.rm = T)

  # check maximum and minimum times when OTMs were recording on max doy and min doy
  min_doy_splines <- otm_splines[otm_splines$doy == min_doy,]
  max_doy_splines <- otm_splines[otm_splines$doy == max_doy,]

  # define empty list for minimum and maximum times each OTM was recording
  mins <- c()
  maxs <- c()

  # loops to estimate all minimum and maximum mod of recording
  for(i in 1:length(min_doy_splines)){
    mins <- c(mins, min(min_doy_splines$spline[[i]]$x))
  }
  for(i in 1:length(max_doy_splines)){
    maxs <- c(maxs, max(max_doy_splines$spline[[i]]$x))
  }

  # get minimum and maximum mod on minimum and maximum doy
  min_mod <- max(mins, na.rm = T)
  max_mod <- min(maxs, na.rm = T)

  # check if doy falls within range
  if(all(doy %in% unique(otm_splines$doy)) != TRUE){
    stop(paste("Some `doy` not available, set between",
               min(otm_splines$doy), "and", max(otm_splines$doy), sep = " "))}
  if(all(mod > 0 | mod < 1440) != TRUE){
    stop("Some `mod` fall outside day range, set between 0 and 1440")}

  # generate thermal landscape object
  landscape <- matches[, !names(matches) %in% "error"]
  landscape <- landscape[!is.na(landscape$otm_id),]

  # prepare prediction object
  otm_ids <- unique(landscape$otm_id)
  otm_ids <- otm_ids[!is.na(otm_ids)]
  pred <- data.frame(otm_id = c(), doy = c(), mod = c(), pred_op_temp = c())

  # loop to predict operative temperatures
  for(i in 1:length(otm_ids)){
    for(j in 1:length(doy)){

      spline <- otm_splines[otm_splines$otm_id == otm_ids[i] &
                              otm_splines$doy == doy[j],]
      pred_op_temp <- stats::predict(spline$spline[[1]], mod)$y
      reps <- length(mod)
      otm_pred <- data.frame(otm_id = rep(otm_ids[i], reps),
                             doy = rep(doy[j], reps),
                             mod = mod,
                             pred_op_temp = pred_op_temp)
      pred <- rbind(pred, otm_pred)
    }
  }

  # merge landscape with predictions and clean output
  landscape_pred <- merge(landscape, pred, by = "otm_id", all = TRUE)

  # remove predictions that fell outside of the range when OTMs were recording
  landscape_pred$pred_op_temp <- ifelse(
    landscape_pred$doy == min_doy & landscape_pred$mod < min_mod, NA,
    landscape_pred$pred_op_temp)
  landscape_pred$pred_op_temp <- ifelse(
    landscape_pred$doy == max_doy & landscape_pred$mod > max_mod, NA,
    landscape_pred$pred_op_temp)
  temps_removed <- length(landscape_pred$pred_op_temp[is.na(landscape_pred$pred_op_temp)])
  if(temps_removed > 0){
    message(paste(round(100 *(temps_removed/nrow(landscape_pred)), digits = 2),
                  "% of predictions were removed due to falling outside of the
                   time when OTMs were recording"))}
  landscape_pred <- landscape_pred[!is.na(landscape_pred$pred_op_temp),]

  return(landscape_pred)
}
