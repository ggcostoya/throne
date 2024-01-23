## Predict thermal landscape ##

#' Predict thermal landscape
#'
#' @param matches x
#' @param otm_splines x
#' @param date x
#' @param minute x
#'
#' @return x
#'
#' @export

predict_thermal_landscape <- function(matches, otm_splines, date, minute){

  # test if prediction query within time limit
  if(!date %in% unique(otm_splines$date)){

    print("Date outside of range")

  }else{

    # add column to store predicted operative temperature
    pred_op_temps <- tibble(otm_id = unique(matches$otm_id),
                            date = rep(date, length(otm_id)),
                            minute = rep(minute,  length(otm_id)),
                            pred_op_temp = rep(NA,  length(otm_id)))

    # loop to predict operative temperatures
    for(i in 1:nrow(pred_op_temps)){

      # filter OTM specific spline for the indicated date
      otm_specific_spline <- otm_splines %>%
        filter(otm_id == pred_op_temps$otm_id[i],
               date == pred_op_temps$date[i])

      # predict operative temperature
      pred_op_temps$pred_op_temp[i] <- predict(otm_specific_spline$splines[[1]],
                                               pred_op_temps$minute[i])$y

    }

    # merge matches with predicted temperatures
    prediction <- merge(matches, pred_op_temps, by = "otm_id")

    # filter columns of interest and return prediction
    prediction <- prediction %>%
      dplyr::select(latitude, longitude, date, minute, pred_op_temp)

    return(prediction)

    }

}
