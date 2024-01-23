## Get correction estimates ##

#' Get correction estimates
#'
#' Gets correction estimates from for a correlation dataset
#'
#' @param corr_data A correlation dataset obtained using the `gen_corr_data` function.
#'
#' @return A list of three objects, first being the correction by date & time dataset, second an intercept and third a slope for the general temperature correction
#'
#' @export

get_correction_factors <- function(corr_data){

  # get date and time correction factors
  date_time_correction <- corr_data %>%
    group_by(year, date, minute) %>%
    summarise(correction_factor = mean(op_temp - ref_temp))

  # merge date and time correction factors with correlation data
  corr_data <- merge(corr_data, date_time_correction, by = c("date", "minute"), all = TRUE)

  # apply date time correction
  corr_data$ref_temp_corr <- corr_data$ref_temp + corr_data$correction_factor

  # fit linear model with corrected reflected temperature data
  model <- lm(corr_data$ref_temp_corr ~ corr_data$op_temp)

  # get intercept
  intercept <- as.numeric(model$coefficients[1])

  # get slope
  slope <- as.numeric(model$coefficients[2])

  return(list(date_time_correction, intercept, slope))

}
