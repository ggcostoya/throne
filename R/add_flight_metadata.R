## Add flight metadata ##

#' Add flight metadata
#'
#' Adds metadata (e.g., flight time) to a processed flight data frame
#'
#' @param flight_data A processed flight data frame generated with the function `rnp_flight_data.R`
#' @param flight_id The ID of the flight to match its metadata.
#' @param flight_metadata A flight metadata data frame.
#'
#' @return A flight data frame with metadata.
#'
#' @export

add_flight_metadata <- function(flight_data, flight_id, flight_metadata){

  # change name of flight id
  specific_flight_id <- flight_id

  # extract flight metadata for that flight id
  metadata <- flight_metadata %>% filter(flight_id == specific_flight_id)

  # get year & julian date when the flight took place
  year <- year(mdy(metadata$date))
  date <- julian(mdy(metadata$date), origin = as.Date( paste(year,"-01-01", sep = "")))

  # get minute of the day when the flight started and when it ended
  minute_start <- hour(hm(metadata$time_start))*60 + minute(hm(metadata$time_start))
  minute_end <- hour(hm(metadata$time_end))*60 + minute(hm(metadata$time_end))

  flight_data <- flight_data %>% mutate(year = year, date = date,
                                        minute_start = minute_start,
                                        minute_end = minute_end)

  return(flight_data)

}
