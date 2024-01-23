## Match OTM dynamics with pixels ##

#' Match data
#'
#' Matches each pixel within a given are where flights have occured with an OTM.
#'
#' @param flights_data A flight data frame for multiple flights
#' @param coverage Number of times a given pixel needs to be covered by a flight for it to be considered.
#' @param otm_splines An OTM splines dataset obtained with the `gen_otm_splines` function
#'
#' @return A match dataset where each pixel is associated with an OTM.
#'
#' @export

match_data <- function(flights_data, coverage, otm_splines){

  # get list of tiles that were covered
  times_covered <- flights_data %>% group_by(latitude, longitude) %>%
    summarise(times_covered = n()) %>% ungroup()

  # merge times covered with flights data
  flights_data <- merge(flights_data, times_covered, by = c("latitude", "longitude"))

  # filter for a specified degree of coverage
  flights_data <- flights_data %>% filter(times_covered > coverage - 1)

  # generate tile identifier column
  flights_data$pix_id <- flights_data$latitude * flights_data$longitude

  # filter rows were op temp was not predicted
  flights_data <- flights_data %>% filter(op_temp != 0)

  # get list of unique tile IDs and generate matching OTM column
  matches <- tibble(pix_id = unique(flights_data$pix_id),
                    otm_id = rep(NA, length(pix_id)))

  # get list of unique OTMs
  otm_ids <- unique(otm_splines$otm_id)

  # start loop
  for(i in 1:nrow(matches)){

    # filter data for specific pixel id
    pix_dat <- flights_data %>% filter(pix_id == matches$pix_id[i])

    # object to store OTM matches
    otm_matches <- tibble(otm_id = c(), match = c())

    # loop to find OTM matches
    for(j in 1:length(otm_ids)){

      # filter splines for a given OTM
      otm_specific_splines <- otm_splines %>% filter(otm_id == otm_ids[j]) %>%
        dplyr::select(otm_id, date, splines)

      # merge otm specific splines with tile data
      pix_dat_merged <- as_tibble(merge(pix_dat, otm_specific_splines, by = "date"))

      # perdict temperatures
      pix_dat_merged <- pix_dat_merged %>%
        mutate(pred_temp = (predict(splines[[1]], minute_start)$y + predict(splines[[1]], minute_end)$y)/2) %>%
        group_by(otm_id) %>%
        summarise(match = mean(abs(op_temp - pred_temp), na.rm = T)) %>% ungroup()

      # bind with otm_matches holder dataset
      otm_matches <- rbind(otm_matches, pix_dat_merged)

    }

    # filter the OTM that lead to the smallest bias
    matches$otm_id[i] <- otm_matches %>%
      filter(match == min(match, na.rm = T)) %>%
      dplyr::select(otm_id) %>% pluck(1)

  }

  return(matches)

}
