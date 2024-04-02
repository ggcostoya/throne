## Match Flights to OTM data ##

#' Match flights to OTM data
#'
#' Matches thermal dynamics of specific tiles (i.e., specific latitude and longitudes)
#' collected across multiple flights to the thermal dynamics of an operative
#' temperature model (OTM).
#'
#' @param flights_data A \code{tibble} of flights data obtained through \code{rnp_flights_data}
#'    and corrected using \code{correct_flights_data}. The temperature
#'    column should be \code{op_temp} but the function will also work if \code{ir_temp} is provided.
#' @param otm_splines A nested \code{tibble} obtained using \code{gen_otm_splines}
#' @param coverage_per A numeric between 0 - 1 indicating the minimum coverage that
#'    a tile should have across all flights provided in order to be included in the matching.
#'    Coverage is calculated as the number of times temperature was measured in a given tile
#'    divided by the total number of flights. Values >= 0.9 are recommended.
#'    The function will provide a warning of the number of tiles for which
#'    coverage is > 0.5.
#' @param error_max The maximum average absolute error between temperature measurements
#'    of a tile and an OTM that makes a match between a tile and OTM valid.
#'    Error is calculated as the average absolute value between the OTM prediction
#'    and the temperature measurements of the tile.
#'
#' @return A matches \code{tibble} with columns for \code{latitude}, \code{longitde}, the \code{otm_id}
#'    that best describes the thermal dynamics of that tile and the average absolute
#'    \code{error} between tile measurements and OTM predictions. Rows where \code{is.na(otm_id)}
#'    indicate tiles where \code{error} was not \code{< error_max} for any of the OTMs provided.
#'
#' @export

match_data <- function(flights_data, otm_splines, coverage_per, error_max){

  ## intial checks

  # check that coverage_per and error_max arguments are correct
  if(coverage_per > 1 | coverage_per < 0){stop("`coverage_per` must be between 0 - 1")}
  if(error_max < 0){stop("`error_max` must be positive")}

  # check if operative temperature column is provided
  if("op_temp" %in% colnames(flights_data) == FALSE){
    warning("Column `op_temp` not found. Make sure you correct the data using
            `correct_flights_data` before performing the matching if you need to
            the matching will continue assuming using `ir_temp`")
    if("ir_temp" %in% colnames(flights_data) == FALSE){
      stop("No column indicating temperature found")
      flights_data <- flights_data %>% rename(op_temp = ir_temp)
    }
  }

  ## check coverage

  # determine number of unique flights
  flight_n <- length(unique(paste(flights_data$year, flights_data$doy, flights_data$mod_start, sep = "_")))

  # determine max number of tiles
  tile_n <- length(unique(flights_data$latitude * flights_data$longitude))

  # get coverage per tile
  suppressWarnings(coverage <- flights_data %>% group_by(latitude, longitude) %>%
    summarise(coverage = n()/flight_n) %>%  ungroup())

  # merge flights_data with coverage
  flights_data <- merge(flights_data, coverage, by = c("latitude", "longitude"))

  # filter tiles that have been covered at least on 90% of flights
  flights_data_cov <- flights_data %>% filter(coverage >= coverage_per)

  # determine number of tiles after coverage
  tile_n_cov <- length(unique(flights_data_cov$latitude * flights_data_cov$longitude))

  # add warning about coverage percentage
  if(tile_n_cov/tile_n < 0.5){

    cat("Warning! coverage was >=", coverage_per * 100, "% in only",
        round((tile_n_cov/tile_n)*100, digits = 2), "% of tiles")
    cont <- readline("Do you wish to continue? [Y/N]")
    if(cont != "Y") stop("Matching aborted, consider reducing `coverage_per`")

  }else{

    print(paste("Coverage was >=", coverage_per * 100, "% in ",
                round((tile_n_cov/tile_n)*100, digits = 2), "% of tiles"))

  }

  ## predict temperatures using OTMs

  # get list of unique OTMs
  otm_ids <- unique(otm_splines$otm_id)

  # get list of all unique year, doy, mod_start and mod_end combinations
  flights_times <- flights_data %>% dplyr::select(year, doy, mod_start, mod_end) %>% unique()

  # generate data frame with all combinations
  all_combos <- expand.grid(otm_id = otm_ids, year = unique(flights_times$year), doy = unique(flights_times$doy),
                        mod_start = unique(flights_times$mod_start), mod_end = unique(flights_times$mod_end))

  # merge all times combinations with all combinations to get each OTM at all times
  otm_preds <- merge(all_combos, flights_times, by = c("year", "doy", "mod_start", "mod_end"))

  # add column for predicted operative temperatured
  otm_preds$pred_op_temp <- rep(NA, nrow(flights_times))

  # loop to predict operative temperatures during that period
  for(i in 1:nrow(otm_preds)){

    # isolate the spline model for that otm_id, that year, that doy
    otm_specific_spline <- otm_splines %>% filter(otm_id == otm_preds$otm_id[i]) %>%
      filter(year == otm_preds$year[i]) %>% filter(doy == otm_preds$doy[i])

    # predict temperatures for the period between mod_start and mod_end
    prediction <- predict(otm_specific_spline$spline[[1]],c(otm_preds$mod_start[i]:otm_preds$mod_end[i]))$y

    # get average predicted temperature
    otm_preds$pred_op_temp[i] <- mean(prediction)

  }

  ## perform maching

  # add column for unique tile id
  flights_data_cov$tile_id <- flights_data_cov$latitude * flights_data_cov$longitude

  # build holder dataset
  matches <- flights_data_cov %>%
    dplyr::select(latitude, longitude, tile_id) %>% unique() %>%
    mutate(otm_id = NA, error = NA) %>% as_tibble()

  # loop to estimate best OTM match for each tile
  for(i in 1:nrow(matches)){

    # filter data for specific tile
    tile_dat <- flights_data_cov %>% filter(tile_id == matches$tile_id[i])

    # select columns of interest from tile_dat
    tile_dat <- tile_dat %>% dplyr::select(tile_id, year, doy, mod_start, mod_end, op_temp)

    # merge tile data with otm_preds
    tile_dat <- merge(tile_dat, otm_preds, by = c("year", "doy", "mod_start", "mod_end"), all = TRUE)

    # find OTM match
    otm_match <- tile_dat %>% group_by(otm_id) %>%
      summarise(error = mean(abs(pred_op_temp - op_temp), na.rm = T)) %>%
      filter(error == min(error, na.rm = T))

    # add info to holder matches dataset
    matches$otm_id[i] <- as.character(otm_match$otm_id[1])
    matches$error[i] <- otm_match$error[1]

  }

  # assign NA to innacurate matches and prepare dataset for return
  matches <- matches %>%
    mutate(otm_id = ifelse(error > error_max, NA, otm_id)) %>%
    mutate(error = ifelse(is.na(otm_id), NA, error)) %>%
    dplyr::select(!tile_id)

  ## posterior checks about matching

  # check number of tiles that could not be assigned
  unassigned_tile_n <- length(matches$otm_id[is.na(matches$otm_id)])
  total_tile_n <- length(matches$otm_id)

  if(unassigned_tile_n/total_tile_n > 0.1){

    cat("Warning!", round((unassigned_tile_n/total_tile_n) * 100, digits = 2), "% of tiles could not be assigned to any OTM")
    cont <- readline("Do you wish to continue? [Y/N]")
    if(cont != "Y") stop("Matching aborted, consider increasing `error_max`")

  }else{

    print(paste(round((unassigned_tile_n/total_tile_n) * 100, digits = 2), "% of tiles could not be assigned to any OTM"))

  }

  # return
  return(matches)

}


