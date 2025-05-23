
## Match Flights to OTM data ##

#' Match flights to OTM data
#'
#' Matches thermal dynamics of specific tiles (i.e., specific latitude and longitudes)
#' collected across multiple flights to the thermal dynamics of an operative
#' temperature model (OTM).
#'
#' @param flights_data A data frame of flights data obtained through \code{rnp_flights_data}
#'    and corrected using \code{correct_flights_data}. The temperature
#'    column should be \code{op_temp} but the function will also work if \code{surf_temp} is provided.
#' @param otm_splines A nested \code{tibble} obtained using \code{gen_otm_splines}
#' @param coverage_per A numeric between 0 - 1 indicating the minimum coverage that
#'    a tile should have across all flights provided in order to be included in the matching.
#'    Coverage is calculated as the number of times temperature was measured in a given tile
#'    divided by the total number of flights. Values >= 0.9 are recommended.
#'    The function will provide a warning of the numbeof tiles for which
#'    coverage is > 0.5. If missing, it defaults to 0.9
#' @param error_max The maximum average absolute error between temperature measurements
#'    of a tile and an OTM that makes a match between a tile and OTM valid.
#'    Error is calculated as the average absolute value between the OTM prediction
#'    and the temperature measurements of the tile. If missing, it defaults to 100,
#'    a sufficiently large number so all tiles are assigned regardless of error.
#'
#' @return A matches data frame with columns for \code{x}, \code{y} (UTM coordinates),
#'    the \code{otm_id} that best describes the thermal dynamics of that tile and
#'    the average absolute \code{error} between tile measurements and OTM predictions.
#'    Rows where \code{is.na(otm_id)} indicate tiles where \code{error} was not
#'    \code{< error_max} for any of the OTMs provided.
#'
#' @export


match_data <- function(flights_data, otm_splines, coverage_per, error_max){

  # checks
  if(missing(coverage_per)){coverage_per <- 0.9} # if coverage missing set to 0.9
  if(missing(error_max)){error_max <- 100} # if error_max missing set to 100
  if(coverage_per > 1 | coverage_per < 0){stop("`coverage_per` must be between > 0 & <= 1")}
  if(error_max < 0){stop("`error_max` must be positive")}

  # check if operative temperature column is provided
  if("op_temp" %in% colnames(flights_data) == FALSE){
    warning("Column `op_temp` not found, the matching will proceed using `surf_temp`")
    if("surf_temp" %in% colnames(flights_data) == FALSE){
      stop("No column indicating temperature found")
      names(flights_data)[names(flights_data) == "surf_temp"] <- "op_temp"
    }
  }

  # determine number of unique flights, tiles and get coverage per tile
  flight_n <- length(unique(paste(flights_data$year, flights_data$doy,
                                  flights_data$mod_start, sep = "_")))
  suppressWarnings(coverage <- flights_data |>
                     dplyr::group_by(get("x"), get("y")) |>
                     dplyr::summarise(coverage = (dplyr::n())/flight_n) |>
                     dplyr::ungroup())
  colnames(coverage) <- c("x", "y", "coverage")
  tile_n <- length(unique(flights_data$y)) * length(unique(flights_data$x))

  # merge flights_data with coverage filter tile with
  flights_data <- merge(flights_data, coverage, by = c("x", "y"))
  flights_data_cov <- flights_data[flights_data$coverage >= coverage_per, ]
  tile_n_cov <- length(unique(flights_data_cov$y)) * length(unique(flights_data_cov$x))

  # warning about coverage percentage
  if(tile_n_cov/tile_n < 0.5){
    cat("Warning! coverage was >=", coverage_per * 100, "% in only",
        round((tile_n_cov/tile_n)*100, digits = 2), "% of tiles")
    cont <- readline("Do you wish to continue? [Y/N]")
    if(cont != "Y") stop("Matching aborted, consider reducing `coverage_per`")
  }else{
    message(paste("Coverage was >=", coverage_per * 100, "% in ",
                  round((tile_n_cov/tile_n)*100, digits = 2), "% of tiles"))
  }

  # prepare data to estimate all operative temperatures during each flight
  otm_ids <- unique(otm_splines$otm_id) # get list of unique OTM ids
  flights_times <- unique(
    flights_data[,c("year", "doy", "mod_start", "mod_end")]) # get list of unique flight times
  all_combos <- expand.grid(otm_id = otm_ids,
                            year = unique(flights_times$year),
                            doy = unique(flights_times$doy),
                            mod_start = unique(flights_times$mod_start),
                            mod_end = unique(flights_times$mod_end)) # generate all OTM flight combinations
  otm_preds <- merge(all_combos, flights_times,
                     by = c("year", "doy", "mod_start", "mod_end")) # merge
  otm_preds$pred_op_temp <- rep(NA, nrow(otm_preds)) # add column for OP temps

  # loop to predict operative temperatures during flight period
  for(i in 1:nrow(otm_preds)){

    # isolate the spline model for that otm_id, that year, that doy
    otm_specific_spline <- otm_splines |>
      dplyr::filter(get("otm_id") == otm_preds$otm_id[i]) |>
      dplyr::filter(get("year") == otm_preds$year[i]) |>
      dplyr::filter(get("doy") == otm_preds$doy[i])

    # predict temperatures for the period between mod_start and mod_end
    prediction <- stats::predict(otm_specific_spline$spline[[1]],
                                 c(otm_preds$mod_start[i]:otm_preds$mod_end[i]))$y

    # get average predicted temperature
    otm_preds$pred_op_temp[i] <- mean(prediction)
  }

  # prepare flights data for matching
  flights_data_cov$tile_id <- (flights_data_cov$y - min(flights_data_cov$y, na.rm=T) + 1) *
    (flights_data_cov$x - min(flights_data_cov$x, na.rm = T) + 1) # create tile column

  # build holder dataset
  matches <- unique(flights_data_cov[, c("x", "y", "tile_id", "coverage")])
  matches$otm_id <- NA
  matches$error <- NA
  matches <- as.data.frame(matches)

  # set up progress bar
  message("Matching tiles to OTMs...")
  pb <- utils::txtProgressBar(min=1,max=nrow(matches),style=3)

  # loop to estimate best OTM match for each tile
  for(i in 1:nrow(matches)){

    # merge tile data with OTm predictions
    tile_dat <- flights_data_cov[flights_data_cov$tile_id == matches$tile_id[i],]
    tile_dat <- tile_dat[,c("tile_id", "year", "doy", "mod_start", "mod_end", "op_temp")]
    tile_dat <- merge(tile_dat, otm_preds,
                      by = c("year", "doy", "mod_start", "mod_end"), all = TRUE)

    # find OTM match
    otm_match <- tile_dat |>
      dplyr::group_by(get("otm_id")) |>
      dplyr::summarise(error = mean(abs(get("pred_op_temp") - get("op_temp")), na.rm = T)) |>
      dplyr::filter(get("error") == min(get("error"), na.rm = T))
    colnames(otm_match) <- c("otm_id", "error")

    # add info to holder matches data set
    matches$otm_id[i] <- as.character(otm_match$otm_id[1])
    matches$error[i] <- otm_match$error[1]

    # print progress
    Sys.sleep(0.1)
    utils::setTxtProgressBar(pb, i)

  }

  # assign NA to innacurate matches and prepare dataset for return
  matches$otm_id <- ifelse(matches$error > error_max, NA, matches$otm_id)
  matches$error <- ifelse(is.na(matches$otm_id), NA, matches$error)
  matches <- matches[, c("x", "y", "coverage", "error", "otm_id")]
  message("Matching complete! Running posterior checks...")

  # run posterior checks about matching
  unassigned_tile_n <- length(matches$otm_id[is.na(matches$otm_id)])
  total_tile_n <- length(matches$otm_id)
  if(unassigned_tile_n/total_tile_n > 0.1){
    cat("Warning!", round((unassigned_tile_n/total_tile_n) * 100, digits = 2),
        "% of tiles could not be assigned to any OTM")
    cont <- readline("Do you wish to continue? [Y/N]")
    if(cont != "Y") stop("Matching aborted, consider increasing `error_max`")
  }else{
    print(paste(round((unassigned_tile_n/total_tile_n) * 100, digits = 2),
                "% of tiles could not be assigned to any OTM"))

  }

  message("Done!")
  return(matches)

}

