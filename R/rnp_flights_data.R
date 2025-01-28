# Read and process flights data

#' Reads and processes data from multiple flights in \code{.tif} format and adds metadata
#'
#' @param path A character string indicating the directory where all \code{.tif}`
#'    files for flights are stored. The name of each flight \code{.tif} file must
#'    have associated metadata.
#' @param metadata A metadata \code{tibble} for the flights with information on
#'    the \code{flight_id} (character), \code{date} (MM/DD/YYYY), \code{time_start} (HH:MM)
#'    and \code{time_end} (HH:MM) of each flight.
#' @param resolution A numeric > 0.5 indicating the spatial resolution of the
#'    the final processed output in squared meters. This lower boundary is chosen
#'    to exceed the spatial resolution of most drone-mounted thermal imaging cameras.
#'
#' @return A \code{data.frame} where each row represents a unique tile (a unique
#'    combination of \code{x} and \code{y} UTM coordinates), with an associated
#'    surface temperature measurement (\code{surf_temp}) for a unique flight.
#'    It also provides columns with the \code{year}, month \code{month},
#'    day of the year \code{doy} the minute of the day when the flight started
#'    (\code{mod_start}) and ended (\code{mod_end})
#'
#' @export

rnp_flights_data <- function(path, metadata, resolution){

  # run checks
  if(!is.character(path)){stop("path must be a character string")}
  if(!is.data.frame(metadata)){stop("metadata must be a data frame")}
  if(!is.numeric(resolution)){stop("resolution must be a numeric value")}
  if(resolution < 0.5){stop("resolution must be >= 0.5")}
  if(length(list.files(path)) == 0){stop("there are no files in path")}
  if("flight_id" %in% colnames(metadata)==F){stop("`flight_id` column missing in metadata")}
  if("date" %in% colnames(metadata) == F){stop("`date` column missing in metadata")}
  if("time_start" %in% colnames(metadata) == F){stop("`time_start` column missing in metadata")}
  if("time_end" %in% colnames(metadata) == F){stop("`time_end` column missing in metadata")}
  if(any(is.na(suppressWarnings(lubridate::mdy(metadata$date)))) &
     any(is.na(suppressWarnings(lubridate::dmy(metadata$date)))) &
     any(is.na(suppressWarnings(lubridate::ymd(metadata$date))))){
    stop("incorrect `date` format in metadata, must be MM/DD/YYYY, DD/MM/YYYY or YYYY/MM/DD")}
  if(any(is.na(suppressWarnings(lubridate::hm(metadata$time_start))))){
    stop("incorrect `time_start` format in metadata, change to HH:MM")}
  if(any(is.na(suppressWarnings(lubridate::hm(metadata$time_end))))){
    stop("incorrect flight `time_end` format in metadata, change to HH:MM")}

  # transform date depending on date format
  if(any(is.na(suppressWarnings(lubridate::mdy(metadata$date)))) &
     any(is.na(suppressWarnings(lubridate::dmy(metadata$date))))){
    metadata$date <- lubridate::ymd(metadata$date)
  }
  else if(any(is.na(suppressWarnings(lubridate::mdy(metadata$date)))) &
          any(is.na(suppressWarnings(lubridate::ymd(metadata$date))))){
    metadata$date <- lubridate::dmy(metadata$date)
  }
  else{metadata$date <- lubridate::mdy(metadata$date)}

  # transform resolution from m^2 to m and round  to one decimal digit
  resolution <- round(sqrt(resolution), digits = 1)

  # modify format of metadata file and select columns of interest
  metadata <- metadata |>
    transform(year = as.numeric(format(date, "%Y")),
              month = as.numeric(format(date, "%m")),
              doy = lubridate::yday(date))
  metadata$time_start <- lubridate::hm(metadata$time_start)
  metadata$time_end <- lubridate::hm(metadata$time_end)
  metadata$mod_start <- lubridate::hour(metadata$time_start) * 60 +
    lubridate::minute(metadata$time_start)
  metadata$mod_end <- lubridate::hour(metadata$time_end) * 60 +
    lubridate::minute(metadata$time_end)
  metadata <- metadata[,c("flight_id","year","month","doy","mod_start","mod_end")]

  # list files and prepare holder for loop
  flight_files_list <- list.files(path, full.names = TRUE) # list all files
  flight_files_list <- flight_files_list[grepl(".tif", flight_files_list)] # keep only .tif files
  flights_data <- data.frame(x = c(), y = c(), year = c(), doy = c(),
                             mod_start = c(), mod_end = c(), surf_temp = c())

  # set up progress bar
  message("Reading and processing flights data...")
  pb <- utils::txtProgressBar(min=1,max=length(flight_files_list),style=3)

  # loop to read and add metadata to all flights in `path`
  for(i in 1:length(flight_files_list)){

    # read and process the flight
    flight_raster <- terra::rast(flight_files_list[i]) # read raster
    flight_raster <- flight_raster[[names(flight_raster)[1]]] # get first layer
    scale_factor <- round(resolution/terra::res(flight_raster)) # scale resolution
    flight_raster <- terra::aggregate(flight_raster,fact=scale_factor, fun = "mean") # re-scale and summarise
    flight <- as.data.frame(flight_raster, xy = TRUE) # convert to df
    colnames(flight) <- c("x","y","surf_temp") # rename columns
    flight <- flight[flight$surf_temp != 0, ] # remove rows that are exactly 0
    flight <- unique(flight)

    # round data to standardize (Noa's trick)
    flight$x <- round(flight$x * (1/resolution)) / (1/resolution)
    flight$y <- round(flight$y * (1/resolution)) / (1/resolution)

    # summarise values of tiles with the same x and y
    flight <- flight |>
      dplyr::group_by(get("x"),get("y")) |>
      dplyr::summarise(surf_temp = mean(get("surf_temp"), na.rm = T),.groups = "keep") |>
      dplyr::ungroup()

    # change column nanmes
    colnames(flight) <- c("x", "y","surf_temp")

    # extract metadata for specific file and add it to processed raster
    flight_metadata <- metadata[metadata$flight_id == substr(
      list.files(path)[i],1, nchar(list.files(path)[i]) - 4),]
    flight <- flight |>
      transform(year = rep(flight_metadata$year, nrow(flight)),
                doy = rep(flight_metadata$doy, nrow(flight)),
                mod_start = rep(flight_metadata$mod_start, nrow(flight)),
                mod_end = rep(flight_metadata$mod_end, nrow(flight)))

    # bind to holder
    flights_data <- rbind(flights_data, flight)

    # print progress
    Sys.sleep(0.1)
    utils::setTxtProgressBar(pb, i)
    if(i == length(flight_files_list)){
      message("Flight data reading and processing complete!")}
  }

  # rename columns
  colnames(flights_data) <- c("x", "y", "year","doy","mod_start","mod_end","surf_temp")

  return(flights_data)
}
