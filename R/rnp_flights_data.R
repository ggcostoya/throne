## Read and process flights data ##

#' Read and process flights data
#'
#' Reads and processes data from multiple flights in \code{.tif} format and adds metadata
#'
#' @param path A character indicating the directory where all \code{.tif}`
#'    files for flights are stored. The name of each flight \code{.tif} file must
#'    have associated metadata for the function to work.
#' @param metadata A metadata \code{tibble} for the flights with information on
#'    the \code{flight_id} (character), \code{date} (MM/DD/YYYY), \code{time_start} (HH:MM)
#'    and \code{time_end} (HH:MM) of each flight.
#' @param digits An integer (>0 & <6), indicating the number of decimal digit places
#'    to which the final output should be summarized to. Will dictate the spatial
#'    resolution of each of the tiles in the eventual thermal landscape.
#'
#' @return A \code{tibble} where each row represents a unique tile (a unique combination
#'    of \code{longitude} and \code{latitude}), with an associated IR surface temperature measurement
#'    (\code{ir_temp}) for a unique flight. To identify each flight, each row of the
#'    return \code{tibble} also contains information on the \code{year}, date of the year (\code{doy})
#'    and the minute of the day (\code{mod}) when the flight started (\code{mod_start}) and ended (\code{mod_end})
#'
#' @export

rnp_flights_data <- function(path, metadata, digits){

  ## checks

  # return error message if no files found on specified folder
  if(length(list.files(path)) == 0){stop("There are no files in the specified folder")}

  # check that format of all files listed in the folder is .tif
  if(any(tools::file_ext(list.files(path)) != "tif")){stop("Some files in the specified folder are not `.tif` format")}

  # check that metadata file has all necessary columns
  if("flight_id" %in% colnames(metadata) == FALSE){stop("`flight_id` column is missing in metadata file")}
  if("date" %in% colnames(metadata) == FALSE){stop("Flight `date` column missing in metadata file")}
  if("time_start" %in% colnames(metadata) == FALSE){stop("Flight `time_start` is column missing in metadata file")}
  if("time_end" %in% colnames(metadata) == FALSE){stop("Flight `time_end` is column missing in metadata file")}

  # check proper formatting on metadata file
  if(any(is.na(mdy(metadata$date)))){stop("Incorrect `date` column format, it should be MM/DD/YYYY")}
  if(any(is.na(hm(metadata$time_start)))){stop("Incorrect flight `time_start` column format, it should be HH:MM")}
  if(any(is.na(hm(metadata$time_end)))){stop("Incorrect flight `time_end` column format, it should be HH:MM")}

  # check if digits is an integer
  if(digits %in% !c(1,2,3,4,5,6)){stop("Incorrect value for `digits`, it should be an integer between 1 and 6")}

  ## preparing data

  # modify format of metadata file and select columns of interest
  metadata$year <- lubridate::year(mdy(metadata$date))
  metadata$doy <- yday(mdy(metadata$date))
  metadata$mod_start <- lubridate::hour(hm(metadata$time_start))*60 + lubridate::minute(hm(metadata$time_start))
  metadata$mod_end <- lubridate::hour(hm(metadata$time_end))*60 + lubridate::minute(hm(metadata$time_end))
  metadata <- metadata %>% dplyr::select(flight_id, year, doy, mod_start, mod_end)

  # list files within path specified
  flight_files_list <- paste(path, "/", list.files(path), sep = "")

  # build holder flights data
  flights_data <- tibble(longitude = c(), latitude = c(), ir_temp = c(),
                         year = c(), doy = c(), mod_start = c(), mod_end = c())

  ## reading and processing data in loop

  # loop to read and add metadata to all flights in folder
  for(i in 1:length(flight_files_list)){

    # read .TIF file as a raster
    flight_raster <- raster(flight_files_list[i])

    # re-project raster into latitude and longitude
    flight_raster <- projectRaster(flight_raster, crs = "+proj=longlat +datum=WGS84 +no_defs")

    # read raster and transform into data frame and change column names
    flight <- as.data.frame(rasterToPoints(flight_raster))
    colnames(flight) <- c("longitude", "latitude", "ir_temp")
    flight <- flight %>% filter(ir_temp != 0)

    # define decimal degrees on latitude and longitude measurements
    flight$longitude <- as.numeric(format(flight$longitude, nsmall = digits))
    flight$latitude <- as.numeric(format(flight$latitude, nsmall = digits))

    # get mean temperature of same coordinates
    suppressMessages(flight <- flight %>%
      group_by(longitude, latitude) %>%
      summarise(ir_temp = mean(ir_temp, na.rm = T)) %>%
      ungroup())

    # extract metadata for the flight of interest
    flight_metadata <- metadata %>% filter(flight_id == str_sub(list.files(path)[i], end = -5))

    # add flight metadata
    flight <- flight %>% mutate(year = flight_metadata$year,
                                doy = flight_metadata$doy,
                                mod_start = flight_metadata$mod_start,
                                mod_end = flight_metadata$mod_end)

    # rbind flight with flights_data
    flights_data <- rbind(flights_data, flight)

    # print loop counter
    print(paste(i,"out of", length(flight_files_list), "flight data files read & processed"))

    # print end message
    if(i == length(flight_files_list)){print("Flight data reading and processing complete!")}

  }

  ## return

  return(flights_data)

}

