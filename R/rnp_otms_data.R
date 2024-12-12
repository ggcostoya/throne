
## Read and process OTMs data ##

#' Read and process OTM data
#'
#' Read and processes data from multiple operative temperature model (OTM)
#' \code{.csv} files and adds metadata
#'
#' @param path A character specifying either the path for a single or the folder
#'    for multiple OTM files in \code{.csv} format. The file needs to have at least a
#'    column with information on the moment (i.e., date and time of the day) when
#'    each measurement was made and a column with an operative temperature value
#'    for each measurement
#' @param rows_skip An integer >= 0 indicating the rows to skip when reading
#'    OTM \code{.csv} files. If omitted, it defaults to \code{0}.
#' @param date_col An integer >= 1 indicating the column in the OTM \code{.csv} file
#'    that contains date of the measurement. Dates need to be in either MM/DD/YYYY
#'    HH:MM:SS, MM/DD/YYYY HH:MM or MM/DD/YYYY.
#' @param time_col An integer >= 1 indicating the column in the OTM \code{.csv} file
#'    that contains the time of the measurement. Times need to be in HH:MM:SS or HH:MM
#'    format. If not specified, it will try to extract the date information
#'    from \code{date_col}.
#' @param op_temp_col An integer >= 1 indicating the column column in the
#'    OTM \code{.csv} file that contains the operative temperature measurement
#' @param metadata A \code{tibble} or \code{data.frame} containing metadata related to each
#'    OTM. It must include an \code{otm_id} character column matching the names of each
#'    of the OTM files and numeric columns for \code{latitude} and \code{longitude}
#'
#' @return A \code{tibble} where each row represents a unique operative temperature
#'    measurement (\code{op_temp}) taken by a given OTM (specified by \code{otm_id}),
#'    that occurred in a given \code{year}, date of the year (\code{doy}) and minute of
#'    the day (\code{mod}). The \code{tibble} will have as many additional columns as
#'    metadata characteristics included in \code{metadata}.
#'
#' @export

rnp_otms_data <- function(path, rows_skip, date_col, time_col, op_temp_col,
                          metadata){

  # define rows to skip, time and date column if not specified
  rows_skip <- ifelse(missing(rows_skip),0,rows_skip)
  time_col <- ifelse(missing(time_col),date_col,time_col)
  if(time_col == date_col){
    message("`time_col` not specified, time will be obtained from date column")}

  # run checks
  if(!is.character(path)){stop("path must be a character string")}
  if(!is.data.frame(metadata)){stop("metadata must be a data frame")}
  if(rows_skip %% 1 != 0){stop("rows_skip must be an integer")}
  if(rows_skip < 0){stop("rows_skip must be an integer >= 0")}
  if(date_col %%1 != 0){stop("date_col must be an integer")}
  if(date_col < 1){stop("date_col must be an integer >= 1")}
  if(time_col%%1 != 0){stop("time_col must be an integer")}
  if(time_col < 1){stop("time_col must be an integer >= 1")}
  if(op_temp_col %%1 != 0){stop("op_temp_col must be an integer")}
  if(op_temp_col < 1){stop("op_temp_col must be an integer >= 1")}
  if(length(list.files(path)) == 0){stop("there are no files in path")}
  if("otm_id" %in% colnames(metadata) == F){stop("`otm_id` column missing in metadata")}
  if("latitude" %in% colnames(metadata) == F){stop("`latitude` column missing in metadata")}
  if("longitude" %in% colnames(metadata) == F){stop("`longitude` column missing in metadata")}
  if(any(is.na(metadata$latitude))){stop("`latitude` column has missing values")}
  if(any(is.na(metadata$longitude))){stop("`longitude` column has missing values")}
  if(any(is.na(metadata$otm_id))){stop("`otm_id` column has missing values")}

  # re-project coordinates of metadata file into UTM
  latlon <- data.frame(lon = metadata$longitude, lat = metadata$latitude)
  utm_zone <- mean(floor((latlon$lon + 180) / 6) + 1)
  new_crs <- paste("+proj=utm +zone=",as.character(utm_zone),
                   " +datum=WGS84 +units=m", sep = "")
  xy <- terra::vect(latlon, geom=c("lon","lat"), crs="+proj=longlat")
  xy <- terra::project(xy, new_crs)
  xy <- terra::geom(xy)
  metadata$x <- xy[,3]
  metadata$y <- xy[,4]

  # define files list and holder object
  otm_file_list <- list.files(path, full.names = TRUE)
  otm_data <- data.frame(otm_id = c(), year = c(), doy = c(),
                         mod = c(),op_temp = c())

  # set up progress bar
  message("Reading and processing OTMs data...")
  pb <- utils::txtProgressBar(min=1,max=length(otm_file_list),style=3)

  # loop through all files
  for(i in 1:length(otm_file_list)){

    # read OTM file
    otm_file <- utils::read.csv(otm_file_list[i], skip = rows_skip)

    # warnings if format of specific file is incorrect
    if(date_col > ncol(otm_file)){
      stop(paste(list.files(path)[i], "has less columns than `date_col`"))}
    if(op_temp_col > ncol(otm_file)){
      stop(paste(list.files(path)[i], "has less columns than `op_temp_col`"))}

    # extract id and rename columns of interest
    otm_id <- substr(list.files(path)[i],1, nchar(list.files(path)[i]) - 4)
    otm_file <- data.frame(date = otm_file[,date_col],
                           time = otm_file[,time_col],
                           op_temp = otm_file[,op_temp_col],
                           otm_id = rep(otm_id, nrow(otm_file)))

    # check data format and extract information of interest
    if(date_col == time_col){
      if(any(is.na(lubridate::mdy_hm(otm_file$date)))){
        stop(paste("incorrect `date` column format for",list.files(path)[i],
                   "it should be MM/DD/YYYY HH:MM"))}
      otm_file$year <- lubridate::year(lubridate::mdy_hm(otm_file$date))
      otm_file$doy <- lubridate::yday(lubridate::mdy_hm(otm_file$date))
      otm_file$mod <- lubridate::hour(lubridate::mdy_hm(otm_file$time)) * 60 +
        lubridate::minute(lubridate::mdy_hm(otm_file$time))
    }else{
      if(any(is.na(lubridate::mdy(otm_file$date)))){
        stop(paste("incorrect `date` column format for",list.files(path)[i],
                   "it should be MM/DD/YYYY"))}
      if(any(is.na(lubridate::hms(otm_file$time)))){
        stop(paste("incorrect `time` column for", list.files(path)[i],
                   "it should be HH:MM"))}
      otm_file$year <- lubridate::year(lubridate::mdy(otm_file$date))
      otm_file$doy <- lubridate::yday(lubridate::mdy(otm_data$date))
      otm_file$mod <- lubridate::hour(lubridate::hm(otm_file$time)) * 60 +
        lubridate::minute(lubridate::hm(otm_data$time))
    }

    # select columns of interest and bind
    otm_file <- otm_file[,c("otm_id","year","doy","mod","op_temp")]
    otm_data <- rbind(otm_data, otm_file)

    # print progress
    Sys.sleep(0.1)
    utils::setTxtProgressBar(pb, i)
    if(i == length(otm_file_list)){
      message(" OTM data reading and processing complete!")}

  }

  # merge with metadata and return
  otm_data <- merge(otm_data, metadata, by = "otm_id")
  return(otm_data)
}




