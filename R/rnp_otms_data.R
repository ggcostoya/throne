## Read and process OTMs data ##

#' Read and process OTM data
#'
#' Read and processes data from multiple operative temperature model (OTM) `.csv` files and
#' adds metadata
#'
#' @param path A character specifying either the path for a single or the folder
#'    for multiple OTM files in `.csv` format. The file needs to have at least a
#'    column with information on the moment (i.e., date and time of the day) when
#'    each measurement was made and a column with an operative temperature value
#'    for each measurement
#' @param rows_skip A positive integer indicating the rows to skip when reading
#'    OTM `.csv` files. If omitted, it defaults to `0`.
#' @param date_col A positive integer indicating the column in the OTM `.csv` file
#'    that contains date of the measurement. Dates need to be in either MM/DD/YYYY
#'    HH:MM:SS, MM/DD/YYYY HH:MM or MM/DD/YYYY.
#' @param time_col A positive integer indicating the column in the OTM `.csv` file
#'    that contains the time of the measurement. Times need to be in HH:MM:SS or HH:MM
#'    format. If not specified, it will try to extract the date information
#'    from `date_col`.
#' @param op_temp_col A positive integer indicating the column column in the
#'    OTM `.csv` file that contains the operative temperature measurement
#' @param metadata A `tibble` or `data.frame` containing metadata related to each
#'    OTM. It needs to have an `otm_id` character column matching the names of each
#'    of the OTM files.
#'
#' @return A `tibble` where each row represents a unique operative temperature
#'    measurement (`op_temp`) taken by a given OTM (specified by `otm_id`),
#'    that occurred in a given `year`, date of the year (`doy`) and minute of
#'    the day (`mod`). The `tibble` will have as many additional columns as
#'    metadata characteristics included in `metadata`.
#'
#' @export

rnp_otms_data <- function(path, rows_skip, date_col, time_col, op_temp_col, metadata){

  ## checks and dealing with missing arguments

  # check if file is for a single file or a folder
  single_multiple <- ifelse(length(list.files(path)) == 0, "single", "multiple")

  # if a folder instead of a single file is specified
  if(single_multiple == "multiple"){

    # return error message if no files found on specified folder
    if(length(list.files(path)) == 0){stop("There are no files in the specified folder")}

    # check that format of all files listed in the folder is .tif
    if(any(tools::file_ext(list.files(path)) != "csv")){stop("Some files in the specified folder are not `.csv` format")}

  }

  # check that arguments are valid
  if(rows_skip < 0){stop("`rows_skip` must be positive")}
  if(date_col < 0){stop("`date_col` must be positive")}
  if(op_temp_col < 0){stop("`op_temp_col` must be positive")}
  if(date_col == op_temp_col){stop("Specify different rows for `date_col` and `op_temp_col`")}

  # check that metadata file has necessary columns
  if("otm_id" %in% colnames(metadata) == FALSE){stop("`otm_id` column is missing in metadata file")}

  # define rows_skip if parameter is missing
  rows_skip <- ifelse(missing(rows_skip), 0, rows_skip)

  # define time column as date column if not defined
  time_col <- ifelse(missing(time_col), date_col, time_col)
  if(time_col == date_col){warning("`time_col` not specified, time will be obtained from date column")}

  ## loop to read and process data

  # define list of all OTM files in the specified folder
  otm_file_list <- if (single_multiple == "single") {path} else {
    paste(path, "/", list.files(path), sep = "")
  }

  # generate empty storage object
  otm_data <- data.frame(date = c(), time = c(), op_temp = c(), otm_id = c())

  # start loop to read and process each file
  for(i in 1:length(otm_file_list)){

    # read OTM file
    otm_file <- read.csv(otm_file_list[i], skip = rows_skip)

    # check that `date_col` and `op_temp_col` are correct
    if(single_multiple == "single"){

      if(date_col > ncol(otm_file)){stop("The file has less columns than `date_col`")}
      if(op_temp_col > ncol(otm_file)){stop("The file has less columns than `op_temp_col`")}

    }else{

      if(date_col > ncol(otm_file)){stop(paste("The file", list.files(path)[i], "has less columns than `date_col`"))}
      if(op_temp_col > ncol(otm_file)){stop(paste("The file", list.files(path)[i], "has less columns than `op_temp_col`"))}

    }

    # extract
    otm_id <- gsub("^.*\\/(.*)\\.csv$", "\\1", otm_file_list[i])

    # select and rename columns of interest
    otm_file <- data.frame(date = otm_file[,date_col],
                           time = otm_file[,time_col],
                           op_temp = otm_file[,op_temp_col],
                           otm_id = rep(otm_id, nrow(otm_file)))

    # bind OTM file to storage object
    otm_data <- rbind(otm_data, otm_file)

  }

  ## read process date time columns

  # extract DOY and MOD from OTM data
  if(date_col == time_col){

    # check if `date` column has correct format
    if(all(is.na(mdy_hm(otm_data$date)))){stop("Incorrect format in `date` column, it should be MM/DD/YYYY HH:MM:SS or MM/DD/YYYY HH:MM")}

    otm_data$year <- year(mdy_hm(otm_data$date))
    otm_data$doy <- yday(mdy_hm(otm_data$date))
    otm_data$mod <- hour(mdy_hm(otm_data$time)) * 60 + minute(mdy_hm(otm_data$time))

  }else{

    # check if `date` and `time` column have correct format
    if(all(is.na(mdy(otm_data$date)))){stop("Incorrect format in `date` column, it should be MM/DD/YYY")}
    if(all(is.na(hms(otm_data$time)))){stop("Incorrect format in `time` column, it should be HH:MM or HH:MM:SS")}

    otm_data&year <- year(mdy(otm_data$date))
    otm_data$doy <- yday(mdy(otm_data$date))
    otm_data$mod <- hour(hm(otm_data$time)) * 60 + minute(mdy_hm(otm_data$time))

  }


  # select columns of interest
  otm_data <- otm_data %>% dplyr::select(otm_id, year, doy, mod, op_temp)

  ## adding metadata
  otm_data <- merge(otm_data, metadata, by = "otm_id")

  # return
  return(otm_data)

}
