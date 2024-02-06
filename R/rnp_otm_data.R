### Read and process (RNP) OTM deta ###

#' Read and process OTM data
#'
#' Reads OTM files from a specified folder and returns a processed data frame.
#'
#' @param folder_path Folder where raw OTM files (in .csv format) are stored.
#' @param rows_skip Rows to skip on each OTM file to get to the data. It defaults to zero if not specified.
#' @param time_col Column within an OTM file where the "time" variable is stored.
#' @param op_temp_col Column within an OTM file where the operative temperature ("op_temp") variable is stored.
#'
#' @return A data frame with otm_id, year, date (Julian), minute (of the day) and operatie temperature.
#'
#' @export rnp_otm_data

rnp_otm_data <- function(folder_path, rows_skip, time_col, op_temp_col){

  # define rows to skip if parameter missing
  rows_skip <- ifelse(missing(rows_skip), 0, rows_skip)

  # define list of all OTM files in the specified folder
  otm_file_list <- paste(folder_path, "/", list.files(folder_path), sep = "")

  # generate empty storage object
  otm_data <- data.frame(raw_time = c(), op_temp = c(), otm_id = c())

  # start loop to read and process each file
  for(i in 1:length(otm_file_list)){

    # read OTM file
    otm_file <- read.csv(otm_file_list[i], skip = rows_skip)

    # extract OTM id
    otm_id <- str_sub(otm_file_list[i], start = str_length(folder_path) + 2, end = -5)

    # select and rename columns of interest
    otm_file <- data.frame(raw_time = otm_file[,time_col],
                           op_temp = otm_file[,op_temp_col],
                           otm_id = rep(otm_id, nrow(otm_file)))

    # bind OTM file to storage object
    otm_data <- rbind(otm_data, otm_file)

  }

  # extract year column
  otm_data$year <- year(mdy_hms(otm_data$raw_time))

  # extract date column
  otm_data$date <-floor(as.numeric(julian(mdy_hms(otm_data$raw_time),
                                          origin = paste(mean(year(mdy_hms(otm_data$raw_time))),
                                                         "-01-01", sep = ""))))

  # extract minute of the day
  otm_data$minute <- hour(mdy_hms(otm_data$raw_time))*60 + minute(mdy_hms(otm_data$raw_time))

  # reformat final object
  otm_data <- otm_data %>% dplyr::select(otm_id, year, date, minute, op_temp)


  return(otm_data)

}
