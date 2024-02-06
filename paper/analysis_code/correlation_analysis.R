
## Correlation analysis ##

## Load `throne` package ------------------------------------------------------

devtools::load_all()

## Reading and processing OTM data --------------------------------------------

# read and process OTM data
otm_data <- rnp_otm_data(folder_path = "data/otm_data",
                         rows_skip = 14, time_col = 1, op_temp_col = 3)

# read OTM metadata
otm_metadata <- read.csv("data/otm_metadata.csv")

# add OTM metadata
otm_data <- add_otm_metadata(otm_data = otm_data, otm_metadata = otm_metadata)

# generate OTM splines
otm_splines <- gen_otm_splines(otm_data = otm_data, knot_p = 96/720)

# save otm splines file
save(otm_splines, file = "data/otm_splines.RData")

## Reading and processing flights data & generating validation dataset --------

# read flights metadata
flights_metadata <- read.csv("data/flights_metadata.csv")

# get the flight files list
folder <- "C:/Users/ggarc/OneDrive/research/throne_data"
flight_files_list <- paste(folder, "/", list.files(folder), sep = "")

# generate validation data holder data frame
corr_data <- data.frame(latitude = c(), longitude = c(), minute = c(),
                        op_temp = c(), ref_temp = c(), gcps = c(),
                        year = c(), date = c())

# loop to generate validation data
for(i in 1:length(flight_files_list)){

  # read and process flight data
  flight <- rnp_flight_data(file_path = flight_files_list[i], digits = 5)

  # add flight metadata
  flight <- add_flight_metadata(flight_data = flight,
                                flight_id = str_sub(list.files(folder)[i], end = -5),
                                flight_metadata = flights_metadata)

  # generate corr data for given flight
  corr_data_flight <- gen_corr_data(flight = flight, splines = otm_splines)

  # add column to indicate GCPs present
  gcps <- flights_metadata$gcps[flights_metadata$flight_id == str_sub(list.files(folder)[i], end = -5)]
  corr_data_flight$gcps <- rep(gcps, nrow(corr_data_flight))

  # add columns for year and date
  corr_data_flight$year <- mean(flight$year, na.rm = T)
  corr_data_flight$date <- mean(flight$date, na.rm = T)

  # bind to holder dataset
  corr_data <- rbind(corr_data, corr_data_flight)

}

## Get correction factors -----------------------------------------------------

# filter correction data only with GCPs
corr_data_gcps <- corr_data %>% filter(gcps == "Y")

# get correction factors
corr_factors_gcps <- get_correction_factors(corr_data = corr_data_gcps)
