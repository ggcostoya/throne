
## Testing the add flight info function ##

# load package
devtools::load_all()

# add info for read and processing function
filepath <-  "C:/Users/ggarc/OneDrive/projects/throne_data/example_flight_1.tif"

# process drone flight
p_flight <- read_n_process_flight_data(file_path = filepath, utm_zone = 11,
                           hemisphere = "north", digits = 5)

test_pflight <- p_flight

# read flight info file
flights_metadata <- read_csv("data/flights_metadata.csv")

add_flight_metadata <- function(flight_data, flight_id, flight_metadata){

  # extract flight metadata for that flight id
  flight_specific_metadata <- flight_metadata %>% filter(flight == flight_id)

  # add columns to flight data
  flight_data <- flight_data %>%
    mutate(height = flight_specific_metadata$height,
           date = flight_specific_metadata$date,
           time_start = flight_specific_metadata$time_start,
           time_end = flight_specific_metadata$time_end)

  # reshape date column
  flight_data$date <- mdy(flight_data$date)

  # get year in which flight took place
  year <- year(mdy(flight_specific_metadata$date))

  # get origin date for julian date transformation
  origin_date <- paste(year,"-01-01", sep = "")

  # add julian date column
  flight_data$date_julian <- julian(flight_data$date,
                                    origin = as.Date(origin_date))

  # reshape time start and time end column
  flight_data$minute_start <- hour(flight_data$time_start)*60 + minute(flight_data$time_start)

  # reshape time start and time end column
  flight_data$minute_end <- hour(flight_data$time_end)*60 + minute(flight_data$time_end)

  return(flight_data)

}

test_pflight %>% mutate(height = flights_metadata$height)


year <- year(mdy(flights_metadata$date))

add_flight_metadata(flight_data = p_flight,
                    flight_id = "example_flight_1",
                    flight_metadata = flights_metadata)
