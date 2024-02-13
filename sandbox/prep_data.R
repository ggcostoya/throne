
## Prepare necessary data for package ##

## Load throne ----

devtools::load_all()

## Read and process flights data ----

# read metadata file
flights_metadata <- read.csv("data/flights_metadata.csv")

# save metadata file as .RData
save(flights_metadata, file = "data/flights_metadata.RData")

# define directory where files are stored
flights_path <- "C:/Users/ggarc/OneDrive/research/throne_data/gcp_flights"

# read and process all flights
flights_data <- rnp_flights_data(path = flights_path, metadata = flights_metadata, digits = 5)

# save flights data
save(flights_data, file = "data/flights_data.RData")


