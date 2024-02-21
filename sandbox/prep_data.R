
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

## Read and process OTM data ----

# read metadata file
otms_metadata <- as_tibble(read.csv("data/otm_metadata.csv"))

# save metadata file as .RData
save(otms_metadata, file = "data/otms_metadata.RData")

# define directory where OTM files are stored
otms_path <- "data/otm_data"

# read and process all OTMs
otms_data <- rnp_otms_data(path = otms_path, rows_skip = 14, date_col = 1, op_temp_col = 3, metadata = otms_metadata)

# save OTMs data
save(otms_data, file = "data/otms_data.RData")

## Generate OTM splines ----

# generate OTM splines
otms_splines <- gen_otm_splines(otm_data = otms_data, knot_p = 1/7.5)

# save OTM splines fiel
otms_splines <- save(otms_splines, file = "data/otms_splines.RData")

## Correct flights data ----

# correct flights data
flights_data_corr <- correct_flights_data(flights_data = flights_data, otm_splines = otms_splines)

# save corrected flights data
save(flights_data_corr, file = "data/flights_data_corr.RData")

## Match data ----

# estimate matches
matches <- match_data(flights_data = flights_data_corr, otm_splines = otms_splines,
                      coverage_per = 0.9, error_max = 5)

# save matches data
save(matches, file = "data/matches.RData")



