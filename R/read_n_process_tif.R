### Read and process .TIF files

#' Read and process .tif files
#'
#' Reads a .tif file obtained from a drone flight and processes it into a tibble object
#'
#' @param file_path The location of the .tif file as a character.
#' @param utm_zone The universal transverse mercator (UTM) zone where the drone flight took place.
#' @param hemisphere The hemisphere where the drone flight took place.
#' @param digits The number of decimal digits to which coordinates should be rounded to. Will define the spatial accuracy of the flight
#'
#' @return A tibble with latitude (lat), longitude (long) and temperature (temp) as columns. Lat and long will have as many digits as specified above and temp will be in degrees C.
#'
#' @export

read_n_process_tif <- function(file_path, utm_zone, hemisphere, digits){

  ## Step 1: Read raster file and transform into a data frame ##

  # read tif file as raster
  raster <- raster(file_path)

  # transform raster into points
  points <- rasterToPoints(raster)

  # points into data frame
  df <- as.data.frame(points)

  # remove points with layer = 0, (no data)
  df <- df %>% filter(layer != 0)

  ## Step 2: Define projection string

  # define utm zone
  zone <- as.character(utm_zone)

  # generate projection string
  string <- paste("+proj=utm +zone=", zone," +", hemisphere," +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = "")

  ## Step 3: Transform x & y in df to coordinates

  coords <- as.data.frame(project(data.frame(x = df$x, y = df$y), string, inverse = TRUE))

  ## Step 4: Re-organize data and get final output

  # add temperature column
  coords$temp <- df$layer

  # rename x & y columns
  data <- coords %>% rename(long = x, lat = y)

  # define decimal degrees on latitude and longitude measurements
  data$long <- as.numeric(format(round(data$long, digits), nsmall = digits))
  data$lat <- as.numeric(format(round(data$lat, digits), nsmall = digits))

  # get mean temperature of same coordinates
  data <- data %>% group_by(lat, long) %>% summarise(temp = mean(temp, na.rm = T)) %>% ungroup()

  # final output
  return(data)

}


