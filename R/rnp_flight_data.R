
#' Read and process flight data
#'
#' Read and process thermal imaging data from a drone flight (in `.tif` format)
#'
#' @param file_path The path to the folder where the `.tif` file is stored.
#' @param digits The number of decimal digits to which the final output should be summarized to.

#'
#' @return Pending
#'
#' @export

rnp_flight_data <- function(file_path, digits){

  # read .TIF file as a raster
  raster <- raster(file_path)

  # transform raster to points
  points <- as.data.frame(rasterToPoints(raster))

  # remove observations where the value was exactly 0 (no data for them)
  points <- points %>% filter(as.data.frame(points)[,3] != 0)

  # transform points into a vector
  vector <- terra::vect(cbind(points$x, points$y), crs = projection(raster))

  # re-project vector into longitude and latitude
  lonlat <- data.frame(geom(terra::project(vector,"+proj=longlat +datum=WGS84"))[,c("x","y")])

  # add temperature column
  lonlattemp <- cbind(lonlat, points[,3])

  # rename columns
  colnames(lonlattemp) <- c("longitude", "latitude", "ref_temp")

  # define decimal degrees on latitude and longitude measurements
  lonlattemp$longitude <- as.numeric(format(lonlattemp$longitude, nsmall = digits))
  lonlattemp$latitude <- as.numeric(format(lonlattemp$latitude, nsmall = digits))

  # get mean temperature of same coordinates
  lonlattemp <- lonlattemp %>% group_by(latitude, longitude) %>%
    summarise(ref_temp = mean(ref_temp, na.rm = T)) %>% ungroup()

  return(lonlattemp)

}
