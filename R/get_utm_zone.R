### Get UTM zone

#' Get UTM zone from a longitude value
#'
#' Returns the universal transverse mercator (UTM) projection zone of a given longitudinal value
#'
#' @param longitude A decimal longitude value between 180 and -180. We recommend this being the average longitude of your site.
#'
#' @return An integer between 1 and 60 indicating the UTM zone
#'
#' @details Determined using the equation:
#' \deqn{UTM_{zone} = \lfloor \frac{longitude + 180}{6} \rfloor + 1}
#'
#' @examples
#'
#' get_utm_zone(longitude = -119.45)
#' get_utm_zone(longitude = 2)
#'
#' @export get_utm_zone

get_utm_zone <- function(longitude){

  zone <- ifelse(longitude > 180, "Error: Longitude values need to be between -180 & 180",
                 ifelse(longitude < - 180, "Error: Longitude values need to be between -180 & 180",
                        floor((longitude + 180)/6) + 1))

  return(zone)

}
