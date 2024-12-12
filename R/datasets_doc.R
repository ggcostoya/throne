## Documentation for available data sets in the `throne` package

# Example data

### * flights metadata

#' Metadata for example flights
#'
#' @format A data frame with 34 rows and 4 columns:
#' \describe{
#'  \item{flight_id}{Character column with a unique identifier for each flight}
#'  \item{date}{Character column with the date when the flight took place in DD/MM/YYYY format}
#'  \item{time_start}{Character column with the time when the flight started in HH:MM format}
#'  \item{time_end}{Character column with the time when the flight ended in HH:MM format}
#'  }
#'
#' @examples
#' tibble::as_tibble(flights_metadata)

"flights_metadata"

### * flights data

#' Example flights data
#'
#' A data set of 33 flights processed through the \code{rnp_flights_data} function
#' Each row in the data represents the surface temperature measured recorded in a
#' given tile (i.e., unique latitude and longitude combination) at the moment in
#' time when a flight took place (specified by a unique year, day of the year
#' and minute of the day).
#'
#' @format A \code{tibble} with 263008 rows and 7 columns, all numeric:
#' \describe{
#'  \item{longitude}{Longitude of a given tile in decimal degrees}
#'  \item{latitude}{Latitude of a given tile in decimal degrees}
#'  \item{ir_temp}{Surface temperature measured via an IR camera}
#'  \item{year}{Year when the flight took place}
#'  \item{doy}{Day of the year when the flight took place}
#'  \item{mod_start}{Minute of the day when the flight started}
#'  \item{mod_end}{Minute of the day when the flight ended}
#' }
#'
#' @examples
#' flights_data

"flights_data"

### * OTMs metadata

#' Metadata for example OTMs
#'
#' @format A \code{tibble} with 36 rows and 6 columns:
#' \describe{
#'  \item{otm_id}{Character column with a unique identifier for each OTM}
#'  \item{microhabitat}{Character column describing the microhabitat where the OTM was deployed,
#'  between \code{"outrcrop"}, \code{"small_rock"}, \code{"rock"},
#'  \code{"boulder"}, \code{"ground"}, and \code{"bush"}}
#'  \item{orientation}{Character column describing the orientation at which the OTM was deployed,
#'  between \code{"N"}, \code{"NE"}, \code{"E"}, \code{"SE"}, \code{"S"}, \code{"SW"},
#'  \code{"W"}, \code{"NW"} and \code{"Flat"}}
#'  \item{latitude}{Latitude where the OTM was deployed in decimal degrees}
#'  \item{longitude}{Longitude where the OTM was deployed in decimal degrees}
#'  \item{elevation}{Elevation where the OTM was deployed in meters}
#'  }
#'
#' @examples
#' otms_metadata

"otms_metadata"

### * OTMs data

#' Example OTMs data
#'
#' A data set of 36 OTMs deployed in the field recording for 3 days at a rate of
#' 1 observation every 2 minutes. This data set was obtained using the \code{rnp_otms_data}
#' function and incorporates some metadata for each OTM. Each row corresponds to
#' a unique operative temperature measurement recorded by a unique OTM at a given
#' moment in time (i.e., unique year, \code{doy} and \code{mod}).
#'
#' @format A \code{data.frame} with 67584 rows and 10 columns
#' \describe{
#' \item{otm_id}{Character column with a unique identifier for each OTM}
#' \item{year}{A numeric column indicating year}
#' \item{doy}{A numeric column indicating day of the year}
#' \item{mod}{A numeric column indicating minute of the day}
#' \item{op_temp}{A numeric column indicating operative temperature}
#' \item{microhabitat}{Character column describing the microhabitat where the OTM was deployed,
#'  between \code{"outrcrop"}, \code{"small_rock"}, \code{"rock"},
#'  \code{"boulder"}, \code{"ground"}, and \code{"bush"}}
#' \item{orientation}{Character column describing the orientation at which the OTM was deployed,
#'  between \code{"N"}, \code{"NE"}, \code{"E"}, \code{"SE"}, \code{"S"}, \code{"SW"},
#'  \code{"W"}, \code{"NW"} and \code{"Flat"}}
#' }
#'
#' @examples
#' tibble::as_tibble(otms_data)

"otms_data"

### * Matches

#' Example matches data
#'
#' A \code{data.frame} indicating the OTM that best described the dynamics of a
#' given tile. This data was generated using the \code{match_data} function using
#' a flights data set processed using \code{rnp_flights_data} with a spatial resolution
#' of 1 m2 (the \code{flights_data} object included in \code{throne}), an OTM splines
#' \code{tibble} generated using \code{gen_otm_splines} setting the
#' \code{knot_p = 0.02} built from the \code{otms_data} data set
#' that is also included in \code{throne}, and by setting \code{error_max = 100}
#' and \code{coverage_per = 0.9}.
#'
#' @format A \code{data.frame} of 6236 rows and 5 columns
#' \describe{
#' \item{x}{A numeric column indicating the horizontal UTM coordinate}
#' \item{y}{A numeric column indicating the vertical UTM coordinate}
#' \item{otm_id}{A character column indicating the OTM that best describe the dynamics of a tile,
#'  a value of \code{NA} indicates that that tile could not be matched with any OTM with an
#'  \code{error < error_max}}
#' \item{error}{A numeric column indicating the average absolute error between the
#' thermal dynamics of a tile and the OTM that best described it}
#' \item{coverage}{A numeric column indicating the coverage of a tile across all flights}
#' }
#'
#' @examples
#' matches

"matches"




























