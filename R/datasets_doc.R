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
#' as_tibble(flights_metadata)

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

### * elevation

#' Elevation profile of the area where we conducted the project at a spatial
#' resolution of ~ 1m^2
#'
#' @format A \code{tibble} with 8170 rows and 3 columns, all numeric:
#'\describe{
#'  \item{longitude}{Longitude in decimal degrees}
#'  \item{latitude}{Latitude in decimal degrees}
#'  \item{elevation}{Elevation in meters}
#' }
#'
#'@examples
#'elevation

"elevation"

### * bad read OTM

#' OTM raw data file read incorrectly
#'
#' Example used in the vignettes of the package to illustrate what happens when
#' a raw OTM file is read incorrectly.
#'
#'@examples
#'bad_read_otm

"bad_read_otm"

### * good read OTM

#' OTM raw data file read correctly
#'
#' Example used in the vignettes of the package to illustrate what happens when
#' a raw OTM file is read correctly
#'
#'@examples
#'good_read_otm

"good_read_otm"

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
#' as_tibble(otms_data)

"otms_data"

### * OTMs splines

#' Example OTM splines nested data
#'
#' A nested \code{tibble} with smoothing splines models for each OTM during each
#' day while deployed in the field to validate the methodology of this package.
#' Each row corresponds to a unique OTM and day of the year combination. The data
#' set contains metadata information on the deployment of the OTMs. This data set
#' was generated from the data set \code{otms_data} using the \code{gen_otm_splines}
#' function specifying \code{knot_p = 4}.
#'
#' @format A \code{tibble} of 132 rows and 9 columns:
#' \describe{
#' \item{otm_id}{Character column with a unique identifier for each OTM}
#' \item{year}{A numeric column indicating year}
#' \item{doy}{A numeric column indicating day of the year}
#' \item{microhabitat}{Character column describing the microhabitat where the OTM was deployed,
#'  between \code{"outrcrop"}, \code{"small_rock"}, \code{"rock"},
#'  \code{"boulder"}, \code{"ground"}, and \code{"bush"}}
#' \item{orientation}{Character column describing the orientation at which the OTM was deployed,
#'  between \code{"N"}, \code{"NE"}, \code{"E"}, \code{"SE"}, \code{"S"}, \code{"SW"},
#'  \code{"W"}, \code{"NW"} and \code{"Flat"}}
#' \item{latitude}{Latitude where the OTM was deployed in decimal degrees}
#' \item{longitude}{Longitude where the OTM was deployed in decimal degrees}
#' \item{elevation}{Elevation where the OTM was deployed in meters}
#' \item{spline}{A nested column containing the smoothing spline model}
#' }
#'
#' @examples
#'
#' otms_splines
#'
#' # when unnesting a spline model
#' otms_splines$spline[[2]]

"otms_splines"

































