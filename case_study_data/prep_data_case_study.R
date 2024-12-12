
## Prepare data for case study ##

# load throne package
library(throne)

# set local path to flights and OTMs folder
flights_path <- "C:/Users/ggarc/OneDrive/research/throne-manuscript/flights_data/case_flights"
otms_path <- "C:/Users/ggarc/OneDrive/research/throne-manuscript/data/otm_data/case_study"

# load flights and OTms metadata
load("case_study_data/c_flights_metadata.RData")
load("case_study_data/c_otms_metadata.RData")

# read and process flights data at 1.5 m resolution
c_flights_data <- rnp_flights_data(flights_path, c_flights_metadata, res = 1.5)

save(c_flights_data, file = "case_study_data/c_flights_data.RData")

# read and process OTMs data
c_otms_data <- rnp_otms_data(path = otms_path,
                             metadata = c_otms_metadata,
                             rows_skip = 0,
                             date_col = 1,
                             op_temp_col = 2)

save(c_otms_data, file = "case_study_data/c_otms_data.RData")


# generate OTM splines
c_otms_splines <- gen_otm_splines(otm_data = c_otms_data, knot_p = 1)

save(c_otms_splines, file = "case_study_data/c_otms_splines.RData")

# run matches
c_matches <- match_data(flights_data = c_flights_data_corr,
                        otm_splines = c_otms_splines,
                        coverage_per = 0.9,
                        error_max = 100)

save(c_matches, file = "case_study_data/c_matches.RData")


# re-project lizard mr data
load("case_study_data/lizard_mr.rda")

# re-project coordinates of metadata file into UTM
latlon <- data.frame(lon = lizard_mr$longitude, lat = lizard_mr$latitude)
utm_zone <- mean(floor((latlon$lon + 180) / 6) + 1)
new_crs <- paste("+proj=utm +zone=",as.character(utm_zone),
                 " +datum=WGS84 +units=m", sep = "")
xy <- terra::vect(latlon, geom=c("lon","lat"), crs="+proj=longlat")
xy <- terra::project(xy, new_crs)
xy <- terra::geom(xy)
lizard_mr$x <- xy[,3]
lizard_mr$y <- xy[,4]

save(lizard_mr, file = "case_study_data/lizard_mr.RData")
