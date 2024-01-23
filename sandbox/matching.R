
## Matching workflow ##

## Load package ---------------------------------------------------------------

devtools::load_all()

## Load correlation data and get correction factors ---------------------------

# load data
load("data/corr_data.RData")

corr_data$minute <- corr_data$minute - 2

# get correction factors
correction_factors <- get_correction_factors(corr_data)

## Read flight metadata and read and process flights of interest --------------

# read flights metadata
flights_metadata <- read.csv("data/flights_metadata.csv")

# get the flight files list
folder <- "C:/Users/ggarc/OneDrive/research/throne_data"
flight_files_list <- paste(folder, "/", list.files(folder), sep = "")

# filter flights with GCP correction
flight_files_list <- flight_files_list[35:68]

# empty object to store flight data
flights_data <- data.frame(latitude = c(), longitude = c(), ref_temp = c(),
                           year = c(), date = c(), minute_start = c(), minute_end = c())

# loop to get all flights data
for(i in 1:length(flight_files_list)){

  # read and process flight data
  flight <- rnp_flight_data(file_path = flight_files_list[i], digits = 5)

  # add flight metadata
  flight <- add_flight_metadata(flight_data = flight,
                                flight_id = str_sub(list.files(folder)[i], end = -5),
                                flight_metadata = flights_metadata)

  # bind to holder dataset
  flights_data <- rbind(flights_data, flight)

  print(paste("You have ", round(100*(1 - i/length(flight_files_list)),2),"% left", sep = ""))

}

# apply correction factor
flights_data <- apply_correction(flight_data = flights_data, correction_factors = correction_factors)

# save flights data
save(flights_data, file = "data/flights_data.RData")

## Prepare drone data for matching --------------------------------------------

# get list of tiles that were covered
times_covered <- flights_data %>% group_by(latitude, longitude) %>%
  summarise(times_covered = n()) %>% ungroup()

# merge times covered with flight_data
flights_data <- merge(flights_data, times_covered, by = c("latitude", "longitude"))

# filter for a specific coverage
flights_data <- flights_data %>% filter(times_covered > 32)

# generate tile identifier column
flights_data$tile_id <- flights_data$latitude * flights_data$longitude

# filter rows were op temp was not predicted
flights_data <- flights_data %>% rename(op_temp = ref_temp)
flights_data <- flights_data %>% filter(op_temp != 0)

## Load OTM splines data ------------------------------------------------------

# load splines data
load("data/otm_splines.RData")

## Test matching --------------------------------------------------------------

# get list of unique tile IDs and generate matching OTM column
matches <- tibble(tile_id = unique(flights_data$tile_id),
                  otm_id = rep(NA, length(tile_id)))

# get list of unique OTMs
otm_ids <- unique(otm_splines$otm_id)

# start loop
for(i in 1:nrow(matches)){

  # filter data for specific tile id
  tile_dat <- flights_data %>% filter(tile_id == matches$tile_id[i])

  # object to store OTM matches
  otm_matches <- tibble(otm_id = c(), match = c())

  # loop to find OTM matches
  for(j in 1:length(otm_ids)){

    # filter splines for a given OTM
    otm_specific_splines <- otm_splines %>% filter(otm_id == otm_ids[j]) %>%
      dplyr::select(otm_id, date, splines)

    # merge otm specific splines with tile data
    tile_dat_merged <- as_tibble(merge(tile_dat, otm_specific_splines, by = "date"))

    # perdict temperatures
    tile_dat_merged <- tile_dat_merged %>%
      mutate(pred_temp = (predict(splines[[1]], minute_start)$y + predict(splines[[1]], minute_end)$y)/2) %>%
      group_by(otm_id) %>%
      summarise(match = mean(abs(op_temp - pred_temp), na.rm = T)) %>% ungroup()

    # bind with otm_matches holder dataset
    otm_matches <- rbind(otm_matches, tile_dat_merged)

  }

  # filter the OTM that lead to the smallest bias
  matches$otm_id[i] <- otm_matches %>% filter(match == min(match, na.rm = T)) %>%
    dplyr::select(otm_id) %>% pluck(1)

  print(i)

}

# add coordinates to each point
coords <- flights_data %>% dplyr::select(latitude, longitude, tile_id)
coords <- unique(coords)
matches <- merge(matches, coords, by = "tile_id")

# add OTM characteristics
otm_metadata <- read.csv("data/otm_metadata.csv")
otm_metadata <- otm_metadata %>% dplyr::select(otm_id, microhabitat, orientation)
matches <- merge(matches, otm_metadata, by = "otm_id")

save(matches, file = "data/matches.RData")

## Predicting thermal landscapes ----------------------------------------------

# prediction holder dataset
prediction <- tibble(latitude = c(), longitude = c(), date = c(),
                     minute = c(), pred_op_temp = c())

# minutes sequence
minutes_sequence <- seq(0, 1380, by = 10)

# loop to predict temperatures
for(i in 1:length(minutes_sequence)){

  # predict for minute x
  pred <- predict_thermal_landscape(matches = matches, otm_splines = otm_splines,
                                    date = 235, minute = minutes_sequence[i])

  # bind prediction
  prediction <- rbind(prediction, pred)

}

## Plotting -------------------------------------------------------------------
library(viridis)
library(gganimate)
library(gifski)
library(magick)
library(metR)
library(transformr)

flights_data_corr %>%
  filter(minute_start == 998) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_raster(aes(fill = ref_temp_cor2 - ref_temp)) +
  scale_fill_gradient2(low = "skyblue", mid = "white", high = "brown", limits = c(-12, 12), midpoint = 0) +
  theme_void() +
  theme(legend.position = c(0.25, 0.85)) +
  labs(fill = "Difference Post - Pre correction")


ggplot() +
  geom_raster(data = flights_data %>% group_by(latitude, longitude) %>% summarise(times = n()),
              aes(x = longitude, y = latitude, fill = times)) +
  geom_point(data = otm_metadata, aes(x = longitude, y = latitude), size = 4, shape = 21, fill = "brown") +
  theme_void() +
  theme(legend.position = c(0.15, 0.85)) +
  labs(fill = "Times captued in a flight")

matches %>%
  group_by(otm_id) %>% summarise(times = n()) %>%
  ungroup() %>% arrange(times) %>%
  ggplot(aes(x = fct_rev(fct_reorder(otm_id, times)), y = 100* (times/5839))) +
  geom_col(aes(fill = 100 * (times/5839)), col = "black") +
  ylab("% of pixels described") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = "none")

matches %>%
  group_by(otm_id) %>% summarise(times = n()) %>%
  ungroup() %>% arrange(desc(times)) %>%
  mutate(cum_times = cumsum(times)) %>%
  ggplot(aes(x = fct_reorder(otm_id, cum_times))) +
  geom_point(aes(y =  100* (cum_times/5839), col = cum_times), size = 3) +
  geom_col(aes(y = 100 * (times/5839), fill = 100 * (times/5839)), col = "black") +
  geom_hline(yintercept = c(50,75,90,95), col = "black", linetype = 2) +
  geom_hline(yintercept = 100, col = "black", linetype = 1) +
  ylab("Cummulative % of pixels described") +
  scale_y_continuous(limits = c(0,102), expand = c(0,0), breaks = c(0,50,75,90,95,100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = "none")

matches %>%
  group_by(orientation, otm_id) %>%
  summarise(times = n()) %>%
  ungroup() %>%
  arrange(desc(times)) %>%
  ggplot(aes(x = fct_rev(fct_reorder(otm_id, times)), fill = orientation)) +
  geom_col(aes(y = 100 * (times/5839)), col = "black") +
  ylab("% of pixels described") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = c(0.85,0.75))


ggplot() +
  geom_raster(data = matches, aes(x = longitude, y = latitude, fill = orientation)) +
  geom_contour(data = elevation, aes(x = longitude, y = latitude, z = elevation),
               linewidth = 0.25, binwidth = 1, alpha = 0.75, col = "white") +
  theme_minimal() +
  theme(panel.grid= element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

library(plotly)

x <- elevation$longitude
y <- elevation$latitude
z <- elevation$elevation
cols <- as.numeric(as.factor(matches$orientation))

# Create 3D mesh plot
p <- plot_ly(x = ~x, y = ~y, z = ~z, type = "mesh3d", color = ~cols)

# Add trace with colors based on matches$orientation
p <- add_trace(p, type = "mesh3d", intensity = ~cols, colorscale = "Viridis")

# Show plot
p


## plot moving thermal landscape
x <- prediction %>%
  mutate(minute = round(minute/60, digits = 0)) %>%
  ggplot(aes(x = longitude, y = latitude, fill = pred_op_temp)) +
  geom_raster() +
  scale_fill_viridis(option = "magma", limits = c(14.4, 57.8)) +
  theme_minimal() +
  theme(panel.grid= element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.25, 0.85)) +
  labs(fill = "Predicted operative temperature (C)",
       title = "Hour: {frame_time}") +
  transition_time(minute) +
  enter_fade() +
  exit_fade()
anim <- animate(x, height = 5.5, width = 5.5, units = "in", fps = 2, res = 200, renderer = gifski_renderer())
anim_save("test.gif", anim)

# plot moving thermal landscape with elevation

load("data/elevation.RData")

test <- merge(prediction, elevation, by = c("latitude", "longitude"))

## plot moving thermal landscape
x <- test %>%
  mutate(minute = round(minute/60)) %>%
  #filter(minute == 0) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_raster(aes(fill = pred_op_temp)) +
  geom_contour(aes(z = elevation),
               col = "white", alpha = 0.5, linewidth = 0.25, bins = 20) +
  geom_text_contour(aes(z = elevation),
                    label.placer = label_placer_flattest(ref_angle = 1),
                    skip = 2, stroke = 0, col = "white", size = 2) +
  scale_fill_viridis(option = "magma", limits = c(14.4, 57.8)) +
  theme_minimal() +
  theme(panel.grid= element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.25, 0.85)) +
  labs(fill = "Predicted operative temperature (C)",
       title = "Hour: {frame_time}") +
  transition_time(minute) +
  enter_fade() +
  exit_fade()
anim <- animate(x, height = 5.5, width = 5.5, units = "in", fps = 2, res = 200, renderer = gifski_renderer())
anim_save("test.gif", anim)


ggplot() +
  geom_raster(data = prediction %>% filter(minute == 1020),
              aes(x = longitude, y = latitude, fill = pred_op_temp)) +
  scale_fill_viridis(option = "magma", limits = c(14.4, 57.8)) +
  geom_contour(data = elevation,
               aes(x = longitude, y = latitude, z = elevation),
               col = "white", alpha = 0.5, linewidth = 0.25, bins = 20) +
  geom_text_contour(data = elevation,
                    aes(x = longitude, y = latitude, z = elevation),
                    label.placer = label_placer_flattest(ref_angle = 1),
                    skip = 0, stroke = 0, col = "white", size = 3) +
  theme_minimal() +
  theme(panel.grid= element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.25, 0.85)) +
  labs(fill = "Predicted operative temperature (C)",
       title = "Hour: 05:00 PM")




