
## Correlation workflow ##

## Load package -----

devtools::load_all()

## Reading and processing OTM data --------------------------------------------

# read and process OTM data
otm_data <- rnp_otm_data(folder_path = "data/otm_data", rows_skip = 14, time_col = 1, op_temp_col = 3)

# read OTM metadata
otm_metadata <- read.csv("data/otm_metadata.csv")

# add OTM metadata
otm_data <- add_otm_metadata(otm_data = otm_data, otm_metadata = otm_metadata)

# generate OTM splines
otm_splines <- gen_otm_splines(otm_data = otm_data, knot_p = 0.0167)

# save otm splines file
save(otm_splines, file = "data/otm_splines.RData")

## Reading and processing flights data & generating validation dataset --------

# read flights metadata
flights_metadata <- read.csv("data/flights_metadata.csv")

# get the flight files list
folder <- "C:/Users/ggarc/OneDrive/research/throne_data"
flight_files_list <- paste(folder, "/", list.files(folder), sep = "")

# generate validation data holder data frame
corr_data <- data.frame(latitude = c(), longitude = c(), minute = c(),
                        op_temp = c(), ref_temp = c(), gcps = c(),
                        year = c(), date = c())

# loop to generate validation data
for(i in 1:length(flight_files_list)){

  # read and process flight data
  flight <- rnp_flight_data(file_path = flight_files_list[i], digits = 5)

  # add flight metadata
  flight <- add_flight_metadata(flight_data = flight,
                                flight_id = str_sub(list.files(folder)[i], end = -5),
                                flight_metadata = flights_metadata)

  # generate corr data for given flight
  corr_data_flight <- gen_corr_data(flight = flight, splines = otm_splines)

  # add column to indicate GCPs present
  gcps <- flights_metadata$gcps[flights_metadata$flight_id == str_sub(list.files(folder)[i], end = -5)]
  corr_data_flight$gcps <- rep(gcps, nrow(corr_data_flight))

  # add columns for year and date
  corr_data_flight$year <- mean(flight$year, na.rm = T)
  corr_data_flight$date <- mean(flight$date, na.rm = T)

  # bind to holder dataset
  corr_data <- rbind(corr_data, corr_data_flight)

  print(paste("You have ", 100*round(1 - i/68, digits = 2),"% left", sep = ""))

}

## Add OTM metadata information and Save file ---------------------------------

save(corr_data, file = "data/corr_data.RData")

## Plot correlation results ---------------------------------------------------

# linear model to quantify error ----
summary(lm(ref_temp ~ op_temp, data = corr_data %>% filter(gcps == "Y")))

# ref temp ~ op temp + time of day (if no gcps) ----
plot1 <- corr_data %>%
  filter(gcps == "N") %>%
  ggplot(aes(x = op_temp, y = ref_temp, col = minute/60)) +
  #geom_smooth(aes(group = as.factor(minute/60)), method = "lm", alpha = 0, lwd = 0.25) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = 1.25) +
  geom_smooth(method = "lm", col = "black") +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue", midpoint = 12) +
  xlab("Operative Temperature (OTM)") +
  ylab("Reflected Temperature (Drone)") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        legend.position = c(0.1, 0.75),
        strip.text = element_text(size = 15)) +
  labs(colour = "Hour",
       title = "A) Original correlation",
       subtitle = expression(paste("Reflected Temp. = 11.54 + 0.63 x Operative Temp.,  ", R^2 == 0.42)))

# ref temp ~ op temp + time of day (if no gcps) ----
plot2 <- corr_data %>%
  filter(gcps == "Y") %>%
  ggplot(aes(x = op_temp, y = ref_temp, col = minute/60)) +
  #geom_smooth(aes(group = as.factor(minute/60)), method = "lm", alpha = 0, lwd = 0.25) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = 1.25) +
  geom_smooth(method = "lm", col = "black") +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue", midpoint = 12) +
  xlab("Operative Temperature (OTM)") +
  ylab("Reflected Temperature (Drone)") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        legend.position = "none",
        strip.text = element_text(size = 15)) +
  labs(colour = "Hour",
       title = "B) Corrected by GCPs",
       subtitle = expression(paste("Reflected Temp. = 6.02 + 0.75 x Operative Temp.,  ", R^2 == 0.64)))

# ref temp ~ op temp animated by time of day (gcps only)----
library(gganimate)
library(gifski)
library(magick)

x <- corr_data %>%
  filter(gcps == "Y") %>%
  ggplot(aes(x = op_temp, y = ref_temp, fill = round(minute/60, digits = 1))) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = 1.25) +
  geom_point(size = 5, shape = 21) +
  scale_fill_gradient2(low = "black", mid = "orange", high = "darkblue", midpoint = 12) +
  xlab("Operative Temperature (OTM)") +
  ylab("Reflected Temperature (Drone)") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        legend.position = "none",
        strip.text = element_text(size = 15)) +
  labs(colour = "Hour", title = "Hour: {frame_time}") +
  transition_time(round(minute/60, digits = 1))
anim <- animate(x, height = 5.5, width = 5.5, units = "in", fps = 2, res = 200, renderer = gifski_renderer())
anim_save("test.gif", anim)

# bias by time of day (gcps)----
corr_data %>%
  filter(gcps == "Y") %>%
  ggplot(aes(x = round(minute/60, digits = 1), y = op_temp - ref_temp, col = round(minute/60, digits = 1))) +
  geom_abline(intercept = 1, slope = 0, linetype = 2, linewidth = 1.25) +
  geom_jitter(width = 0.025, alpha = 0.25) +
  stat_summary(aes(fill = round(minute/60, digits = 1)), shape = 21, size = 1, linewdith = 2, col = "black", show_guide = FALSE) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue", midpoint = 12) +
  scale_fill_gradient2(low = "black", mid = "orange", high = "darkblue", midpoint = 12) +
  scale_x_continuous(breaks = seq(9,19, by = 1)) +
  geom_smooth(col = "black") +
  ylab("Bias (Operative - Reflected Temperature)") +
  xlab("Hour") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        legend.position =  "none",
        strip.text = element_text(size = 15)) +
  labs(colour = "Hour")

# get correction factors by time of day----
correction_factors <- corr_data %>%
  filter(gcps == "Y") %>%
  group_by(minute) %>%
  summarise(corr_factor = mean(op_temp - ref_temp))

# merge correction factors with data----
corr_data_gcps <- corr_data %>% filter(gcps == "Y")
corr_data_test <- merge(corr_data_gcps, correction_factors, by = "minute", all = TRUE)

# bias by time of day after applying correction----
corr_data_test %>%
  mutate(ref_temp_corr = ref_temp + corr_factor) %>%
  ggplot(aes(x = round(minute/60, digits = 1), y = op_temp - ref_temp_corr, col = round(minute/60, digits = 1))) +
  geom_abline(intercept = 0, slope = 0, linetype = 2, linewidth = 1.25) +
  geom_jitter(width = 0.025, alpha = 0.25) +
  stat_summary(aes(fill = round(minute/60, digits = 1)), shape = 21, size = 1, linewdith = 1, col = "black", show_guide = FALSE) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue", midpoint = 12) +
  scale_fill_gradient2(low = "black", mid = "orange", high = "darkblue", midpoint = 12) +
  scale_x_continuous(breaks = seq(9,19, by = 1)) +
  geom_smooth(col = "black") +
  ylab("Bias (Operative - Reflected Temperature)") +
  xlab("Hour") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        legend.position = "none",
        strip.text = element_text(size = 15)) +
  labs(colour = "Hour")

# ref temp ~ op temp (gcp only and corrected by time of day)----
plot3 <- corr_data_test %>%
  mutate(ref_temp_corr = ref_temp + corr_factor) %>%
  ggplot(aes(x = op_temp, y = ref_temp_corr, col = minute/60)) +
  #geom_smooth(aes(group = as.factor(minute/60)), method = "lm", alpha = 0, lwd = 0.25) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = 1.25) +
  geom_smooth(method = "lm", col = "black") +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue", midpoint = 12) +
  xlab("Operative Temperature (OTM)") +
  ylab("Reflected Temperature (Drone)") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        legend.position = "none",
        strip.text = element_text(size = 15)) +
  labs(colour = "Hour",
       title = "C) Corrected by GCPs & Hour of Day",
       subtitle = expression(paste("Reflected Temp. = 11.84 + 0.69 x Operative Temp.,  ", R^2 == 0.7)))

# add columns for corrected by time of day & run model----
corr_data_test$ref_temp_corr1 <- corr_data_test$ref_temp + corr_data_test$corr_factor
model <- lm(corr_data_test$ref_temp_corr1 ~ corr_data_test$op_temp)
summary(model)

# correction by temperature value----
corr_data_test$ref_temp_corr2 <- (2 - 0.68658) * corr_data_test$ref_temp_corr1 - 11.84
summary(lm(corr_data_test$ref_temp_corr2 ~ corr_data_test$op_temp))

# ref temp ~ op temp (gcp only corrected by time of day, temp)----
plot4 <- corr_data_test %>%
  ggplot(aes(x = op_temp, y = ref_temp_corr2, col = minute/60)) +
  #geom_smooth(aes(group = as.factor(minute/60)), method = "lm", alpha = 0, lwd = 0.25) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = 1.25) +
  geom_smooth(method = "lm", col = "black") +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue", midpoint = 12) +
  xlab("Operative Temperature (OTM)") +
  ylab("Reflected Temperature (Drone)") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        legend.position = "none",
        strip.text = element_text(size = 15)) +
  labs(colour = "Hour",
       title = "D) Corrected by GCPs, Hour of Day & Temp.",
       subtitle = expression(paste("Reflected Temp. = 3.71 + 0.91 x Operative Temp.,  ", R^2 == 0.7)))

# generate grid----
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)



get_correction_factors(corr_data)














