## Validation ##


## Packages -------------------------------------------------------------------

devtools::load_all()

## Loading and prep training data ---------------------------------------------

#load OTM splines
load("data/otm_splines.RData")

# load matches data
load("data/matches.RData")

# format decimal digits from both datasets
otm_splines$longitude <- as.numeric(format(otm_splines$longitude, nsmall = 5))
otm_splines$latitude <- as.numeric(format(otm_splines$latitude, nsmall = 5))
otm_splines$pix_id <- otm_splines$longitude * otm_splines$latitude

# get OTM pix id
otm_pixs <- otm_splines %>% dplyr::select(otm_id, pix_id) %>% unique() %>%
  mutate(closest_pix_id = rep(NA, nrow(.)))

# filter
matches$longitude <- as.numeric(format(matches$longitude, nsmall = 5))
matches$latitude <- as.numeric(format(matches$latitude, nsmall = 5))
matches$pix_id <- matches$longitude * matches$latitude

# find closest pixel to each otm
for(i in 1:nrow(otm_pixs)){

  otm_pixs$closest_pix_id[i] <- matches$pix_id[which.min(abs(otm_pixs$pix_id[i] - matches$pix_id))]

}

# reformat
otm_pixs <- otm_pixs %>% dplyr::select(otm_id, closest_pix_id) %>%
  rename(actual_otm_id = otm_id, pix_id = closest_pix_id)

# filter pixels in matches that are the closest to otms
matches_test <- matches %>% filter(pix_id %in% otm_pixs$pix_id)

# merge to create validation data
val_data <- merge(matches_test, otm_pixs)

# select rows of interest from validation data
val_data <- val_data %>% dplyr::select(otm_id, actual_otm_id)

## Validation -----------------------------------------------------------------

# repeat val data for test
val_data <- do.call("rbind", replicate(100, val_data, simplify = FALSE))

# add date column
val_data$date <- sample(unique(otm_splines$date)[2:3], nrow(val_data), replace = TRUE)

# add minute column
val_data$minute <- sample(seq(0,1439, by = 1), nrow(val_data), replace = TRUE)
val_data <- unique(val_data)

# add columns for predicted temp vs observed temperature
val_data <- val_data %>% mutate(obs_op_temp = rep(NA, nrow(.)),
                                pred_op_temp = rep(NA, nrow(.)))

## Loop to perform validation
for(i in 1:nrow(val_data)){

  # filter spline for the actual temperature
  spline_actual <- otm_splines %>%
    dplyr::filter(otm_id == val_data$actual_otm_id[i], date == val_data$date[i])

  # predict temperature
  val_data$obs_op_temp[i] <- predict(spline_actual$splines[[1]], val_data$minute[i])$y

  # filter spline for the actual temperature
  spline_pred <- otm_splines %>%
    dplyr::filter(otm_id == val_data$otm_id[i], date == val_data$date[i])

  # predict temperature
  val_data$pred_op_temp[i] <- predict(spline_pred$splines[[1]], val_data$minute[i])$y

}

## Plotting -------------------------------------------------------------------

val_data %>%
  ggplot(aes(x = pred_op_temp, y = obs_op_temp)) +
  geom_point(alpha = 0.5, col = "lightgray", size = 4) +
  geom_abline(intercept = c(10,0, -10), slope = c(1,1,1),
              linetype = c(2,1,2), linewidth = 1.25) +
  geom_smooth(method = "lm") +
  xlab("Predicted operative temperature") +
  ylab("Observed operative temperature") +
  theme_classic()

val_data %>%
  ggplot(aes(x = minute/60, y = obs_op_temp - pred_op_temp)) +
  geom_point(aes(col = minute/60),  alpha = 0.5, size = 3) +
  geom_abline(intercept = c(10,0, -10), slope = c(0,0,0),
              linetype = c(2,1,2), linewidth = 1.25) +
  geom_smooth(col = "black") +
  scale_color_gradient2(low = "darkblue", mid = "orange", high = "darkblue", midpoint = 12) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0:24)) +
  xlab("Hour") +
  ylab("Prediction error (Observed - Predicted)") +
  theme_classic() +
  theme(legend.position = "none")

summary(lm(obs_op_temp ~ pred_op_temp, data = val_data))

val_data %>%
  ggplot(aes(y = ))

val_data %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_tile(aes(fill = otm_id)) +
  geom_point(aes(fill = actual_otm_id), shape = 21, size = 2) +
  theme_minimal() +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.title = element_blank())

val_data %>%
  ggplot(aes(x = 1, y = obs_op_temp - pred_op_temp)) +
  geom_jitter(width = 0.1)
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.2,
               position = position_jitter(height = 0.1))

