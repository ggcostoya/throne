
## Full validation workflow ##

## Load package ---------------------------------------------------------------

devtools::load_all()

## Load and prepare datasets of interest --------------------------------------

# load all flights data
load("data/flights_data.RData")
flights_data <- flights_data %>% rename(op_temp = ref_temp)

# load OTM splines
#load("data/otm_splines.RData")
# read and process OTM data
otm_data <- rnp_otm_data(folder_path = "data/otm_data", rows_skip = 14, time_col = 1, op_temp_col = 3)

# read OTM metadata
otm_metadata <- read.csv("data/otm_metadata.csv")

# add OTM metadata
otm_data <- add_otm_metadata(otm_data = otm_data, otm_metadata = otm_metadata)

# generate OTM splines
otm_splines <- gen_otm_splines(otm_data = otm_data, knot_p = 1/3)

# format decimal digits from both datasets
otm_splines$longitude <- as.numeric(format(otm_splines$longitude, nsmall = 5))
otm_splines$latitude <- as.numeric(format(otm_splines$latitude, nsmall = 5))
otm_splines$pix_id <- otm_splines$longitude * otm_splines$latitude

# generate OTM pixels data
otm_pixs <- otm_splines %>% dplyr::select(otm_id, pix_id) %>% unique() %>%
  mutate(closest_pix_id = rep(NA, nrow(.))) %>% rename(actual_otm_id = otm_id)

## Prepare parameters to test -------------------------------------------------

# determine number of flights to use
n_flights <- round(c(0.3, 0.1) * length(unique(flights_data$minute_start))) - c(1,0)

# determine number of otms to use
n_otms <- round(c(1, 0.75, 0.5) * length(unique(otm_splines$otm_id)))

# get combinations
combs <- expand.grid(n_flights, n_otms)
colnames(combs) <- c("n_flights", "n_otms")

# get replicates
combs <- do.call("rbind", replicate(5, combs, simplify = FALSE))

# add iteration
combs$it <- seq(1:nrow(combs))

# flights list
flights_list <- flights_data %>% dplyr::select(date, minute_start) %>% unique()
flights_morning <- flights_list %>% filter(minute_start < 660)
flights_mid <- flights_list %>% filter(minute_start > 660) %>% filter(minute_start < 1000)
flights_afternoon <- flights_list %>% filter(minute_start > 1000)

# otms list
otms_list <- unique(otm_splines$otm_id)

## Start of the loop ---------------------------------------------------------

# loading parallel R package
library(doParallel)

# register parallel cores
registerDoParallel(detectCores())

# Progress combine function
f <- function(iterator){
  pb <- txtProgressBar(min = 1, max = iterator - 1, style = 3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb, count)
    flush.console()
    rbind(...) # this can feed into .combine option of foreach
  }
}

# running match loop
all_matches <- foreach(i = 1:nrow(combs), .packages = "tidyverse", .combine = f(nrow(combs))) %dopar% {

  # load package
  devtools::load_all()

  # select flights of interest
  fmorn <- flights_morning %>% sample_n(combs$n_flights[i]/3)
  fmid <- flights_mid %>% sample_n(combs$n_flights[i]/3)
  faft <- flights_afternoon %>% sample_n(combs$n_flights[i]/3)
  flights_int <- rbind(fmorn, fmid, faft)
  flights <- flights_data %>% filter(minute_start %in% flights_int$minute_start)

  # select OTMs of interest
  otms_int <- sample(otms_list, combs$n_otms[i])
  otms <- otm_splines %>% filter(otm_id %in% otms_int)

  # match
  matched <- match_data(flights_data = flights, coverage = combs$n_flights[i], otm_splines = otms)

  # add information columns
  matched <- matched %>% mutate(n_flights = rep(combs$n_flights[i], nrow(.)),
                                n_otms = rep(combs$n_otms[i], nrow(.)),
                                it = rep(combs$it[i], nrow(.)))

  matched
}

# Prepare data for checking --------------------------------------------------

# prepare holder dataset
matches_data <- all_matches

# add 100% - 100% option
match_all <- matches %>% dplyr::select(tile_id, otm_id) %>%
  rename(pix_id = tile_id) %>%
  mutate(n_flights = rep(34, nrow(.)), n_otms = rep(33, nrow(.)), it = rep(0, nrow(.)))
matches_data <- rbind(matches_data, match_all)

# find closest pixel id to each otm
for(i in 1:nrow(otm_pixs)){

  otm_pixs$closest_pix_id[i] <- matches_data$pix_id[which.min(abs(otm_pixs$pix_id[i] - matches_data$pix_id))]

}

# reformat
otm_pixs <- otm_pixs %>% dplyr::select(actual_otm_id, closest_pix_id) %>%
  rename(pix_id = closest_pix_id)

# filter pixels in matches that are the closest to otms
val_data <- matches_data %>% filter(pix_id %in% otm_pixs$pix_id)

# merge to create validation data
val_data <- merge(val_data, otm_pixs, by = "pix_id")

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
  ggplot(aes(x = as.factor(n_flights), y = abs(obs_op_temp - pred_op_temp))) +
  geom_violin(aes(col = as.factor(n_otms), fill = as.factor(n_otms)), alpha = 0.5,  position = position_dodge(width = 0.5)) +
  #geom_point(aes(col = as.factor(n_otms)), alpha = 0.05,  position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.1)) +
  geom_abline(intercept = c(-10, -5, 0, 5, 10), slope = rep(0, 5),
              linetype = c(3, 2, 1, 2, 3),
              col = c("lightgray", "darkgray", "black", "darkgray","lightgray")) +
  stat_summary(aes(fill =  as.factor(n_otms)),
               col = "black", shape = 21, linewidth = 2, size = 1,
               position = position_dodge(width = 0.5)) +
  xlab("Flights used") + ylab("Bias (Observed - Predicted)") +
  theme_classic() +
  theme(legend.position = "top") +
  labs(colour = "OTMs used", fill = "OTMs used")

val_data %>%
  ggplot(aes(y = as.factor(n_flights), x = abs(obs_op_temp - pred_op_temp))) +
  geom_vline(xintercept = c(-10, -5, 0, 5, 10),
              linetype = c(3, 2, 1, 2, 3),
              col = c("lightgray", "darkgray", "black", "darkgray","lightgray")) +
  stat_summary(aes(fill =  as.factor(n_otms), col = as.factor(n_otms)),
               fun = mean,
               fun.max = function(x) mean(x) + 2*sd(x),
               fun.min = function(x) mean(x) - 2*sd(x),
               geom = "pointrange",
               shape = 21, linewidth = 0.5, size = 1,
               position = position_dodge(width = 0.5)) +
  stat_summary(aes(fill =  as.factor(n_otms), col = as.factor(n_otms)),
               fun = mean,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x),
               geom = "pointrange",
               shape = 21, linewidth = 1, size = 1,
               position = position_dodge(width = 0.5)) +
  #scale_y_continuous(limits = c(0,11), expand = c(0,0)) +
  coord_cartesian(xlim = c(0,11)) +
  ylab("Flights used") + xlab("Bias (Observed - Predicted)") +
  theme_classic() +
  theme(legend.position = "top") +
  labs(colour = "OTMs used", fill = "OTMs used")

val_data %>%
  group_by(n_flights, n_otms) %>%
  summarise(bias_m = mean(abs(obs_op_temp - pred_op_temp), na.rm = T),
            bias_sd = sd(abs(obs_op_temp - pred_op_temp), na.rm = T),
            bias_95 = 2 * sd(abs(obs_op_temp - pred_op_temp), na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(n_flights), y = bias_m, col = as.factor(n_otms))) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_segment(aes(x = as.factor(n_flights), xend = as.factor(n_flights),
                   y = bias_m - bias_sd, yend = bias_m + bias_sd),
               position = position_dodge(width = 0.5))

val_data %>%
  ggplot(aes(x = minute/60, y = obs_op_temp - pred_op_temp)) +
  geom_point(aes(col = minute/60), alpha = 0.5) +
  scale_color_gradient2(low = "darkblue", mid = "orange", high = "darkblue", midpoint = 12) +
  #geom_hline(yintercept = c(-10, -5, 0, 5, 10), linetype = c(3, 2, 1, 2, 3),  colour = c("lightgray", "darkgray", "black", "darkgray","lightgray")) +
  geom_smooth(col = "black") +
  geom_hline(yintercept = c(-10,10), linetype = 3, col = "gray") +
  geom_hline(yintercept = c(-5,5), linetype = 2, col = "darkgray") +
  geom_hline(yintercept = 0, linetype = 1) +
  xlab("Hour") + ylab("Error (Observed - Predicted)") +
  facet_grid(cols = vars(n_flights), rows = vars(n_otms)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        panel.border = element_rect(fill = NA))


## Mock plot to show matching

otm <- data.frame(predict(otm_splines$splines[[3]], seq(0:1439)))
otm_2 <- data.frame(predict(otm_splines$splines[[6]], seq(0:1439)))
obs <- sample_n(otm, 10)
obs$y <- obs$y + rnorm(10, 0, 2)

ggplot()+
  geom_line(data = otm, aes(x = x/60, y = y), col = "brown", linewidth = 2) +
  geom_line(data = otm_2, aes(x = x/60, y = y), col = "brown", linewidth = 2, alpha = 0.5) +
  geom_point(data = obs, aes(x = x/60, y = y), shape = 21, fill = "skyblue", size = 5) +
  xlab("Hour") + ylab("Operative temperature") +
  theme_classic()

## Relationships between pred temp and obs temp
val_data %>%
  ggplot(aes(x = pred_op_temp, y = obs_op_temp)) +
  geom_point(alpha = 0.5, col = "gray") +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1, linetype = 1) +
  geom_abline(intercept = c(-10, 10), slope = c(1,1), linetype = 2) +
  xlab("Predicted operative temperature") +
  ylab("Observed operative temperature") +
  facet_grid(cols = vars(n_flights), rows = vars(n_otms)) +
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA),
        panel.grid = element_blank())



val_data <- val_data %>%
  group_by(it) %>%
  filter(!actual_otm_id %in% unique(otm_id)) %>%
  ungroup()

val_data %>%
  #filter(minute > 420) %>% filter(minute < 1140) %>%
  mutate(minute = round(minute/96)) %>%
  mutate(abs_error = abs(obs_op_temp - pred_op_temp)) %>%
  group_by(n_flights, n_otms, minute) %>%
  summarise(mean_abs_error = mean(abs_error),
            sd_abs_error = sd(abs_error),
            mean_raw_error = mean(obs_op_temp - pred_op_temp),
            sd_raw_error = sd(obs_op_temp - pred_op_temp)) %>%
  group_by(n_flights, n_otms) %>%
  summarise(mean_abs_error = mean(mean_abs_error),
            sd_abs_error = mean(sd_abs_error),
            mean_raw_error = mean(mean_raw_error),
            sd_raw_error = sd(sd_raw_error))

#val_data_1.6k <- val_data
#save(val_data_1.6k, file = "data/val_data_1.6k.RData")

val_data4k <- val_data



## Plotting comparisons between validations -----------------------------------

save(val_data4k, file = "data/val_data_4k.RData")
rm(list = ls())

# merging datasets

val_data <- val_data %>% mutate(knot_h = 12)
val_data4k <- val_data4k %>% mutate(knot_h = 4)
val_data_1.6k <- val_data_1.6k %>% mutate(knot_h = 1.6)
vals_data <- rbind(val_data, val_data4k, val_data_1.6k)

# raw error plot
vals_data %>%
  filter(minute > 420) %>% filter(minute < 1140) %>%
  mutate(minute = round(minute/96)) %>%
  group_by(n_flights, n_otms, knot_h, minute) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  ggplot(aes(x = as.factor(n_flights), y = pred_op_temp - obs_op_temp)) +
  geom_hline(yintercept = 0) +
  stat_summary(aes(col = as.factor(n_otms), shape = as.factor(knot_h)),
               fun.data = "mean_sdl",
               geom = "pointrange",
               position = position_dodge(width = 0.75)) +
  #scale_y_continuous(breaks = seq(-16,16,by = 2)) +
  #coord_cartesian(ylim = c(0,6), expand = FALSE) +
  xlab("Flights used") +
  ylab ("Error (Predicted - Observed Operative Temp.)") +
  theme_minimal() +
  guides(col = guide_legend("OTMs used"),
         shape = guide_legend("Spline Knots / h")) +
  ggtitle("Raw error, day-time temps, averaging every 15 minutes")

# absolute error plot
vals_data %>%
  filter(minute > 420) %>% filter(minute < 1140) %>%
  mutate(minute = round(minute/96)) %>%
  group_by(n_flights, n_otms, knot_h, minute) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  ggplot(aes(x = as.factor(n_flights), y = abs(pred_op_temp - obs_op_temp))) +
  geom_hline(yintercept = 0) +
  stat_summary(aes(col = as.factor(n_otms), shape = as.factor(knot_h)),
               fun.data = "mean_sdl",
               geom = "pointrange",
               position = position_dodge(width = 0.75)) +
  #scale_y_continuous(breaks = seq(-16,16,by = 2)) +
  coord_cartesian(ylim = c(0,6), expand = FALSE) +
  xlab("Flights used") +
  ylab ("Absolute Error (|Observed - Predicted Operative Temp.|") +
  theme_minimal() +
  guides(col = guide_legend("OTMs used"),
         shape = guide_legend("Spline Knots / h")) +
  ggtitle("Absolute error, day-time temps, averaging every 15 minutes")



x <- vals_data %>%
  #filter(minute > 420) %>% filter(minute < 1140) %>%
  #mutate(minute = round(minute/96)) %>%
  #group_by(n_flights, n_otms, knot_h, minute) %>%
  #summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  group_by(n_flights, n_otms, knot_h) %>%
  summarise(mean_raw_error = mean(pred_op_temp - obs_op_temp),
            sd_raw_error = sd(pred_op_temp - obs_op_temp),
            mean_abs_error = mean(abs(pred_op_temp - obs_op_temp)),
            sd_abs_error = sd(abs(pred_op_temp - obs_op_temp)))




