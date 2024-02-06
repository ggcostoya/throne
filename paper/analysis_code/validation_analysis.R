
## Validation analysis ##

## Load `throne` package ------------------------------------------------------

devtools::load_all()

## Load and prepare data for validations --------------------------------------

# load all flights data
load("data/flights_data.RData")
flights_data <- flights_data %>% rename(op_temp = ref_temp)

# add tile id column to flights_data
flights_data$longitude <- as.numeric(format(flights_data$longitude, nsmall = 5))
flights_data$latitude <- as.numeric(format(flights_data$latitude, nsmall = 5))
flights_data$tile_id <- flights_data$longitude * flights_data$latitude

# get a flights list and divide them between morning, mid-day and afternoon
flights_list <- flights_data %>% dplyr::select(date, minute_start) %>% unique()
flights_morning <- flights_list %>% filter(minute_start < 660)
flights_mid <- flights_list %>% filter(minute_start > 660) %>% filter(minute_start < 1000)
flights_afternoon <- flights_list %>% filter(minute_start > 1000)

# load OTM data
otm_data <- rnp_otm_data(folder_path = "data/otm_data", rows_skip = 14, time_col = 1, op_temp_col = 3)

# read OTM metadata
otm_metadata <- read.csv("data/otm_metadata.csv")

# subset for OTMs that recorded data
otm_metadata <- otm_metadata %>% filter(otm_id %in% unique(otm_data$otm_id))

# round latitude and longitude columns to 5 digits
otm_metadata$longitude <- as.numeric(format(otm_metadata$longitude, nsmall = 5))
otm_metadata$latitude <- as.numeric(format(otm_metadata$latitude, nsmall = 5))

# add OTM metadata
otm_data <- add_otm_metadata(otm_data = otm_data, otm_metadata = otm_metadata)

# generate otm splines with different knot_p combinations
otm_splines_1.2 <- gen_otm_splines(otm_data = otm_data, knot_p = 1/2) %>% mutate(knot_p = 1/2)
otm_splines_1.7.5 <- gen_otm_splines(otm_data = otm_data, knot_p = 1/7.5) %>% mutate(knot_p = 1/7.5)
otm_splines_1.15 <- gen_otm_splines(otm_data = otm_data, knot_p = 1/15) %>% mutate(knot_p = 1/15)
otm_splines_1.60 <- gen_otm_splines(otm_data = otm_data, knot_p = 1/60) %>% mutate(knot_p = 1/60)
otm_splines <- bind_rows(otm_splines_1.2, otm_splines_1.7.5, otm_splines_1.15, otm_splines_1.60)

# otms list
otms_list <- unique(otm_data$otm_id)

# add tile id column to metadata
otm_metadata$tile_id <- otm_metadata$longitude * otm_metadata$latitude

# filter pixels on flight data where OTMs are present
flights_data <- flights_data %>% filter(tile_id %in% otm_metadata$tile_id)

# merge flights_data with OTM metadata
actual_otms <- otm_metadata %>%
  dplyr::select(tile_id, otm_id, microhabitat, orientation) %>%
  rename(act_otm_id = otm_id, act_microhabitat = microhabitat, act_orientation = orientation)

## Prepare parameters to test -------------------------------------------------

# number of flights to be used
n_flights <- c(3, 6, 9)

# number of otms to be used
n_otms <- c(8, 16, 24, 33)

# knots / h to be considered
knot_ps <- c(1/60, 1/15, 1/7.5, 1/2)

# get combinations grid
combs <- expand.grid(n_flights, n_otms, knot_ps)
colnames(combs) <- c("n_flights", "n_otms", "knot_p")

# get replicates
combs <- do.call("rbind", replicate(5, combs, simplify = FALSE))

# add all flights all otms combinations which only vary in knots / h
# no replicates are needed as all flights and all OTMs are being sampled
all_flights <- data.frame(n_flights = rep(34, 4), n_otms = rep(33, 4), knot_p = knot_ps)

# bind combinations to all flights iterations
combs <- rbind(combs, all_flights)

# add iterations column
combs$it <- seq(1, nrow(combs), by = 1)

## Matches --------------------------------------------------------------------

# loading parallel R package
library(doParallel)

# register parallel cores
registerDoParallel(detectCores())

# running match loop
all_matches <- foreach(i = 1:nrow(combs), .combine = rbind) %dopar% {

  # load package
  devtools::load_all()

  # select flights of interest
  if(combs$n_flights[i] < 34){

    fmorn <- flights_morning %>% sample_n(combs$n_flights[i]/3)
    fmid <- flights_mid %>% sample_n(combs$n_flights[i]/3)
    faft <- flights_afternoon %>% sample_n(combs$n_flights[i]/3)
    flights_int <- rbind(fmorn, fmid, faft)
    flights <- flights_data %>% filter(minute_start %in% flights_int$minute_start)

  }else{

    flights <- flights_data

  }

  # select OTMs of interest
  otms_int <- sample(otms_list, combs$n_otms[i])

  # filter OTM splines to be used
  otms <- otm_splines %>% filter(otm_id %in% otms_int) %>% filter(knot_p == combs$knot_p[i])

  # match
  matched <- match_data(flights_data = flights, coverage = combs$n_flights[i], otm_splines = otms)

  # add information columns
  matched <- matched %>% mutate(n_flights = rep(combs$n_flights[i], nrow(.)),
                                n_otms = rep(combs$n_otms[i], nrow(.)),
                                knot_p = rep(combs$knot_p[i], nrow(.)),
                                it = rep(combs$it[i], nrow(.)))

  matched
}

# rename the tile id column
all_matches <- all_matches %>% rename(tile_id = pix_id)

# merge matches with otm_metadata
all_matches <- merge(all_matches, otm_metadata, by = "otm_id")

# select columns of interest
all_matches <- all_matches %>% dplyr::select(!c(latitude, longitude, elevation, tile_id.y)) %>%
  rename(tile_id = tile_id.x)

# generate validation data
val_data <- merge(all_matches, actual_otms, by = "tile_id")

## Validation -----------------------------------------------------------------

# repeat each match 100 times
val_data <- do.call("rbind", replicate(100, val_data, simplify = FALSE))

# add date column
val_data$date <- sample(unique(otm_splines$date)[2:3], nrow(val_data), replace = TRUE)

# add minute column
val_data$minute <- sample(seq(0,1439, by = 1), nrow(val_data), replace = TRUE)

# add columns for predicted temp vs observed operative temperature
val_data <- val_data %>% mutate(obs_op_temp = rep(NA, nrow(.)),
                                pred_op_temp = rep(NA, nrow(.)))

## Loop to perform validation
for(i in 1:nrow(val_data)){

  # filter spline for the actual temperature
  spline_actual <- otm_splines %>%
    dplyr::filter(otm_id == val_data$act_otm_id[i],
                  knot_p == val_data$knot_p[i],
                  date == val_data$date[i])

  # predict temperature
  val_data$obs_op_temp[i] <- predict(spline_actual$splines[[1]], val_data$minute[i])$y

  # filter spline for the actual temperature
  spline_pred <- otm_splines %>%
    dplyr::filter(otm_id == val_data$otm_id[i],
                  knot_p == val_data$knot_p[i],
                  date == val_data$date[i])

  # predict temperature
  val_data$pred_op_temp[i] <- predict(spline_pred$splines[[1]], val_data$minute[i])$y

}

all_val_data <- val_data
save(all_val_data, file = "data/all_val_data.RData")

## Plotting -----


library(ggridges)
val_data %>%
  #mutate(knot_p = 1/7.5) %>%
  filter(knot_p == 1/7.5) %>%
  #filter(minute > 420) %>% filter(minute < 1140) %>%
  #mutate(minute = round(minute/96)) %>%
  #group_by(n_flights, n_otms, knot_p, minute) %>%
  #summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  ggplot(aes(y = as.factor(knot_p), x = pred_op_temp - obs_op_temp)) +
  geom_density_ridges(aes(fill = as.factor(knot_p)),
                      quantile_lines = TRUE, quantiles = c(0.025, 0.975),
                      alpha = 0.5, scale = 1.25) +
  stat_summary(aes(fill = as.factor(knot_p)),shape = 21, size = 0.5) +
  scale_x_continuous(#limits = c(0,6), expand = c(0,0), breaks = seq(0,5, by = 1),
    sec.axis = sec_axis(~.,name = "Flights used", breaks = NULL)) +
  scale_y_discrete(expand = c(0.05,0.05)) +
  #scale_fill_manual(values = c("skyblue", "royalblue", "darkblue")) +
  ylab("Knots / Hour") +
  xlab ("Absolute Error (|Observed - Predicted Operative Temp.|") +
  facet_grid(cols = vars(n_flights), rows = vars(n_otms)) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        axis.ticks = element_line(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25),
        strip.text = element_text(size = 9),
        strip.background = element_rect(fill = "lightgray"),
        axis.title = element_text(size = 11, face = "bold"))

val_data %>%
  filter(knot_p == 1/15) %>%
  filter(minute > 420) %>% filter(minute < 1140) %>%
  mutate(minute = round(minute/96)) %>%
  group_by(n_flights, n_otms, knot_p, minute) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  ggplot(aes(y = as.factor(n_otms), x = pred_op_temp - obs_op_temp)) +
  geom_density_ridges(aes(fill = as.factor(knot_p)),alpha = 0.5, scale = 1.25) +
  geom_vline(xintercept = 0) +
  stat_summary(aes(fill = as.factor(n_otms)), fun.data = mean_sdl, shape = 21, size = 0.5) +
  facet_grid(cols = vars(n_flights))


val_data %>%
  filter(knot_p == 1/60) %>%
  filter(minute > 420) %>% filter(minute < 1140) %>%
  mutate(minute = round(minute/96)) %>%
  group_by(n_flights, n_otms, knot_p, minute) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  ggplot(aes(y = pred_op_temp - obs_op_temp, x = minute)) +
  geom_line(aes(group = as.factor(n_otms), col = as.factor(n_otms))) +
  facet_grid(cols = vars(n_flights))

val_data %>%
  filter(knot_p == 1/7.5) %>%
  filter(minute > 420) %>% filter(minute < 1140) %>%
  mutate(minute = round(minute/96)) %>%
  group_by(n_flights, n_otms, knot_p, minute) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  #filter(n_otms == 8) %>%
  ggplot(aes(x = pred_op_temp - obs_op_temp)) +
  geom_density(aes(col = as.factor(n_otms))) +
  facet_grid(cols = vars(n_flights))

val_data %>%
  mutate(matched_correctly = ifelse(otm_id == act_otm_id, 1, 0)) %>%
  group_by(n_otms, n_flights, knot_p) %>%
  summarise(avg_match_correct = mean(matched_correctly)) %>%
  ggplot(aes(x = as.factor(n_otms), y = avg_match_correct, col = as.factor(knot_p))) +
  geom_path(aes(group = as.factor(knot_p))) +
  facet_grid(cols = vars(n_flights))

