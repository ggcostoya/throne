
## Validation analysis ##

## Load `throne` package ------------------------------------------------------

devtools::load_all()

## Load and prepare data ------------------------------------------------------

# get list of flights
flights_list <- flights_data_corr %>%
  dplyr::select(year, doy, mod_start, mod_end) %>% unique()

# generate OTM splines with different knot_p
a <- gen_otm_splines(otm_data = otms_data, knot_p = 1/2) %>% mutate(knot_p = 1/2)
b <- gen_otm_splines(otm_data = otms_data, knot_p = 1/7.5) %>% mutate(knot_p = 1/7.5)
c <- gen_otm_splines(otm_data = otms_data, knot_p = 1/15) %>% mutate(knot_p = 1/15)
d <- gen_otm_splines(otm_data = otms_data, knot_p = 1/60) %>% mutate(knot_p = 1/60)
m_otm_splines <- bind_rows(a, b, c, d)

# round latitude and longitude columns to 5 digits
m_otm_splines$longitude <- as.numeric(format(m_otm_splines$longitude, nsmall = 5))
m_otm_splines$latitude <- as.numeric(format(m_otm_splines$latitude, nsmall = 5))

# add tile id column to metadata
m_otm_splines$tile_id <- m_otm_splines$longitude * m_otm_splines$latitude

# filter tile on flight data where OTMs are present
tile_flights_data_corr <- flights_data_corr %>%
  mutate(tile_id = latitude * longitude) %>%
  filter(tile_id %in% unique(m_otm_splines$tile_id))

## Prepare parameters to test -------------------------------------------------

# number of flights to use
n_flights <- c(3,6,9)

# number of OTMs to use
n_otms <- c(8,16,24,33)

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

## Matching -------------------------------------------------------------------

# generate holder object
all_matches <- tibble(latitude = c(), longitude = c(), tile_id = c(),
                      otm_id = c(), error = c(), n_flights = c(), n_otms = c(), knot_p = c())

# running matching loop
for(i in 1:nrow(combs)){

  # load package
  #devtools::load_all()

  # select subset of flights of interest
  if(combs$n_flights[i] < 34){

    fmorn <- flights_list %>% filter(mod_start < 660) %>% sample_n(combs$n_flights[i]/3)
    fmid <- flights_list %>% filter(mod_start < 660) %>% filter(mod_start < 1000) %>% sample_n(combs$n_flights[i]/3)
    faft <- flights_list %>% filter(mod_start > 1000)%>% sample_n(combs$n_flights[i]/3)
    flights_int <- rbind(fmorn, fmid, faft)
    sub_flights <- tile_flights_data_corr %>% filter(mod_start %in% flights_int$mod_start)

  }else{

    flights_int <- flights_list
    sub_flights <- tile_flights_data_corr

  }

  # select subset of OTMs
  sub_otm <- sample(unique(m_otm_splines$otm_id), combs$n_otms[i])

  # select subset of OTMs of interest, doy < 239 as no flights took place then.
  sub_otm_splines <- m_otm_splines %>% filter(knot_p == combs$knot_p[i]) %>%
    filter(otm_id %in% sub_otm) %>% filter(doy < 239)

  # subset flights times
  all_otms_times <- expand.grid(otm_id = sub_otm, year = unique(flights_int$year),doy = unique(flights_int$doy),
                                mod_start = unique(flights_int$mod_start), mod_end = unique(flights_int$mod_end))

  # merge all times combinations with all combinations to get each OTM at all times
  sub_otm_preds <- merge(all_otms_times, flights_int, by = c("year", "doy", "mod_start", "mod_end"))

  # add column for predicted operative temperatured
  sub_otm_preds$pred_op_temp <- rep(NA, nrow(sub_otm_preds))

  # add columns for latitude and longitude
  sub_otm_preds$latitude <- rep(NA, nrow(sub_otm_preds))
  sub_otm_preds$longitude <- rep(NA, nrow(sub_otm_preds))

  # loop to predict operative temperatures during that period
  for(j in 1:nrow(sub_otm_preds)){

    # isolate the spline model for that otm_id, that year, that doy
    otm_specific_spline <- sub_otm_splines %>% filter(otm_id == sub_otm_preds$otm_id[j]) %>%
      filter(year == sub_otm_preds$year[j]) %>% filter(doy == sub_otm_preds$doy[j])

    # predict temperatures for the period between mod_start and mod_end
    prediction <- predict(otm_specific_spline$spline[[1]],c(sub_otm_preds$mod_start[j]:sub_otm_preds$mod_end[j]))$y

    # get average predicted temperature
    sub_otm_preds$pred_op_temp[j] <- mean(prediction)

    # add info on latitude and longitude
    sub_otm_preds$latitude[j] <- otm_specific_spline$latitude[1]
    sub_otm_preds$longitude[j] <- otm_specific_spline$longitude[1]

  }

  # build holder dataset
  sub_matches <- sub_flights %>%
    dplyr::select(latitude, longitude, tile_id) %>% unique() %>%
    mutate(otm_id = NA, error = NA) %>% as_tibble()

  # loop to estimate best OTM match for each tile
  for(j in 1:nrow(sub_matches)){

    # filter data for specific tile
    tile_dat <- sub_flights %>% filter(tile_id == sub_matches$tile_id[j])

    # select columns of interest from tile_dat
    tile_dat <- tile_dat %>% dplyr::select(tile_id, year, doy, mod_start, mod_end, op_temp)

    # merge tile data with otm_preds
    tile_dat <- merge(tile_dat, sub_otm_preds, by = c("year", "doy", "mod_start", "mod_end"), all = TRUE)

    # find OTM match
    otm_match <- tile_dat %>% group_by(otm_id) %>%
      summarise(error = mean(abs(pred_op_temp - op_temp), na.rm = T)) %>%
      filter(error == min(error, na.rm = T))

    # add info to holder matches dataset
    sub_matches$otm_id[j] <- as.character(otm_match$otm_id[1])
    sub_matches$error[j] <- otm_match$error[1]

  }

  # add information columns
  sub_matches <- sub_matches %>% mutate(n_flights = combs$n_flights[i],
                                        n_otms = combs$n_otms[i],
                                        knot_p = combs$knot_p[i],
                                        it = combs$it[i])

  # bind to all matches
  all_matches <- rbind(all_matches, sub_matches)
  print(i)

}

# get OTMs list to merge
merge_otms <- m_otm_splines %>%
  dplyr::select(otm_id, latitude, longitude) %>% unique() %>%
  rename(a_otm_id = otm_id)

# merge all_matches with OTMs to create validation data
val_data <- merge(all_matches, merge_otms, by = c("latitude", "longitude"))

## Validation -----------------------------------------------------------------

# repeat each match 100 times
val_data <- do.call("rbind", replicate(100, val_data, simplify = FALSE))

# add date column
val_data$doy <- sample(unique(otms_splines$doy)[2:3], nrow(val_data), replace = TRUE)

# add minute column
val_data$mod <- sample(seq(0,1439, by = 1), nrow(val_data), replace = TRUE)

# expand grid of otm, doy and mod combinations
otm_time_combs <- expand.grid(unique(val_data$a_otm_id), unique(val_data$doy),
                              unique(val_data$mod), unique(val_data$knot_p))
colnames(otm_time_combs) <- c("otm_id", "doy", "mod", "knot_p")
otm_time_combs$op_temp <- rep(NA, nrow(otm_time_combs))

# loop to perform pre-calculations
for(i in 1:nrow(otm_time_combs)){

  # filter spline for the actual temperature
  spline <- m_otm_splines %>%
    dplyr::filter(otm_id == otm_time_combs$otm_id[i],
                  knot_p == otm_time_combs$knot_p[i],
                  doy == otm_time_combs$doy[i])

  # predict temperature
  otm_time_combs$op_temp[i] <- predict(spline$spline[[1]], otm_time_combs$mod[i])$y

  print(i)

}

# merge first for actual otms
a_otm_time_combs <- otm_time_combs %>% rename(a_otm_id = otm_id, obs_op_temp = op_temp)
val_data <- merge(val_data, a_otm_time_combs, by = c("a_otm_id", "doy", "mod", "knot_p"))

# merge for predicted otms
p_otm_time_combs <- otm_time_combs %>% rename(pred_op_temp = op_temp)
val_data <- merge(val_data, p_otm_time_combs, by = c("otm_id", "doy", "mod", "knot_p"))

## Save data ------------------------------------------------------------------

save(val_data, file = "data/val_data.RData")

## Validation summaries ------------------------------------------------------

x <- val_data %>%
  filter(mod > 7*60) %>% filter(mod < 19*60) %>%
  #mutate(minute = round(mod/96)) %>%
  #group_by(n_flights, n_otms, knot_p, minute) %>%
  #summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  group_by(n_flights, n_otms, knot_p) %>%
  summarise(mean_error = mean(pred_op_temp - obs_op_temp),
            sd_error = sd(pred_op_temp - obs_op_temp),
            abs_mean_error = mean(abs(pred_op_temp - obs_op_temp)),
            abs_sd_error = sd(abs(pred_op_temp - obs_op_temp)))




