
## Testing case example ##

devtools::load_all()

## Read and process flights data ----

# read flights metadata
c_flights_metadata <- read.csv("data/case_flights_metadata.csv")

# read and process flights data
c_flights_data <- rnp_flights_data(path = "C:/Users/ggarc/OneDrive/research/throne_data/case_flights",
                                   metadata = c_flights_metadata,
                                   digits = 5)

## Reand and process OTM data ----

# read OTMs metadata
c_otms_metadata <- as_tibble(read.csv("data/case_otm_metadata.csv"))

# read OTMs data
c_otms_data <- rnp_otms_data(path = "data/case_otm_data",
                             rows_skip = 1, date_col = 1, op_temp_col = 2,
                             metadata = c_otms_metadata)

# filter data for month of august only
c_otms_data <- c_otms_data %>% filter(doy > 143) %>% filter(doy < 235)

## Generate OTM splines ----

# generate OTM splines
c_otms_splines <- gen_otm_splines(otm_data = c_otms_data, knot_p = 0.75)

## Correct flights data ---

# correct flights data
c_flights_data_corr <- correct_flights_data(flights_data = c_flights_data,
                                            otm_splines = c_otms_splines)
## Estimate matches ----

# estimate tile to OTM matches
c_matches <- match_data(flights_data = c_flights_data_corr,
                        otm_splines = c_otms_splines,
                        coverage_per = 0.9,
                        error_max = 5)


save(c_matches, file = "data/case_data/c_matches.RData")

## Prepare case study data -----

case_study_data$longitude <- as.numeric(format(case_study_data$longitude, nsmall = 5))
case_study_data$latitude <- as.numeric(format(case_study_data$latitude, nsmall = 5))

tiles <- c_matches$latitude*c_matches$longitude

case_study_data <- case_study_data %>% mutate(tile = latitude*longitude) %>% filter(tile %in% tiles)
case_study_data <- case_study_data %>% filter(doy > 163) %>% filter(doy < 250)


lizard_mr <- case_study_data

lizard_mr <- lizard_mr %>% filter(doy > 163) %>% rename(tb = probe_tb) %>%
  dplyr::select(!tile) %>% as_tibble()

lizard_mr


save(c_otms_data, file = "data/c_otms_data.Rdata")
save(c_otms_splines, file = "data/c_otms_splines.Rdata")
save(lizard_mr, file = "data/lizard_mr.Rdata")

## Predict thermal landscapes ----

c_matches_sub <- c_matches %>% mutate(tile = latitude * longitude) %>%
  filter(tile %in% csd$tile)
doys <- unique(csd$doy)
mods <- unique(csd$mod)

csd$op_temp <- rep(NA, nrow(csd))


for(i in 1:nrow(csd)){

  c_matches_sub <- c_matches %>% mutate(tile = latitude * longitude) %>%
    filter(tile == csd$tile[i])

  print(c_matches_sub$otm_id[1])

  if(is.na(c_matches_sub$otm_id[1])){

    csd$op_temp[i] <- NA


  }else{

    pred <- predict_thermal_landscape(matches = c_matches_sub, otm_splines = c_otms_splines,
                                      doy = csd$doy[i], mod = csd$mod[i])

    csd$op_temp[i] <- pred$pred_op_temp[1]


  }

  print(i)

}


csd %>%
  ggplot(aes(x = mod/60, y = probe_tb - op_temp)) +
  geom_point()

csd %>%
  ggplot(aes(x = mod / 60)) +
  geom_point(aes(y = op_temp)) +
  geom_point(aes(y = probe_tb), col = "red")


csd %>%
  pivot_longer(c("probe_tb", "op_temp"), names_to = "source", values_to = "temp") %>%
  mutate(source = ifelse(source == "probe_tb", "Probe Tb.", "Predicted Operative Temp.")) %>%
  ggplot(aes(x = mod/60, y = temp)) +
  geom_point(aes(col = source)) +
  geom_smooth(aes(col = source), method = "lm") +
  scale_x_continuous(breaks = seq(8,18,by = 1)) +
  ylab("Temperature") +
  xlab("Hour of the day") +
  theme(legend.position = "top",legend.title = element_blank())


full_landscape <- predict_thermal_landscape(matches = c_matches, otm_splines = c_otms_splines,
                                            doy = doys, mod = seq(8*60, 18*60, by = 60))

full_landscape_sum <- full_landscape %>%
  group_by(mod) %>%
  summarise(mot = mean(pred_op_temp, na.rm = T),
            sdot = sd(pred_op_temp, na.rm = T),
            maxot = max(pred_op_temp, na.rm = T),
            minot = min(pred_op_temp, na.rm = t))

c_example_otm <- read.csv('data/case_otm_data/H22.csv')
ggplot() +
  geom_ribbon(data = full_landscape_sum,
              aes(x = mod/60, ymax = maxot, ymin = minot),
              fill = "lightgray", alpha = 0.5) +
  geom_ribbon(data = full_landscape_sum,
              aes(x = mod/60, ymax = mot + sdot, ymin = mot - sdot),
              fill = "gray", alpha = 0.5) +
  geom_line(data = full_landscape_sum, aes(x = mod/60, y = mot)) +
  geom_point(data = csd, aes(x = mod/60, y = probe_tb), col = "darkred", alpha = 0.5, size = 4) +
  geom_smooth(data = csd, aes(x = mod/60, y = probe_tb), col = "darkred", fill = "red", alpha = 0.1,
              method = "lm", fullrange = TRUE) +
  scale_x_continuous(breaks = seq(8,18, by = 1), expand = c(0,0)) +
  ylab("Operative Temperature (C)") +
  xlab("Hour of the day") +
  theme_minimal() +
  theme(axis.line = element_line(), axis.ticks = element_line(),
        panel.grid = element_blank()) +
  ggtitle("Body temperatures & Operative Temp. Distribution for August 2022 at HE")




## Plotting ----

test$hour <- floor(test$mod/60)

x <- test %>%
  ggplot(aes(x = longitude, y = latitude, fill = pred_op_temp)) +
  geom_raster() +
  scale_fill_viridis(option = "magma") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "Predicted operative temperature (C)",
       title = "Hour: {frame_time}") +
  transition_time(as.integer(hour)) +
  enter_fade() +
  exit_fade()
anim <- animate(x, height = 5, width = 5, units = "in", fps = 5, res = 200, renderer = gifski_renderer())
anim_save("case_dynamic_thermal_landscape.gif", anim)


test %>%
  filter(!is.na(hour)) %>%
  filter(hour > 5) %>% filter(hour < 22) %>%
  ggplot(aes(x = longitude, y = latitude, fill = pred_op_temp)) +
  geom_raster() +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~hour) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 12),
        legend.position = "top") +
  guides(fill = guide_colorbar("Predicted operative temperature (°C)"))


c_matches %>%
  ggplot(aes(x = longitude, y = latitude, fill = otm_id)) +
  geom_tile()+
  theme(legend.position = "none")

c_matches %>%
  filter(!is.na(otm_id)) %>%
  group_by(otm_id) %>%
  summarise(tiles_covered = n()/982) %>%
  arrange(desc(tiles_covered)) %>%
  mutate(cum_tiles_covered = cumsum(tiles_covered)) %>%
  ggplot(aes(x = reorder(otm_id, -tiles_covered))) +
  geom_col(aes(y = tiles_covered)) +
  geom_point(aes(y = cum_tiles_covered)) +
  theme(axis.text.x = element_text(angle = 90))

c_flights_data_corr %>%
  ggplot(aes(x = longitude, y = latitude, fill = op_temp)) +
  geom_raster() +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~mod_start)


c_otms_data %>%
  filter(otm_id == "H1") %>%
  ggplot(aes(x = mod, y = op_temp)) +
  geom_point() +
  facet_wrap(~doy)

ggplot() +
  geom_raster(data = c_flights_data, aes(x = longitude, y = latitude, fill = ir_temp)) +
  geom_point(data = c_otms_splines, aes(x = longitude, y = latitude), col = "black")
