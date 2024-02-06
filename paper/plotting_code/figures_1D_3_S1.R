
## Plotting matching results ##

## Load `throne` package and other necessary packages to plot -----------------

devtools::load_all()

library(viridis)
library(gridExtra)
library(metR)

## Figure 3 -------------------------------------------------------------------

# load datasets

load("data/matches.RData")
load("data/elevation.RData")

# panel A
panel_a <- matches %>%
  group_by(otm_id) %>% summarise(times = n()) %>%
  ungroup() %>% arrange(desc(times)) %>%
  mutate(cum_times = cumsum(times)) %>%
  ggplot(aes(x = fct_reorder(otm_id, cum_times))) +
  geom_hline(yintercept = c(50,75,90,95), col = "gray", linetype = 2) +
  geom_hline(yintercept = 100, col = "gray", linetype = 1) +
  geom_line(aes(y =  100* (cum_times/5839)), group = 1) +
  geom_point(aes(y = 100 * (cum_times/5839))) +
  geom_col(aes(y = 100 * (times/5839), fill = 100 * (times/5839)), col = "black") +
  ylab("Thermal variability of the site described (%)") +
  scale_y_continuous(limits = c(0,102), expand = c(0,0), breaks = c(0,50,75,90,95,100)) +
  scale_fill_viridis() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15)) +
  ggtitle("A")

# panel B
panel_b <- ggplot() +
  geom_tile(data = matches %>%  group_by(otm_id) %>% mutate(times = n()) %>% ungroup(),
            aes(x = longitude, y = latitude, fill = 100 * (times/5839))) +
  geom_contour(data = elevation, aes(x = longitude, y = latitude, z = elevation),
               col = NA, linewidth = 0.5, bins = 20) +
  scale_fill_viridis() +
  theme_void() +
  theme(legend.position = c(0.2, 0.85),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 15)) +
  labs(fill = "Area described (%)") +
  ggtitle("B")

# panel C

# define orientation colors
orientation_colors <- c("N" = "darkblue", "NW" = "#7F468B","W" = "darkorange",
                        "SW" = "#FCCE7B", "S" = "gold", "SE" = "#90B122",
                        "E" = "forestgreen", "NE" = "#11458B", "Flat" = "maroon")
# plot
panel_c <- matches %>%
  group_by(orientation) %>% summarise(times = n()) %>%
  ungroup() %>% arrange(desc(times)) %>%
  mutate(cum_times = cumsum(times)) %>%
  ggplot(aes(x = fct_reorder(orientation, cum_times))) +
  geom_hline(yintercept = c(50,75,90,95), col = "gray", linetype = 2) +
  geom_hline(yintercept = 100, col = "gray", linetype = 1) +
  geom_line(aes(y =  100* (cum_times/5839)), group = 1) +
  geom_point(aes(y = 100 * (cum_times/5839))) +
  geom_col(aes(y = 100 * (times/5839), fill = orientation), col = "black") +
  ylab("Thermal variability of the site described (%)") +
  xlab("OTM orientation") +
  scale_y_continuous(limits = c(0,102), expand = c(0,0), breaks = c(0,50,75,90,95,100)) +
  scale_fill_manual(values = orientation_colors) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = 15)) +
  ggtitle("C")

# panel d

# re-structure data for plotting
matches_plot <- matches
matches_plot$orientation <- factor(matches_plot$orientation,
                                   levels = c("Flat", "N", "NE", "E", "SE", "S", "SW", "W", "NW"))
# plot
panel_d <- ggplot() +
  geom_tile(data = matches_plot,
            aes(x = longitude, y = latitude, fill = orientation)) +
  geom_contour(data = elevation, aes(x = longitude, y = latitude, z = elevation),
               col = "white", alpha = 0.5, linewidth = 0.5, bins = 20) +
  geom_text_contour(data = elevation, aes(x = longitude, y = latitude, z = round(elevation)),
                    skip = 1, stroke = 0.2, col = "black", size = 2, bins = 10) +
  geom_segment(aes(x = -119.6266, xend = -119.6266,  y = 39.868375, yend = 39.86850),
               data = NULL, col = "black", linewidth = 1,
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_text(aes(x = -119.6266 - 0.000075,  y = 39.8684375),
            data = NULL, col = "black", label = "N", size = 8) +
  scale_fill_manual(values = orientation_colors) +
  theme_void() +
  theme(legend.position = c(0.15, 0.82),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 15)) +
  labs(fill = "Orientation") +
  ggtitle("D")

# combine panels
grid.arrange(panel_a, panel_b, panel_c, panel_d, ncol = 2, nrow = 2)

## Figure 1 Panel D -----------------------------------------------------------

# generate OTM splines
otm_data <- rnp_otm_data(folder_path = "data/otm_data",
                         rows_skip = 14, time_col = 1, op_temp_col = 3)
otm_metadata <- read.csv("data/otm_metadata.csv")
otm_data <- add_otm_metadata(otm_data = otm_data, otm_metadata = otm_metadata)
otm_splines <- gen_otm_splines(otm_data = otm_data, knot_p = 96/720)

# prediction holder dataset
prediction <- tibble(latitude = c(), longitude = c(), date = c(),
                     minute = c(), pred_op_temp = c())

# minutes sequence
minutes_sequence <- c(540, 720, 1020)

# loop to predict temperatures
for(i in 1:length(minutes_sequence)){

  # predict for minute x
  pred <- predict_thermal_landscape(matches = matches, otm_splines = otm_splines,
                                    date = 235, minute = minutes_sequence[i])

  # bind prediction
  prediction <- rbind(prediction, pred)

}

# merge prediction with elevation data
prediction <- prediction %>%
  mutate(hour = paste(minute/60, ":00", sep = "")) %>%
  mutate(hour = fct_reorder(hour, minute))


prediction %>%
  mutate(hour = paste(minute/60, ":00", sep = "")) %>%
  mutate(hour = fct_reorder(hour, minute)) %>%
  ggplot(aes(x = longitude, y = latitude, fill = pred_op_temp)) +
  geom_tile() +
  #geom_contour(aes( z = elevation),
               #col = "white", alpha = 0.5, linewidth = 0.5, bins = 20) +
  #geom_text_contour(aes( z = round(elevation)),
                    #skip = 1, stroke = 0.2, col = "black", size = 2, bins = 10) +
  geom_text(aes(x = -119.6275, y = 39.869125, label = hour),
            size = 6) +
  geom_segment(aes(x = -119.6266, xend = -119.6266,  y = 39.868375, yend = 39.86850),
               data = NULL, col = "black", linewidth = 1,
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_text(aes(x = -119.6266 - 0.000075,  y = 39.8684375),
            data = NULL, col = "black", label = "N", size = 8) +
  scale_fill_viridis(option = "magma") +
  facet_grid(cols = vars(hour)) +
  theme_void() +
  theme(strip.text = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title = "Predicted operative temperature (°C)"))


ggplot() +
  geom_tile(data = prediction, aes(x = longitude, y = latitude, fill = pred_op_temp)) +
  facet_grid(cols = vars(hour)) +
  geom_contour(data = elevation, aes(x = longitude, y = latitude, z = elevation),
               col = "white", alpha = 0.5, linewidth = 0.5, bins = 20) +
  geom_text_contour(data = elevation, aes(x = longitude, y = latitude, z = round(elevation)),
                    skip = 1, stroke = 0.2, col = "black", size = 2, bins = 10) +
  geom_segment(aes(x = -119.6266, xend = -119.6266,  y = 39.868375, yend = 39.86850),
               data = NULL, col = "black", linewidth = 1,
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_text(aes(x = -119.6266 - 0.000075,  y = 39.8684375),
            data = NULL, col = "black", label = "N", size = 8) +
  scale_fill_viridis(option = "magma") +
  theme_void() +
  theme(strip.text = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title = "Predicted operative temperature (°C)"))


## Figure S1 ------------------------------------------------------------------

# set file path for flight
folder <- "C:/Users/ggarc/OneDrive/research/throne_data/VAL2_GCP_26.tif"

# process at 4, 5 & 6 decimal digits
dig4 <- rnp_flight_data(file_path = folder, digits = 4)
dig5 <- rnp_flight_data(file_path = folder, digits = 5)
dig6 <- rnp_flight_data(file_path = folder, digits = 6)

# panel a
panel_d4 <- dig4 %>%
  ggplot(aes(x = longitude, y = latitude, fill = ref_temp)) +
  geom_tile() +
  scale_fill_viridis(option = "magma", limits = c(25, 65)) +
  geom_segment(aes(x = -119.6264, xend = -119.6264,  y = 39.868375, yend = 39.86850),
               data = NULL, col = "black", linewidth = 1,
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_text(aes(x = -119.6264 - 0.000075,  y = 39.8684375),
            data = NULL, col = "black", label = "N", size = 6) +
  theme_void() +
  theme(legend.position = c(0.1, 0.81),
        legend.key.size = unit(0.5, "cm"),) +
  guides(fill = guide_colorbar("°C")) +
  ggtitle("A) 4 decimal digits")

# panel b
panel_d5 <- dig5 %>%
  ggplot(aes(x = longitude, y = latitude, fill = ref_temp)) +
  geom_tile() +
  scale_fill_viridis(option = "magma", limits = c(25, 65)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("B) 5 decimal digits")

# panel c
panel_d6 <- dig6 %>%
  ggplot(aes(x = longitude, y = latitude, fill = ref_temp)) +
  geom_raster() +
  scale_fill_viridis(option = "magma", limits = c(25, 65)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("C) 6 decimal digits")

# merge panels
grid.arrange(panel_d4, panel_d5, panel_d6, nrow = 1, ncol = 3)


