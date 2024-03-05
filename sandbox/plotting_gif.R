

library(viridis)
library(gganimate)
library(gifski)
library(magick)
library(metR)
library(transformr)

pred <- predict_thermal_landscape(matches = matches, otm_splines = otms_splines,
                          doy = c(237), mod = seq(0,1440,by = 60))

pred$hour <- floor(pred$mod/60)


x <- pred %>%
  ggplot(aes(x = longitude, y = latitude, fill = pred_op_temp)) +
  geom_tile() +
  scale_fill_viridis(option = "magma") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.25, 0.85)) +
  labs(fill = "Predicted operative temperature (C)",
       title = "Hour: {frame_time}") +
  transition_time(as.integer(hour)) +
  enter_fade() +
  exit_fade()
anim <- animate(x, height = 5, width = 5, units = "in", fps = 5, res = 200, renderer = gifski_renderer())
anim_save("dynamic_thermal_landscape.gif", anim)
