

library(viridis)
library(gganimate)
library(gifski)
library(magick)
library(metR)
library(transformr)

pred <- predict_thermal_landscape(matches = c_matches, otm_splines = c_otms_splines,
                          doy = 180, mod = seq(360,1260,by = 60))

pred$hour <- floor(pred$mod/60)

x <- pred %>%
  ggplot(aes(x = longitude, y = latitude, fill = pred_op_temp)) +
  geom_tile() +
  scale_fill_viridis(option = "magma") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "Predicted operative temperature (C)",
       title = "Hour: {frame_time}") +
  transition_time(as.integer(hour)) +
  enter_fade() +
  exit_fade()
anim <- animate(x, height = 6, width = 5, units = "in", fps = 5, res = 200, renderer = gifski_renderer())
anim_save("case_dynamic_thermal_landscape.gif", anim)
