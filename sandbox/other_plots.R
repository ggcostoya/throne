
## Plotting Other figures ##

## Packages -------------------------------------------------------------------

library(tidyverse)

## Raw OTM data dynamics ------------------------------------------------------

otm_data %>%
  filter(date %in% c(235, 236)) %>%
  mutate(hour = round(minute/60)) %>%
  ggplot(aes(y = op_temp)) +
  geom_point(aes(x = minute/60), col = "lightgray", alpha = 0.25) +
  stat_summary(aes(x = hour, group = orientation, col = orientation), geom = "line") +
  stat_summary(aes(x = hour), fun.data = mean_sdl) +
  facet_wrap(~date)
