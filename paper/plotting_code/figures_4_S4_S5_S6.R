
## Plotting validation results ##

## Packages ----

devtools::load_all()
library(ggridges)
library(gridExtra)

## Load data to plot ----

load("data/all_val_data.RData")

## Figure 4 ----

# panel A
panel_a <- all_val_data %>%
  filter(knot_p == 1/7.5) %>%
  #mutate(mod = round(mod/96)) %>%
  #group_by(n_flights, n_otms, knot_p, mod) %>%
  #summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  mutate(n_flights_name = paste("Flights used = ", n_flights, sep = "")) %>%
  mutate(n_flights_name = fct_reorder(n_flights_name, n_flights)) %>%
  ggplot(aes(y = as.factor(n_otms), x = pred_op_temp - obs_op_temp)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density_ridges(alpha = 0.5, col = NA, fill = "royalblue", scale = 0.75) +
  stat_summary(fun.data = mean_sdl, size = 0.5) +
  xlab("Prediction Error (°C)") +
  ylab("OTMs used") +
  facet_wrap(~n_flights_name) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        strip.background = element_rect(fill = "lightgray"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25),
        axis.ticks = element_line()) +
  ggtitle("A") +
  coord_cartesian(xlim = c(-10,10))

# panel B
panel_b <- all_val_data %>%
  filter(knot_p == 1/7.5) %>%
  filter(mod > 382) %>% filter(mod < 1180) %>%
  mutate(mod = round(mod/96)) %>%
  group_by(n_flights, n_otms, knot_p, mod) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  mutate(n_flights_name = paste("Flights used = ", n_flights, sep = "")) %>%
  mutate(n_flights_name = fct_reorder(n_flights_name, n_flights)) %>%
  ggplot(aes(y = as.factor(n_otms), x = pred_op_temp - obs_op_temp)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density_ridges(alpha = 0.5, col = NA, fill = "darkorange", scale = 0.75) +
  stat_summary(fun.data = mean_sdl, size = 0.5) +
  xlab("Prediction Error (°C)") +
  ylab("OTMs used") +
  facet_wrap(~n_flights_name) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        strip.background = element_rect(fill = "lightgray"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25),
        axis.title.y = element_blank(),
        axis.ticks = element_line()) +
  ggtitle("B") +
  coord_cartesian(xlim = c(-3,3))

# dimensions 8.5 inch x 5 inch
grid.arrange(panel_a, panel_b, nrow = 1, ncol = 2)

## Figure S4 ----

fig_S4 <- all_val_data %>%
  mutate(minute = round(mod/96)) %>%
  group_by(n_flights, n_otms, knot_p, minute) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  mutate(knot_h = knot_p * 30) %>%
  ggplot(aes(y = as.factor(knot_h), x = pred_op_temp - obs_op_temp)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density_ridges(alpha = 0.5, fill = "royalblue", col = NA, scale = 0.75) +
  stat_summary(fun.data = mean_sdl, size = 0.5) +
  xlab("Prediction Error (°C)") +
  ylab("Knots / Hour on Spline Model") +
  facet_grid(cols = vars(n_flights), rows = vars(n_otms)) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        strip.background = element_rect(fill = "lightgray"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25))

# dimensions 7 x 7 inch
grid.arrange(fig_S4, top = "Flights used", right = "OTMs used")

## Figure S5 ----

fig_S5 <- all_val_data %>%
  filter(mod > 382) %>% filter(mod < 1180) %>%
  mutate(mod = round(mod/96)) %>%
  group_by(n_flights, n_otms, knot_p, mod) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  mutate(knot_h = knot_p * 30) %>%
  ggplot(aes(y = as.factor(knot_h), x = pred_op_temp - obs_op_temp)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density_ridges(alpha = 0.5, fill = "darkorange", col = NA, scale = 0.75) +
  stat_summary(fun.data = mean_sdl, size = 0.5) +
  xlab("Prediction Error (°C)") +
  ylab("Knots / Hour on Spline Model") +
  facet_grid(cols = vars(n_flights), rows = vars(n_otms)) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        strip.background = element_rect(fill = "lightgray"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25),
        axis.ticks = element_line())

# dimensions 7 x 7 inch
grid.arrange(fig_S5, top = "Flights used", right = "OTMs used")

## Figure S6 (dimensions 8 x 7 inch) ----

all_val_data %>%
  mutate(mod = round(mod/96)) %>%
  group_by(n_flights, n_otms, knot_p, mod) %>%
  summarise(obs_op_temp = mean(obs_op_temp), pred_op_temp = mean(pred_op_temp)) %>%
  mutate(knot_h = knot_p * 30) %>%
  ggplot(aes(x = (mod*96)/60, y = pred_op_temp - obs_op_temp)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(aes(group = as.factor(knot_h), col = as.factor(knot_h)),
            linewidth = 1.25, alpha = 0.75) +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = NULL, labels = NULL, name = "OTMs used")) +
  scale_x_continuous(expand = c(0,0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL, name = "Flights used")) +
  scale_color_manual(values = c("skyblue", "royalblue", "blue", "darkblue")) +
  ylab("Prediction Error (°C)") +
  xlab("Hour of the day") +
  facet_grid(cols = vars(n_flights), rows = vars(n_otms)) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        strip.background = element_rect(fill = "lightgray"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25),
        axis.ticks = element_line(),
        axis.title.y.right = element_text(margin = margin(t = 0,r = 0, b= 0, l = 10)),
        axis.title.x.top = element_text(margin = margin(t = 0,r = 0, b= 10, l = 0))) +
  labs(colour = "Knots / Hour")


