
## Plotting correlation results ##

## Load `throne` package ------------------------------------------------------

devtools::load_all()

## Loading general dataset ----------------------------------------------------

load("data/corr_data.RData")

## Figure 2 -------------------------------------------------------------------

# filter correction data with GCPs only
corr_data_gcps <- corr_data %>% filter(gcps == "Y")

# get correction factors
corr_factors_gcps <- get_correction_factors(corr_data = corr_data_gcps)

# merge correction factors with correlation data
corr_data_gcps <- merge(corr_data_gcps, corr_factors_gcps[[1]],
                        by = c("year", "date", "minute"), all = TRUE)

# add column for date & time corrected ref_temp
corr_data_gcps <- corr_data_gcps %>%
  mutate(ref_temp_c1 = ref_temp + correction_factor)

# add column for temperature corrected ref_temp
corr_data_gcps <- corr_data_gcps %>%
  mutate(ref_temp_c2 = (2 - corr_factors_gcps[[3]])*ref_temp_c1 - corr_factors_gcps[[2]])

# run linear model to get fit estimate
summary(lm(ref_temp_c2 ~ op_temp, data = corr_data_gcps))

# plotting (4.45 x 4.45 inch)
corr_data_gcps %>%
  ggplot(aes(x = op_temp, y = ref_temp_c2, col = minute/60)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1.1, linetype = 2) +
  geom_smooth(method = "lm", col = "black") +
  geom_text(x = 48, y = 17, col = "black",
            label = expression(paste("y = 3.59 + 0.91x , ", R^2 == 0.7))) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue",midpoint = 12) +
  xlab("Operative Temperature (°C)") +
  ylab("Reflected Temperature (°C)") +
  theme_minimal() +
  theme(axis.line = element_line(),
        legend.position = c(0.15, 0.8),
        axis.ticks = element_line(),
        panel.grid = element_blank()) +
  labs(colour = "Hour of the day")

## Figure S2 ------------------------------------------------------------------

# load grid extra package
library(patchwork)

# linear models to get estimates
summary(lm(ref_temp ~ op_temp, data = corr_data %>% filter(gcps == "N")))

# plot panel A, raw relationship
panel_a <- corr_data %>%
  filter(gcps == "N") %>%
  ggplot(aes(x = op_temp, y = ref_temp, col = minute/60)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1.1, linetype = 2) +
  geom_smooth(method = "lm", col = "black") +
  geom_text(x = 45, y = 17, col = "black",
            label = expression(paste("y = 11.54 + 0.63x , ", R^2 == 0.42))) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue",midpoint = 12) +
  scale_x_continuous(limits = c(15,60)) +
  scale_y_continuous(limits = c(15,60)) +
  xlab("Operative Temperature (°C)") +
  ylab("Reflected Temperature (°C)") +
  theme_minimal() +
  theme(axis.line = element_line(),
        axis.ticks = element_line(),
        panel.grid = element_blank()) +
  labs(colour = "Hour of the day")

# plot panel B, gcp corrected relationship
panel_b <- corr_data %>%
  filter(gcps == "Y") %>%
  ggplot(aes(x = op_temp, y = ref_temp, col = minute/60)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1.1, linetype = 2) +
  geom_smooth(method = "lm", col = "black") +
  geom_text(x = 45, y = 17, col = "black",
            label = expression(paste("y = 6.01 + 0.75x , ", R^2 == 0.64))) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue",midpoint = 12) +
  scale_x_continuous(limits = c(15,60)) +
  scale_y_continuous(limits = c(15,60)) +
  xlab("Operative Temperature (°C)") +
  ylab("Reflected Temperature (°C)") +
  theme_minimal() +
  theme(axis.line = element_line(),
        axis.ticks = element_line(),
        panel.grid = element_blank()) +
  labs(colour = "Hour of the day")

# plot panel C, gcp + date and time corrected relationship
panel_c <- corr_data_gcps %>%
  ggplot(aes(x = op_temp, y = ref_temp_c1, col = minute/60)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1.1, linetype = 2) +
  geom_smooth(method = "lm", col = "black") +
  geom_text(x = 45, y = 17, col = "black",
            label = expression(paste("y = 11.84 + 0.69x , ", R^2 == 0.7))) +
  scale_x_continuous(limits = c(15,60)) +
  scale_y_continuous(limits = c(15,60)) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue",midpoint = 12) +
  xlab("Operative Temperature (°C)") +
  ylab("Reflected Temperature (°C)") +
  theme_minimal() +
  theme(axis.line = element_line(),
        axis.ticks = element_line(),
        panel.grid = element_blank()) +
  labs(colour = "Hour of the day")

# panel D, gcp + date and time + temp corrected relationship
panel_d <- corr_data_gcps %>%
  ggplot(aes(x = op_temp, y = ref_temp_c2, col = minute/60)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1.1, linetype = 2) +
  geom_smooth(method = "lm", col = "black") +
  geom_text(x = 45, y = 17, col = "black",
            label = expression(paste("y = 3.59 + 0.91x , ", R^2 == 0.7))) +
  scale_color_gradient2(low = "black", mid = "orange", high = "darkblue",midpoint = 12) +
  scale_x_continuous(limits = c(15,60)) +
  scale_y_continuous(limits = c(15,60)) +
  xlab("Operative Temperature (°C)") +
  ylab("Reflected Temperature (°C)") +
  theme_minimal() +
  theme(axis.line = element_line(),
        axis.ticks = element_line(),
        panel.grid = element_blank()) +
  labs(colour = "Hour of the day")

# combine plots, dimensions are 8 x 7.75 inches
(panel_a | panel_b)/(panel_c | panel_d) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.tag.position = c(0.18, 0.95))

## Figure S3 ----

# filter OTM01 on Julian date 236
otm_data_plot <- otms_data %>% filter(otm_id == "OTM01") %>%
  filter(doy == 237)

# generate OTM splines
otm_splines_15 <- gen_otm_splines(otm_data = otm_data_plot, knot_p = 360/720)
otm_splines_4 <- gen_otm_splines(otm_data = otm_data_plot, knot_p = 96/720)
otm_splines_2 <- gen_otm_splines(otm_data = otm_data_plot, knot_p = 48/720)
otm_splines_0.5 <- gen_otm_splines(otm_data = otm_data_plot, knot_p = 12/720)

# generate data
otm_15 <- as.data.frame(predict(otm_splines_15$spline[[1]])) %>%
  rename(mod = x, op_temp = y) %>% mutate(knot_h = 15)
otm_4 <- as.data.frame(predict(otm_splines_4$spline[[1]])) %>%
  rename(mod = x, op_temp = y) %>% mutate(knot_h = 4)
otm_2 <- as.data.frame(predict(otm_splines_2$spline[[1]])) %>%
  rename(mod = x, op_temp = y) %>% mutate(knot_h = 2)
otm_0.5 <- as.data.frame(predict(otm_splines_0.5$spline[[1]])) %>%
  rename(mod = x, op_temp = y) %>% mutate(knot_h = 0.5)
otm_pred <- rbind(otm_15, otm_4, otm_2, otm_0.5)
otm_pred$knot_h_name <- paste("Knots / h = ", otm_pred$knot_h, sep = "")

# plot (5 x 4.5 inches)
ggplot() +
  geom_point(data = otm_data_plot, aes(x = mod/60, y = op_temp), size = 1, alpha = 0.5) +
  geom_line(data = otm_pred, aes(x = mod/60, y = op_temp),
            col = "orange", alpha = 0.75, linewidth = 1.25) +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~knot_h_name) +
  xlab("Hour of the day") +
  ylab("Operative Temperature (°C)") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.1, color = "lightgray"),
        panel.border = element_rect(linewidth = 0.5, fill = NA),
        strip.background = element_rect(linewidth = 0.5, fill = "lightgray"),
        strip.text = element_text(size = 12))




