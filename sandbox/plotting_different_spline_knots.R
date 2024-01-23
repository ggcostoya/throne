
# Splines test #

# extract spline for one OTM
otm_x <- otm_data %>% filter(otm_id == "OTM02") %>% filter(date == 235)

# define holder dataset
preds <- data.frame(x = c(), y = c(), knot_p = c())
knots <- c(0.5,0.25, 0.06667, 0.05, 0.0333, 0.016667, 0.00833334)

# loop to generate data from splines
for(i in 1:length(knots)){

  otm_spline <- gen_otm_splines(otm_data = otm_x, knot_p = knots[i])
  pred <- as.data.frame(predict(otm_spline$splines[[1]], seq(0,1440)))
  pred$knot_p <- rep(knots[i], nrow(pred))
  preds <- rbind(preds, pred)

}

# plotting

ggplot() +
  geom_point(data = otm_x, aes(x = minute/60, y = op_temp), size = 0.5, alpha = 0.25) +
  geom_line(data = preds, aes(x = x/60, y = y,
                              col = as.factor(round((knot_p*1440)/24, digits = 2))), alpha = 0.5) +
  xlab("Hour of the day") +
  ylab("Operative temperature") +
  theme_classic() +
  guides(color = guide_legend(title = "Knots / h")) +
  ggtitle("OTM 02 on DOY 235")

