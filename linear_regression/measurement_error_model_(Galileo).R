# The code to use dslabs function rfalling_object to generate simulations of dropping balls:
  library(dslabs)
  library(tidyverse)
falling_object <- rfalling_object()
# The code to draw the trajectory of the ball:
  falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")
# The code to use the lm() function to estimate the coefficients:
  fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)

# The code to check if the estimated parabola fits the data:
  augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")
# The code to see the summary statistic of the regression:
  tidy(fit, conf.int = TRUE)