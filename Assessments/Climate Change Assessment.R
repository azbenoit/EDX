library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#Temperature Anomaly line graph
p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year,temp_anomaly)) +
  geom_line() +
  geom_line(aes(y = ocean_anomaly), color = "green") +
  geom_line(aes(y = land_anomaly), color = "brown")
p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p <- p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = .05, label = "20th century mean"), color = "blue")

g <- greenhouse_gases %>%
  mutate(gas = factor(gas)) 

g %>% ggplot(aes(year, concentration, color = gas)) +
  geom_line() +
  geom_vline(xintercept = 1850, color = "blue") +
  scale_y_continuous(breaks = c(0,100,275,500,1000,1500), limits = c(0,2000))
  #facet_grid(gas~.) 

t <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  geom_vline(xintercept = 1970, color = "blue") +
  geom_vline(xintercept = 2014, color = "blue") +
  geom_vline(xintercept = 1960, color = "blue") 

historic_co2 %>% 
  ggplot(aes(year,co2, color = source)) +
  scale_x_continuous(limits = c(-3000,2018)) +
  geom_line()
  






