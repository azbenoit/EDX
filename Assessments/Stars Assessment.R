library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3)  

stars <- stars %>% 
  filter(!is.na(magnitude))

smean <- mean(stars$magnitude)
ssd <- sd(stars$magnitude)

stars %>%
  ggplot(aes(log10(temp), magnitude, color = type)) +
  scale_y_reverse() +
  scale_x_continuous(trans = "reverse") +
  xlab("Log 10 of Temp (kelvin)") +
  ylab("Luminosity") +
  geom_point() 
