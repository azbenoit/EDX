library(Lahman)
library(tidyverse)
library(broom)
data("Teams")

eff <- Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>% 
  do(tidy(lm(R~BB+HR,data = .), conf.int = T)) %>% 
  filter(term == "BB") 
eff %>%  
  ggplot(aes(yearID, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_smooth(method = "lm") +
  geom_point()

summary(lm(estimate~yearID, data = eff))
