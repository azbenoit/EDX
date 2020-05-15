library(Lahman)
data("Teams")
?Teams

teams <- Teams %>% filter(yearID >= 1961, yearID <= 2001) %>% 
  mutate(win_rate = W/(W+L), R_avg = R/G, AB_avg = AB/G, E_avg = E/G)
plot <- teams %>%
  ggplot(aes(X2B, X3B)) +
  geom_point(alpha = .5)
teams %>% summarise(RABcor = cor(X2B/G,X3B/G)) %>% .$RABcor
