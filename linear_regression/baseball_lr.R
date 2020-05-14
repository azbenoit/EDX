library(Lahman)
data("Teams")

Teams %>% filter(yearID >= 1961, yearID <= 2001) %>% 
  mutate(win_rate = W/(W+L)) %>% 
  ggplot(aes(X2B, X3B)) +
  geom_point(alpha = .5)
