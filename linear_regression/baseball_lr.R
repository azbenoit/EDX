library(Lahman)
data("Teams")

teams <- Teams %>% filter(yearID >= 1961, yearID <= 2001) %>% 
  mutate(win_rate = W/(W+L), R_avg = R/G, AB_avg = AB/G, E_avg = E/G, BB_avg = BB/G, 
         HR_avg = HR/G)
plot <- teams %>%
  ggplot(aes(X2B, X3B)) +
  geom_point(alpha = .5)
teams %>% summarise(RABcor = cor(X2B/G,X3B/G)) %>% .$RABcor

summary(lm(R_avg~BB_avg+HR_avg, data = teams))

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

Batting %>% filter(yearID >= 1999, yearID <= 2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) %>% 
  group_by(playerID) %>% 
  summarise(mean_singles = mean(singles), mean_bb = mean(bb)) %>% 
  inner_join(bat_02)  %>% 
  lm(bb~mean_bb, data = .)
