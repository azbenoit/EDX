library(tidyverse)
library(broom)
library(Lahman)
data("Teams")
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G,
         R = R/G,
         HR = HR/G)
lm(avg_attendance~yearID, data= Teams_small)

cor(Teams_small$W,Teams_small$HR)

strat <- Teams_small %>% 
  mutate(W = round(W/10)) %>% 
  group_by(W) %>% 
  filter(n() >= 20) %>% 
  do(tidy(lm(avg_attendance~HR, data = .)))

plot <- strat %>%  filter(term == "HR") %>% 
  ggplot(aes(W,estimate)) +
  geom_point()

m <- lm(avg_attendance~R+HR+W+yearID, data = Teams_small)

df <- data.frame(R = 5, HR = 1.2, W = 80, yearID = 1960)
predict(m,df)

te <- Teams %>% 
  filter(yearID == 2002) %>% 
  mutate(R = R/G,
         HR = HR/G) 
te %>% mutate(predicted = predict(m,.),
         avg_a = attendance/G) %>%
  summarise(cor(predicted, avg_a))
  
