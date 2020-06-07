library(tidyverse)
library(caret)
edx <- if(!exists("edx")){read_rds("movielens_project/data/edx.rds")}else{edx}
sum(edx$rating == 3)
edx %>% group_by(movieId) %>% summarise(n()) %>% nrow(.)
n_distinct(edx$userId)
# Getting distinct genres
edx %>% group_by(genres) %>% summarise(n = n()) %>% 
  separate(genres,into = c("g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8"), sep = "\\|") %>% 
  pivot_longer(cols = -n,names_to = "gnum", values_to = "genre", values_drop_na = T) %>%
  select(-gnum) %>% group_by(genre) %>% summarise(n = sum(n))
# Another easier way of coding it
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
# Movie with the most ratings
edx %>% group_by(title) %>% summarise(n = n()) %>% arrange(desc(n))
# Most given ratings
edx %>% group_by(rating) %>% summarise(n  = n()) %>% arrange(desc(n)) %>% 
  mutate(half = str_detect(rating, "\\.5$")) %>% group_by(half) %>% 
  summarise(n = sum(n))
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
