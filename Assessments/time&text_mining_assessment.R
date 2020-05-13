library(dslabs)
library(tidyverse)
library(lubridate)
options(digits = 3)    # 3 significant digits

data(brexit_polls)
# view(brexit_polls)

ex_1 <- brexit_polls %>%  
  filter(month(.$startdate) == 4)

ex_2 <- brexit_polls %>% 
  mutate(enddate = round_date(enddate, unit = "week")) %>% 
  filter(enddate == "2016-06-12")
nrow(ex_2)

ex_3 <- brexit_polls %>% 
  mutate(weekday_end = weekdays(enddate)) %>% 
  # count(weekday_end) %>%
  ggplot(aes(weekday_end, fill = weekday_end)) +
  geom_bar()

data(movielens)
# view(movielens)

ex_4 <- movielens %>% 
  mutate(timestamp = hour(as_datetime(timestamp))) %>% 
  count(timestamp) %>% 
  arrange(desc(n))

library(gutenbergr)
library(tidytext)

pride_txt <- gutenberg_download(gutenberg_works(title == "Pride and Prejudice")$gutenberg_id)

words <- pride_txt %>% 
  unnest_tokens("word",text)

words <- words %>% 
  filter(!word %in% stop_words$word,! str_detect(word,"\\d")) 
nrow(words)

words %>% count(word) %>% arrange(desc(n)) %>% 
  filter(n >= 100) %>% nrow

afinn <- get_sentiments("afinn")

sentiment_pp <- words %>% 
  inner_join(afinn)

sentiment_pp %>% 
  nrow()
sum(sentiment_pp$value == 4 )



