library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST"))

# For convenience we include the result of the code above in the dslabs package:
library(dslabs)
data("trump_tweets")

head(trump_tweets)

names(trump_tweets)

?trump_tweets

trump_tweets %>% count(source) %>% arrange(desc(n))

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

# For each tweet, we will extract the hour, in the east coast (EST), it was tweeted 
# then compute the proportion of tweets tweeted at each hour for each device.

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

library(tidytext)

# unnest_tokens() :  The functions will take a vector of strings and extract the tokens so that each one gets a row in the new table.
# A token refers to the units that we are considering to be a data point. 
# The most common tokens will be words, but they can also be single characters, ngrams, sentences, lines or a pattern defined by a regex.

# Example 
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

# Now let's look at a quick example with a tweet number 3008:

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

# defining is a patter that starts with @, # or neither and is followed by any combination of letters or digits:
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

# For each device and a given word, let's call it y, 
# we compute the odds or the ratio between the proportion of words that are y and not y and compute the ratio of those odds.
# Here we will have many proportions that are 0 so we use the 0.5 correction.

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  pivot_wider(names_from = source, values_from = n, values_fn = str_replace_na(replacement = 0)) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

# Given that several of these words are overall low frequency words we can impose a filter based on the total frequency like this:
  
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

# ------------------------------------------------------------------------- #
# -------------------------SENTIMENT ANALYSIS------------------------------ #
# ------------------------------------------------------------------------- #

# The tidytext package includes several maps or lexicons in the object sentiments:
  
# the bing lexicon divides words into positive and negative
get_sentiments("bing")

# The AFINN lexicon assigns a score between -5 and 5, with -5 the most negative and 5 the most positive.
get_sentiments("afinn")

# The loughran and nrc lexicons provide several different sentiments:
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

# For the analysis here we are interested in exploring the different sentiments of each tweet, so we will use the nrc lexicon:
nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

# we will count and compare the frequencies of each sentiment appears for each device.
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

# Because more words were used on the Android than on the phone:
tweet_words %>% group_by(source) %>% summarize(n = n())
# for each sentiment we can compute the odds of being in the device: proportion of words with sentiment versus proportion of words without and then compute the odds ratio comparing the two devices:
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

# for each sentiment, an odds ratio and confidence interval. We will add the two values we need to form a two-by-two table and the odds ratio:
  
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

# A graphical visualization shows some sentiments that are clearly overrepresented:

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

# We see that the disgust, anger, negative sadness and fear sentiments are associated with the Android in a way that is hard to explain by chance alone. Words not associated to a sentiment were strongly associated with the iPhone source, which is in agreement with the original claim about hyperbolic tweets.

# If we are interested in exploring which specific words are driving these differences, we can back to our android_iphone_or object:
  
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))
# We can make a graph:
  
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 