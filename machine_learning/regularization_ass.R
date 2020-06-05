library(dslabs)
library(tidyverse)
library(caret)
suppressWarnings(set.seed(1986, sample.kind="Rounding"))
n <- round(2^rnorm(1000, 8, 1))
suppressWarnings(set.seed(1, sample.kind="Rounding"))
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
schools %>% top_n(10, quality) %>% arrange(desc(quality))
suppressWarnings(set.seed(1, sample.kind="Rounding"))
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# Top schools based on average score
top_by_score <- schools %>% arrange(desc(score)) %>% top_n(10, score)

# Compare the median school size to the median school size of the top 10 schools based on the score.
median(schools$size)
median(top_by_score$size)
schools %>% arrange(score) %>% top_n(-10, score) %>% summarise(median(size))

schools %>% ggplot(aes(size,score)) +
  geom_point(alpha = .5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

# Using regularization to reduce variability based on size
overall <- mean(sapply(scores, mean))

lambda <- 25
schools <- schools %>% mutate(b_i = (score - overall)/(lambda + n)*size,
                              regularized_score = overall + b_i)
schools %>% arrange(desc(regularized_score)) %>% top_n(10, regularized_score)

# Other way:
# score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+lambda))
# schools %>% mutate(score_reg = score_reg) %>%
#   top_n(10, score_reg) %>% arrange(desc(score_reg))

# Finding best lambda
lamddas <- 10:250
RMSEs <- map_df(lamddas, function(lambda){
  RMSE <- schools %>% mutate(b_i = (score - overall)/(lambda + n)*size,
                             regularized_score = overall + b_i) %>% 
    summarise(RMSE = sqrt(sum((quality - regularized_score)^2)/nrow(.))) %>% .$RMSE
  data.frame(RMSE, lambda)
})
RMSEs %>% arrange(RMSE) %>% head(.)
lambda <- 135
schools <- schools %>% mutate(b_i = (score - overall)/(lambda + n)*size,
                              regularized_score = overall + b_i)
schools %>% arrange(desc(regularized_score)) %>% top_n(10, regularized_score)
