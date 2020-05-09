library(dslabs)
library(tidyverse)
options(digits = 3)
data(brexit_polls)

p <- .481
q <- 1-p
d <- 2*p-1
N <- 1500
#ex <- p*N
#se <- sqrt(p*q*N)
X <- p
SE <- sqrt(p*q/N)
SED <- 2*SE

brexit_polls <-  brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
se <- sqrt(brexit_polls$x_hat*(1-brexit_polls$x_hat)/brexit_polls$samplesize)

ci<- brexit_polls$x_hat[1] + c(-1,1)*qnorm(.975)*se[1]

june_polls <- brexit_polls %>% 
  filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_d = 2*se_x_hat,
         upper = spread + qnorm(.975)*se_d,
         lower = spread - qnorm(.975)*se_d,
         hit = d >= lower & d <= upper) 

pr_hit_by_pollster <- june_polls %>%
  group_by(pollster) %>%
  summarise(pr_hits = mean(hit), n = n()) %>%
  arrange(pr_hits)

#june_polls %>%
#  ggplot(aes(poll_type, spread, fill = poll_type)) +
#  geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarise(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1) / 2,
            se = 2*sqrt(p_hat * (1- p_hat)/N),
            lower = spread - qnorm(.975)*se,
            upper = spread + qnorm(.975)*se)
combined_by_type

brexit_hit <- brexit_polls %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_d = 2*se_x_hat,
         p_hat = x_hat,
         upper = spread + qnorm(.975)*se_d,
         lower = spread - qnorm(.975)*se_d,
         hit = d >= lower & d <= upper) %>%
   group_by(poll_type) %>%
  summarise(hits = sum(hit), misses = length(hit) - sum(hit)) #%>%
  #ungroup()%>%
  #select(-poll_type)

#chisq.test(brexit_hit)

#odds ratio
(48/37)/(10/32)

brexit_polls %>% 
  ggplot(aes(enddate,spread, color = poll_type)) +
  geom_smooth(method = "loess", span = .4) +
  geom_point(alpha = .4) +
  geom_hline(yintercept = -0.038)

brexit_long <- brexit_polls %>%
  pivot_longer(c(remain:undecided),names_to = "vote", values_to = "proportion") %>%
  mutate(vote = factor(vote)) %>%
  ggplot(aes(enddate, proportion, color = vote)) +
  geom_smooth(method = "loess", span = .3) +
  geom_point(alpha = .4) 
brexit_long



