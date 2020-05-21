# A = Having disease
# B = Test is positive = p(B|)
A <- .02
B_g_A <- .85
Bc_g_Ac <- .9
B_g_Ac <- .1
B <- B_g_A * A + B_g_Ac * (1-A)

A_g_B <- B_g_A*A/B

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test)

mean(test == 0 & disease == 1) / mean(test == 0)

mean(test & disease) / mean(test) / mean(disease)

library(dslabs)
library(tidyverse)
data("heights")
# Finding   𝑃(𝑥)=Pr(Male|height=𝑥)  for each  𝑥 .
heights %>%
  mutate(height = cut(height, quantile(height, seq(0, 1, 0.1)), include.lowest = TRUE)) %>% 
  group_by(height) %>% 
  summarise(p = (mean(sex == "Male") )) %>% 
qplot(height, p, data =.)

# Create a bivariate distribution
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat$x,dat$y)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
