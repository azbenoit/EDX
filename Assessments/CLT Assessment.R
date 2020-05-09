library(tidyverse)
options(digits = 3)
set.seed(21, sample.kind = "Rounding")

p <- 5/38
q <- 1-p
a <- 6
b <- -1
n<- 500
B <- 10000
avg <- (p*a + q*b) *n
se <- sqrt(n) * abs(b-a) * sqrt(p*q)
p35 <- pnorm(0, avg, se)

#S <- replicate(B, {
#  samp <- sample(c(a,b), n, replace = TRUE, prob = c(p,q))
# sum(samp)
#})
#mean(S>=8)

ans <- data.frame(p,avg,p35) %>%
  filter(p35 >= .8) %>%
  summarise(ans = min(p)) %>%
  pull(ans)

