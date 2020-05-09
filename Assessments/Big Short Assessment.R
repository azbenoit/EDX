library(tidyverse)
library(dslabs)
data(death_prob)
options(digits = 3)
set.seed(29, sample.kind = "Rounding")

#p<- death_prob %>%
#  filter(age == 50, sex == "Male") %>%
#  pull(prob)
p<- .015
q<- 1 - p
a <- -150000
z <- qnorm(.05)
n <- 1000
b <-(z*a*sqrt(p*q*n) - n*p*a) / (z*sqrt(p*q*n) + n*q) 
Ex <- (p*a + q*b) * n
SE <- abs(a-b) * sqrt(p*q*n) 
prl <- pnorm(-1000000,Ex,SE)
Ex/n
data.frame(p,prl) %>%
  filter(prl>=.9) %>%
  summarise(min = min(p)) %>%
  pull(min)
B<- 10000

profits <- replicate(B,{
  p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  q <- 1- p
  s <- sample(c(a,b),n,replace = TRUE, prob = c(p,q))
  sum(s)/(1e+6)
})

mean(profits)







