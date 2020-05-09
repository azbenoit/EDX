set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000,20.9,5.7)
options(digits = 3)
mean(act_scores)
sd(act_scores)
mean(act_scores <= 10)

x <- 1:36
f_x <- dnorm(x, 20.9,5.7)
data.frame(x = x, f_x = f_x) %>%
  ggplot(aes(x,f_x)) +
  geom_line()

z <- scale(act_scores)
mean(z>2)
quantile(act_scores,.975)

F <- function(p){
  mean(act_scores<= p)
}
actCDF<-  data.frame(act = 1:36, p = sapply(1:36,F))

actCDF %>% 
  filter(p>=.95) %>%
  summarise(min = min(act)) %>%
  pull(min)

qnorm(.95,20.9,5.7)

p <- seq(0.01, 0.99, 0.01)
sq <- quantile(act_scores,p)

data.frame(act_scores) %>%
  ggplot(aes(sample = act_scores)) +
  geom_qq()


