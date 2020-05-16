
library(tidyverse)
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_d <- mean(female_heights$daughter)
mu_m <- mean(female_heights$mother)
sigma_d <- sd(female_heights$daughter)
sigma_m <- sd(female_heights$mother)
rho <- cor(female_heights$daughter, female_heights$mother)

m_dm <- rho*(sigma_d/sigma_m)
intercept_dm <- mu_d - m_dm*mu_m

intercept_dm + m_dm*60

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(female_heights, N, replace = TRUE) %>%
    lm(daughter ~ mother, data = .) %>% .$coef 
})
cor(lse[1,], lse[2,]) 

predict(lm(mother~daughter,data = female_heights))

