library(dslabs)
library(tidyverse)
library(broom)
data("research_funding_rates")
# Long wack way
tab <- research_funding_rates %>% 
  select(discipline,applications_men,applications_women, awards_men, awards_women) %>% 
  pivot_longer(c(applications_men, applications_women), names_to = "gender", values_to = "applications") %>% 
  pivot_longer(c(awards_men,awards_women), names_to = "gender2", values_to = "awards")

tab$gender <- str_replace_all(string = tab$gender, pattern = "applications_", replacement = "") 
tab$gender2 <- str_replace_all(string = tab$gender2, pattern = "awards_", replacement = "")
    
t1 <- tab %>% select(discipline,gender, applications)
t2 <- tab %>% select(discipline,gender2, awards) %>% rename(gender = gender2)
pat2 <- c(seq(1,nrow(t2), by = 4), seq(2,nrow(t2), by = 4))
t1 <- t1[seq(1, nrow(t1), by = 2),]
t2 <- t2[pat2,]
tab <- inner_join(t1,t2, c("discipline","gender")) %>% 
  group_by(gender) %>% 
  summarise(awarded = sum(awards), not = sum(applications) - sum(awards))

# Better way
two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
tidy(chisq.test(two_by_two[-1]))

# Checking for Simpson's paradox
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")

dat %>% ggplot(aes(discipline,success, color = gender, size = applications)) +
  geom_point()

