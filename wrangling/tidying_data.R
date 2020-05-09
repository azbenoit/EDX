library(tidyverse)

# Simple: from already fairly tidy data
wide_data <- read_csv(file.path("data/fertility-two-countries-example.csv"))

new_tidy_data <- wide_data %>%
  pivot_longer(-country, names_to = "year", values_to = "fertility") %>% 
  mutate(year = as.numeric(year))
new_wide_data <- new_tidy_data %>% 
  pivot_wider(names_from = year, values_from = fertility)

# More messy
wide_data2 <- read_csv(file.path("data/life-expectancy-and-fertility-two-countries-example.csv"))

tidy_data2 <- wide_data2 %>% 
  pivot_longer(-country, "key", "value") %>% 
  separate(key, c("year", "variable_name"),sep =  "_", extra = "merge") %>% 
  pivot_wider(names_from = variable_name,values_from =  value)


# Assessment
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_tidy <- co2_wide %>% 
  pivot_longer(-year, "month", values_to = "co2")
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat <- dat %>% 
  pivot_wider(names_from = gender, values_from = admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
