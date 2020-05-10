library(tidyverse)
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>% 
  left_join(Master, "playerID") %>% 
  select(playerID, nameFirst, nameLast, HR)

top_salary <- Salaries %>% 
  filter(yearID == 2016) %>% 
  right_join(top_names) %>% 
  select(nameFirst, nameLast, teamID, HR, salary)

Awards <- AwardsPlayers %>% 
  filter(yearID == 2016)

non_top_awards <- anti_join(Awards, top_names)

nrow(semi_join(Master, non_top_awards))