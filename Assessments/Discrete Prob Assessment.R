library(gtools)
library(tidyverse)
options(digits = 3)

set.seed(1, sample.kind="Rounding")
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
Pr <- replicate(1,{
  s <- sample(runners,3, replace = FALSE)
  all(s == c("Jamaica", "Jamaica", "Jamaica"))
})
mean(Pr)

eats <- function(n){
  6*nrow(combinations(n,2))*3
}

p <- sapply(6:12,eats)
plot(6:12,p)

all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

a <- esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(ncontrols)
a/all_controls

t <- esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  summarise(ncases = sum(ncases),ncontrols = sum(ncontrols), all_cases = all_cases) %>%
  mutate(p = ncases/all_controls) %>%
  pull(ncontrols)
t/all_controls


  
  
  
  
  