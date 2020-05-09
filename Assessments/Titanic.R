options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic_train$Survived <- ifelse(titanic_train$Survived == 0, "No", "Yes")
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>%
  filter(!is.na(Age), !is.na(Sex), !is.na(Survived), !is.na(Fare), Fare > 0 )



## Density plot
titanic %>% 
  ggplot(aes(Age, fill = Survived)) +
  geom_density(aes(y = ..count..), alpha = .2, position = "stack") +
  ylab("Count") +
  facet_grid(Sex~Pclass)

## QQ plot
##Long way
sampleq <- quantile(titanic$Age, seq(.001,1,.001), na.rm = TRUE)
theoretical <- qnorm(seq(.001,1,.001), mean = mean(titanic$Age, na.rm = TRUE), sd = sd(titanic$Age, na.rm = TRUE))
qq_df <- data.frame(sampleq = sampleq, theoretical = theoretical)
qq_df %>%
  ggplot(aes(theoretical,sampleq)) +
  geom_point(alpha = .5) +
  geom_abline(alpha = .2)

##Short way
params <- titanic %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

##Bar plot
titanic %>% 
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

##Box plot
titanic %>%
  ggplot(aes(Survived, Fare, fill = Survived)) +
  scale_y_continuous(trans = "log2") +
  geom_boxplot() +
  geom_jitter(width = .1, alpha = .2)

  
  