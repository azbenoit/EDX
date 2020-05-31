library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

suppressWarnings(set.seed(42, sample.kind = "Rounding"))
i <- createDataPartition(y = titanic_clean$Survived, list = F, p = .2)
train <- titanic_clean[-i,]
test <- titanic_clean[i,]
mean(train$Survived == "1")

# Guessing the outcome
suppressWarnings(set.seed(3, sample.kind = "Rounding"))
y_hat_guess <- sample(c("0","1"), nrow(test), replace = T)
mean(y_hat_guess == test$Survived)

# Predicting survival by sex
mean(train$Survived == "1" & train$Sex == "female")/mean(train$Sex == "female")
mean(train$Survived == "1" & train$Sex == "male")/mean(train$Sex == "male")
y_hat_sex <- factor(ifelse(test$Sex == "female", "1", "0"), levels = levels(test$Survived))
mean(y_hat_sex == test$Survived)
confusionMatrix(y_hat_sex, test$Survived)
F_meas(y_hat_sex, test$Survived)

# Predicting survival by passenger class
train %>% 
  mutate(Pclass = factor(Pclass)) %>% 
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()
train %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))
y_hat_class <- factor(ifelse(test$Pclass == 1, "1", "0"))
mean(y_hat_class  == test$Survived)
confusionMatrix(y_hat_class, test$Survived)
F_meas(y_hat_class, test$Survived)

# Both Sex and Class
train %>% 
  group_by(Pclass, Sex) %>% 
  summarise(Survived = mean(Survived == "1"))
y_hat_sex_class <- factor(ifelse(test$Pclass != 3 & test$Sex == "female", "1", "0"))
mean(y_hat_sex_class  == test$Survived)
confusionMatrix(y_hat_sex_class, test$Survived)
F_meas(y_hat_sex_class, test$Survived)

# Survival by fare - LDA and QDA
# LDA
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
fit_lda <- train(Survived~Fare, data = train, method = "lda")
y_hat_lda <- predict(fit_lda, test, type = "raw")
mean(y_hat_lda  == test$Survived)
# QDA
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
fit_qda <- train(Survived~Fare, data = train, method = "qda")
y_hat_qda <- predict(fit_qda, test, type = "raw")
mean(y_hat_qda  == test$Survived)

# Logistic regression models 
# Age only
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
fit_glm_age <- train(Survived~Age, data = train, method = "glm")
y_hat_glm_age <- predict(fit_glm_age, newdata = test, type = "raw")
mean(y_hat_glm_age  == test$Survived)
# sex, class, fare, and age.
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
fit_glm <- train(Survived~Age+Sex+Fare+Pclass, data = train, method = "glm")
y_hat_glm <- predict(fit_glm, newdata = test, type = "raw")
mean(y_hat_glm  == test$Survived)
# All predictors
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
fit_glm_all <- train(Survived~., data = train, method = "glm")
y_hat_glm_all <- predict(fit_glm_all, newdata = test, type = "raw")
mean(y_hat_glm_all  == test$Survived)

# KNN
train_x <- train %>% select(-Survived) %>% as.matrix(.)
train_y <- train %>% select(Survived) %>% as.vecto
suppressWarnings(set.seed(6, sample.kind = "Rounding"))
fit_knn <- train(train_x, train_y, data = train, method = "knn", 
                 tunegrid = data.frame(k = seq(3, 51, 2)))
