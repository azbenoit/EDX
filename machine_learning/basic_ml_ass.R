library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% group_by(type) %>% 
  summarise(mean(sex == "Female"))

y_hat <- factor(ifelse(x == "inclass", "Female", "Male"))

confusionMatrix(y_hat,y)

table(y_hat, y)

# IRIS DATASET
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

suppressWarnings(set.seed(2, sample.kind="Rounding"))
i <- suppressWarnings(createDataPartition(y, 1, .5, F))
test <- iris[i,]
train <- iris[-i,]

# Actual way
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)


# # Big dumb long way
m <- train %>% group_by(Species) %>% summarise(mean(Sepal.Length), mean(Sepal.Width),
                                          mean(Petal.Length), mean(Petal.Width),
                                          sd(Sepal.Length), sd(Sepal.Width),
                                          sd(Petal.Length), sd(Petal.Width))
train2 <- train %>% pivot_longer(c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width), names_to = "key", values_to = "value")

cutoff <- seq(1,8, by = .01)
accuracy <- map(cutoff, function(x){
  train2 %>% group_by(key) %>% mutate(y_hat = factor(ifelse(value > x, "virginica", "versicolor"), levels = c("virginica", "versicolor")),
                                     Species = factor(Species, levels = c("virginica", "versicolor"))) %>%
    summarise(accuracy = mean(y_hat == Species)) %>% .$accuracy
})

df <- data.frame(t(setNames(data.frame(accuracy), 1:701)))
df <- setNames(df, c("Petal.Length", "Petal.Width","Sepal.Length","Sepal.Width"))
max(df[1])
max(df[2])
max(df[3])
max(df[4])
i <- which.max(df$Petal.Length)
best_cutoff <- cutoff[i]

# TEST
test %>% mutate(y_hat = factor(ifelse(Petal.Length > best_cutoff, "virginica", "versicolor"), levels = levels(Species))) %>% 
  summarise(acuracy = mean(y_hat == Species))

# EDX answer:
# predictions <- foo(train[,3])
# rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
# cutoffs <-rangedValues[which(predictions==max(predictions))]
# 
# y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
# mean(y_hat==test$Species)

predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)

plot(iris,pch=21,bg=iris$Species)

predictions4 <- foo(train[,4])
rangedValues4 <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs4 <-rangedValues4[which(predictions4==max(predictions4))] %>% .[1]

y_hat <- factor(ifelse(test[,4]>cutoffs4 | test[,3] > best_cutoff,'virginica','versicolor'), levels = levels(test$Species))
mean(y_hat==test$Species)