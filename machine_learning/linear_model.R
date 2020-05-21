library(tidyverse)
library(caret)
options(digits = 3)
suppressWarnings(set.seed(1, sample.kind="Rounding"))
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

suppressWarnings(set.seed(1, sample.kind="Rounding"))
models <- replicate(n, {
  i <- suppressWarnings(createDataPartition(dat$y, times = 1, p= .5, list = F))
  test <- dat[i,]
  train <- dat[-i,]
  fit <- lm(y~x,data = train)
  y_hat <- predict(fit, test)
  RMSE <- sqrt(sum((y_hat - test$y)^2)/length(y_hat))
})
mean(models)
sd(models)

# Function that does this for any size n
nlmRMSE <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  models <- replicate(100, {
    i <- suppressWarnings(createDataPartition(dat$y, times = 1, p= .5, list = F))
    test <- dat[i,]
    train <- dat[-i,]
    fit <- lm(y~x,data = train)
    y_hat <- predict(fit, test)
    RMSE <- sqrt(sum((y_hat - test$y)^2)/length(y_hat))
  })
  tibble(mean = mean(models), sd = sd(models), n = n)
}

n <- c(100, 500, 1000, 5000, 10000)
suppressWarnings(set.seed(1, sample.kind="Rounding"))
sapply(n, nlmRMSE)

# making correlation larger
suppressWarnings(set.seed(1, sample.kind="Rounding"))
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

suppressWarnings(set.seed(1, sample.kind="Rounding"))
models <- replicate(100, {
  i <- suppressWarnings(createDataPartition(dat$y, times = 1, p= .5, list = F))
  test <- dat[i,]
  train <- dat[-i,]
  fit <- lm(y~x,data = train)
  y_hat <- predict(fit, test)
  RMSE <- sqrt(sum((y_hat - test$y)^2)/length(y_hat))
})
tibble(mean = mean(models), sd = sd(models), n = n)


# New dataset w/two predictors
suppressWarnings(set.seed(1, sample.kind="Rounding"))
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

i <- suppressWarnings(createDataPartition(dat$y, times = 1, p= .5, list = F))
test <- dat[-i,]
train <- dat[i,]
RMSE <- function(f){
  suppressWarnings(set.seed(1, sample.kind="Rounding"))
  fit <- lm(f,data = train)
  y_hat <- predict(fit, test)
  sqrt(sum((y_hat - test$y)^2)/length(y_hat)) 
}

RMSE(y~x_1)
RMSE(y~x_2)
suppressWarnings(set.seed(1, sample.kind="Rounding"))
RMSE(y~x_1+x_2)

# x_1, and x_2 are highly correlated
suppressWarnings(set.seed(1, sample.kind="Rounding"))
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
i <- suppressWarnings(createDataPartition(dat$y, times = 1, p= .5, list = F))
test <- dat[-i,]
train <- dat[i,]
RMSE(y~x_2)
