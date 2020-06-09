# To load the edx and verification datasets for the first time from scratch
if(!exists("edx") & !exists("verification")){source("~/Projects/edx/movielens_project/load_dataset.R", echo = T)}
# Note: location of the file may be different across different devices

# ---------------------------GOAL RMSE < 0.86490 ----------------------------------

options(digits = 7)
# Load libraries
library(tidyverse)
library(caret)

# Splitting edx set into test and training set
i <- createDataPartition(edx$rating, times = 1, p = .1, list = F)
train <- edx[-i,]
test <- edx[i,]
# Making sure all the movies and users in the test set are also in the train set
test <- test %>% semi_join(train, by = "movieId") %>% 
  semi_join(train, by = "userId")

# Writing a function to compute the Root Mean Squared Error of a model (RMSE)
RMSE <- function(actual_rating, predicted_rating){
  sqrt(mean((actual_rating - predicted_rating)^2))
}

#Using a guessing model as a baseline (Y_u,i = mu + epsilon_u,i)
mu <- mean(train$rating)
rmse_guess <- RMSE(test$rating, mu)



#Model using movie effects (b_i), and user effects (b_u) (Y_u,i = mu + b_i + b_u):

# Movie effect: can be found by taking the residual from the avg rating for each movie
# Better than avg movies will have positive residuals, while worse than avg movies will have negative residuals
b_i <- train %>% group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

# In order to test the model this effect can then be incorporated into the train and test data frame as such: 
train <- train  %>% left_join(b_i, by = "movieId")
test <- test  %>% left_join(b_i, by = "movieId")
# Movie Id RMSE
RMSE(test$rating, mu + test$b_i)

# User effect can be found similarily
b_u <- train %>% group_by(userId) %>% 
  summarise(b_u = mean(rating - mu - b_i))
train <- train  %>% left_join(b_u, by = "userId")
test <- test  %>% left_join(b_u, by = "userId")
# User Id RMSE
RMSE(test$rating, mu + test$b_u)
# User + Movie RMSE
RMSE(test$rating, mu + test$b_u + test$b_i)


# Regularizing the movie and user effect:

# Finding best lambda
# Define functions to find rmse given specific lambdas:

# Used to find optimal lambda_i
reg_RMSE_b_i <- function(l_i){
  b_i_reg <- train %>% group_by(movieId) %>% 
    summarise(b_i_reg = sum(rating - mu)/(n() + l_i))
  test_r <- test %>% left_join(b_i_reg, by = "movieId")
  data.frame(rmse =  RMSE(test_r$rating, mu + test_r$b_i_reg), l_i = l_i)
}

# Used to find optimal lambda_u, using lambda_i
reg_RMSE <- function(l_i, l_u){
  b_i_reg <- train %>% group_by(movieId) %>% 
    summarise(b_i_reg = sum(rating - mu)/(n() + l_i))
  test_r <- test %>% left_join(b_i_reg, by = "movieId")
  train_r <- train %>% left_join(b_i_reg, by = "movieId")
  b_u_reg <- train_r %>% group_by(userId) %>% 
    summarise(b_u_reg = sum(rating - mu - b_i_reg)/(n() + l_u))
  test_r <- test_r %>% left_join(b_u_reg, by = "userId")
  data.frame(rmse =  RMSE(test_r$rating, mu + test_r$b_i_reg + test_r$b_u_reg), l_i = l_i , l_u = l_u)
}

# Test out lambdas 1-20 for lambda_i
lambdas <- 1:20
reg_rmses_b_i <- map_df(lambdas, function(x) reg_RMSE_b_i(x))
plot(reg_rmses$l, reg_rmses$rmse) #plot results
reg_rmses_b_i[which.min(reg_rmses_b_i$rmse),] #Best result (l = 2)

# Narrow down l_i 
lambdas <- seq(1,3,.1)
reg_rmses_b_i <- map_df(lambdas, function(x) reg_RMSE_b_i(x))
plot(reg_rmses_b_i$l_i, reg_rmses_b_i$rmse) #plot results
reg_rmses_b_i[which.min(reg_rmses_b_i$rmse),] #Best result (l_i = 1.8)
l_i <- reg_rmses_b_i[which.min(reg_rmses_b_i$rmse),]$l_i

# Test out lambdas 1:20 for lambda_u
lambdas <- 1:20
reg_rmses <-  map_df(lambdas, function(x) reg_RMSE(l_i, x))
plot(reg_rmses$l_u, reg_rmses$rmse) #plot results
reg_rmses[which.min(reg_rmses$rmse),] #Best result (l_u = 5)

# Narrow down l_u
lambdas <- seq(4,6, by = .1)
reg_rmses <-  map_df(lambdas, function(x) reg_RMSE(l_i, x))
plot(reg_rmses$l_u, reg_rmses$rmse) #plot results
reg_rmses[which.min(reg_rmses$rmse),] #Best result (l_u = 4.7) (rmse = 0.8654892)
l_u <- reg_rmses[which.min(reg_rmses$rmse),]$l_u

reg_RMSE(l_i, l_u)



