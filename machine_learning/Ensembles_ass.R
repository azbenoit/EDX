models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
set.seed(1, sample.kind = "Rounding")
data("mnist_27")

# Run all the modelsyes
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

y_hat <- sapply(fits, function(x){
  predict(x, mnist_27$test, type = "raw")})

accuracy_individual <- apply(y_hat, 2, function(x){
  mean(x == mnist_27$test$y)
})
# Other way: colMeans(pred == mnist_27$test$y)
mean(accuracy_individual)

# Ensemble
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modes <- apply(y_hat, 1, getmode)
accuracy_ensemble <- mean(modes == mnist_27$test$y)
accuracy_individual[accuracy_individual > accuracy_ensemble]

min_accuracy_estimates <- sapply(fits, function(x){
  min(x$results$Accuracy)    })
mean(min_accuracy_estimates)

# Better ensemble
i <- which(min_accuracy_estimates > .8)
y_hat_best <- y_hat[,i]
modes_best <- apply(y_hat_best, 1, getmode)
accuracy_ensemble_best <- mean(modes_best == mnist_27$test$y)
mean(accuracy_ensemble_best)
