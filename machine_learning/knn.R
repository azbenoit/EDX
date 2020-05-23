#logistic regression
library(caret)
library(dslabs)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]


# Heigths data set
set.seed(1, sample.kind = "Rounding")
data("heights")
heights 

y <- heights$sex 
i <- createDataPartition(y, times = 1, p = .5, list = F)
train <- heights[-i,]
test <- heights[i,]
ks <- seq(1, 101, 3)
accuracy <- map_df(ks, function(k){
  fit <- knn3(sex~., data = train, k = k)
  y_hat <- predict(fit, test, type = "class")
  f1 <- F_meas(data = y_hat, reference = factor(test$sex))
  tibble(f1 = f1, k = k)
  })
which.max(accuracy$f1)
accuracy[which.max(accuracy$f1),]
plot(accuracy$k, accuracy$f1)


# Tissue data set
library(dslabs)
data("tissue_gene_expression")

set.seed(1, sample.kind = "Rounding")
i <- createDataPartition(tissue_gene_expression$y, list = F)
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
train_x <- x[-i,]
train_y <- y[-i]
test_x <- x[i,]
test_y <- y[i]
k <- c(1, 3, 5, 7, 9, 11)
accuracy <- map_df(k, function(k){
  fit <- knn3(train_x, train_y, k = k)
  y_hat <- predict(fit, test_x, type = "class")
  acu <- mean(y_hat == test_y)
  tibble(acu = acu, k = k)
})
accuracy
