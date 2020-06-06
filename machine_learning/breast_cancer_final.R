options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

x <- brca$x
y <- brca$y

mean(y == "M")
col_means <- colMeans(x)
which.max(col_means)
col_sds <- colSds(x)
which.min(col_sds)

# Scaling the matrix
x <- sweep(x ,2, colMeans(x))
x <- sweep(x, 2, colSds(x), "/")
median(x[,1])

# Distance
dy_hat <- dist(x)
dm <- as.matrix(dy_hat)
d1 <- dm[1,]
i <- which(y == "B")
mean(d1[-i])
heatmap(as.matrix(dist(t(x))))

# Hierarchichal clustering
dx <- dist(t(x))
h <- hclust(dx)
plot(h, cex = .65)
groups <- cutree(h, k = 5)
split(names(groups), groups)

# PCA
pca <- prcomp(x)
variance <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(variance)
variance

# Plotting Principle components
# Scatterplot of first two:
data.frame(pca$x, y) %>% select(1,2,y) %>% 
  ggplot(aes(PC1, PC2, col = y)) +
  geom_point()
# Boxplot:
data.frame(pca$x[,1:10], y) %>% 
  pivot_longer(cols = -y, names_to = "PC_num", values_to = "value") %>% 
  ggplot(aes(PC_num, value, fill = y)) +
  geom_boxplot()

# Splitting into training and test sets
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x[test_index,]
test_y <- brca$y[test_index]
train_x <- x[-test_index,]
train_y <- brca$y[-test_index]
# Check that the training and test sets have similar proportions of benign and malignant tumors.
mean(train_y == "B")
mean(test_y == "B")

# K-means
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
k <- kmeans(train_x, centers = 2)
y_hat_kmeans <-  predict_kmeans(test_x, k)
y_hat_kmeans <- factor(ifelse(y_hat_kmeans == 1, "B", "M"), levels = levels(test_y))
mean(y_hat_kmeans == test_y)
confusionMatrix(data = y_hat_kmeans, reference = test_y)

#Define accuracy function
accuracy <- function(model, tune = NULL, ...){
  fit <- train(train_x, train_y, method = model, tuneGrid = tune, ...)
  y_hat <- predict(fit, test_x, type = "raw")
  list(accuracy = mean(y_hat == test_y), fit = fit, y_hat = y_hat)
}
# Logistic regression model
glm <- accuracy("glm")

# LDA and QDA
lda <- accuracy("lda")
qda <- accuracy("qda")

# Loess
set.seed(5, sample.kind = "Rounding") 
loess <- accuracy("gamLoess")

# Knn
set.seed(7, sample.kind = "Rounding") 
knn <- accuracy("knn", data.frame(k = seq(3,21,2)))

# Random Forest
set.seed(9, sample.kind = "Rounding") 
rf <- accuracy("rf", data.frame(mtry = c(3,5,7,9)))
rf$accuracy
rf$fit$bestTune
varImp(rf$fit)

# Creating an ensemble
# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Ensemble
y_hat_all <- as.matrix(data.frame(y_hat_kmeans, rf$y_hat, qda$y_hat, lda$y_hat, loess$y_hat, knn$y_hat, glm$y_hat))
y_hat_ens <- apply(y_hat_ens, 1, getmode)
mean(y_hat_ens == test_y)
accuracy_all <- apply(y_hat_all, 2, function(x) mean(x == test_y))
accuracy_all
