library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
set.seed(1995) # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]
y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

#distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
# image(as.matrix(d))

#order the distance by labels
# image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))
d_492 <- as.matrix(d)[492,]
# image(1:28, 1:28, matrix(d_492, 28, 28))

# Exercises
library(dslabs)
data(tissue_gene_expression)

sqrt(sum((tissue_gene_expression$x[1,] - tissue_gene_expression$x[73,])^2))
sqrt(sum((tissue_gene_expression$x[39,] - tissue_gene_expression$x[40,])^2))
sqrt(sum((tissue_gene_expression$x[73,] - tissue_gene_expression$x[74,])^2))

# Correct way
d <- dist(tissue_gene_expression$x)
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
image(as.matrix(d))
