library(dslabs)
library(tidyverse)
library(caret)

set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

# Examine correlation between test scores
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Computing SVD
s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

# Compute the sum of squares of the columns of  𝑌  
ss_y <- apply(y, 2, function(x) sum(x^2))
# Then compute the sum of squares of columns of the transformed  𝑌𝑉  
yv <- y%*%s$v
ss_yv <- apply(yv, 2, function(x) sum(x^2))
sum(ss_y)
sum(ss_yv)

plot(1:length(ss_y),ss_y)
plot(1:length(ss_yv),ss_yv)
plot(s$d ,sqrt(ss_yv))

pca <- prcomp(y)
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)
diag(s$d)

# Computing UD without constructing diagonal matrix
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

# average (across all subjects) for each student should explain a lot of the variability.
avg_score <- rowMeans(y)
plot(-s$u[,1]*s$d[1], avg_score)
#  we see that the first column of  𝑈𝐷  is almost identical to the average score for each student except for the sign.
# This implies that multiplying  𝑌  by the first column of  𝑉  must be performing a similar operation to taking the average. 
image(s$v)
# The first column is very close to being a constant, which implies that the first column of YV is the sum of the rows of Y multiplied by some constant, and is thus proportional to an average.
# We can also see that the first three columns represent possible factors
# (1st is variablilty due to a student being an overall good student
# 2nd is variability connacting math and science
# 3rd is variability connecting the three subject groups)
