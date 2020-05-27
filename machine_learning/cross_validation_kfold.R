library(caret)
library(tidyverse)
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results
# 
# install.packages("BiocManager")
# BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
ind <- which(pvals < .01)

x_subset <- x[,ind]

fit <- train(x_subset, y, method = "glm")
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


library(dslabs)
data("tissue_gene_expression")
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn",
             tuneGrid = data.frame(k = seq(1,7,2)))
ggplot(fit)

# ------------------------------Bootstrap-------------------------------------
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

set.seed(1995)
#use set.seed(1995, sample.kind="Rounding") instead if using R 3.6 or later
N <- 250
X <- sample(income, N)
M<- median(X)
M

library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)

B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()

quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

mean(M) + 1.96 * sd(M) * c(-1,1)

mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)

# Ex
library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind="Rounding")
i <- createResample(mnist_27$train$y, 10)
i3 <- map_int(1:10, function(x){
  sum(i[[x]] == 3)
})
sum(i3)

set.seed(1995, sample.kind="Rounding")
quants <- replicate(10000, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(quants)
sd(quants)
qnorm(0.75)

set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding")
i <- createResample(y, 10000)
ysum <- map_df(1:10000, function(x){
  y_hat <- y[i[[x]]] 
  q <- quantile(y_hat, 0.75)
  data.frame(q)
})

mean(ysum$q)
sd(ysum$q)
