library(dslabs)
library(tidyverse)
library(caret)

data("movielens")
top <- movielens %>%
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>%
  top_n(50, n) %>%
  pull(movieId)

x <- movielens %>% 
  filter(movieId %in% top) %>%
  group_by(userId) %>%
  filter(n() >= 25) %>%
  ungroup() %>% 
  select(title, userId, rating) %>%
  spread(userId, rating)

row_names <- str_remove(x$title, ": Episode") %>% str_trunc(20)
x <- x[,-1] %>% as.matrix()
x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- row_names

#  find out if there are clusters of movies based on the ratings from 139 movie raters.
# first step is to find the distance between each pair of movies
d <- dist(x)

# Hierarchichal Clustering
h <- hclust(d)
plot(h, cex = 0.65, main = "", xlab = "")

groups <- cutree(h, k = 10)
data.frame(groups) %>% group_by(groups) %>% summarise(n())

# Kmeans
x_0 <- x
x_0[is.na(x_0)] <- 0
k <- kmeans(x_0, centers = 10)
k <- kmeans(x_0, centers = 10, nstart = 25)
groups <- k$cluster

# Heatmaps
# The first step is compute:
data("tissue_gene_expression")
x <- sweep(tissue_gene_expression$x, 2, colMeans(tissue_gene_expression$x), FUN = "-") #centers the data at 0
h_1 <- hclust(dist(x))
h_2 <- hclust(dist(t(x)))

# Now we can use the results of this clustering to order the rows and columns.
image(x[h_1$order, h_2$order])
# heatmap function that does it for us:
heatmap(x, col = RColorBrewer::brewer.pal(11, "Spectral"))

# Filering: include only the features with high variance.
library(matrixStats)
sds <- colSds(x, na.rm = TRUE)
o <- order(sds, decreasing = TRUE)[1:25]
heatmap(x[,o], col = RColorBrewer::brewer.pal(11, "Spectral"))
