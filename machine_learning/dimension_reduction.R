library(dslabs)
library(tidyverse)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

sds <- colSds(tissue_gene_expression$x)
i1 <- which.max(sds)
i2 <- which(sds %in% top_n(data.frame(sds), 2)$sds & sds != max(sds))



pca <- prcomp(tissue_gene_expression$x)
q <- pca$rotation
p<- pca$x

dat <- data.frame(y = tissue_gene_expression$y,
                  x1 = p[,1],
                  x2 = p[,2])

dat %>% ggplot(aes(x1,x2, color = y)) +
  geom_point()

row_avg <- rowMeans(tissue_gene_expression$x)

data.frame(row_avg, pc_1 = p[,1], y = tissue_gene_expression$y) %>% 
  ggplot(aes(pc_1, row_avg, color = y)) +
  geom_point()
cor(row_avg, p[,1])


# Redoing PCA with centered data
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pca <- prcomp(x)
data.frame(pc_1 = pca$x[,1], pc_2 = pca$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# par(mfrow=c(1,1))
data.frame(pca$x[,1:10], y = tissue_gene_expression$y) %>% pivot_longer(cols = -y,names_to = "pc_num", values_to = "value") %>% 
  ggplot(aes(pc_num, value, color = y)) +
  geom_boxplot()

# Variance explained by each pc
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)
