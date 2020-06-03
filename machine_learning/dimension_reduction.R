library(dslabs)
library(tidyverse)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

dat <- data.frame(y = tissue_gene_expression$y,
                  x1 = tissue_gene_expression$x[,1],
                  x2 = tissue_gene_expression$x[,2])

dat %>% ggplot(aes(x1,x2, color = y)) +
  geom_point()
