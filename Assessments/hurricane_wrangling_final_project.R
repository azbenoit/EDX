library(tidyverse)
library(dslabs)
library(tidytext)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

# system2("open", args = fn)

txt <- pdf_text(fn)

x <- txt[9] %>% str_split("\n", simplify = F)
s <- x[[1]]
s <- str_trim(s)
s[1]

header_index <- str_which(s,"SEP.*2015.*2016")

header <- str_split(s[[header_index]], pattern = "\\s+", simplify = T)
month <- header[1]
header <- header[-1]

tail_index <- str_which(s, "Total")[1] 
n <- str_count(s, "\\d+")


s <- s[-1:-header_index] %>% .[-str_which(., "Total"):-length(.)] %>% 
  .[-which(str_count(., "\\d+") == 1)] 

# Better way
# out <- c(1:header_index, which(n==1), tail_index:length(s))
# s <- s[-out]

s <- str_remove_all(s, "[^\\d\\s]")

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5] 
s <- data.frame(s, stringsAsFactors = F)

header <- as.character(c("day", header))

tab <- s %>% setNames(header) %>% mutate_all(as.numeric) %>% 
  mutate(month = "SEP")

tab %>% filter(day >19) %>% summarise(avg = mean(.$"2017")) %>% .$avg

tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab %>% filter(year != 2018) %>% 
  ggplot(aes(day, deaths, color = year)) +
  geom_vline(xintercept = 20) +
  geom_line() +
  geom_point()