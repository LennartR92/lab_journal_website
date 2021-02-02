library("tidyverse")
diamonds2 <- readRDS('~/OneDrive/R working directory/01_getting_started/diamonds2.rds')
diamonds2 %>% pivot_longer(cols = c("2008","2009"),
                            names_to = "year", values_to = "price")
diamonds3 <- readRDS('~/OneDrive/R working directory/01_getting_started/diamonds3.rds')
diamonds3 %>% pivot_wider(names_from = "dimension",
                          values_from = "measurement")
diamonds4 <- readRDS('~/OneDrive/R working directory/01_getting_started/diamonds4.rds')
diamonds4 %>% separate (col = "dim",
                       into = c("x","y","z"),
                       sep = "/",
                       convert = TRUE)
diamonds5 <- readRDS('~/OneDrive/R working directory/01_getting_started/diamonds5.rds')
diamonds5 %>% unite(col = "clarity",
                    c("clarity_prefix","clarity_suffix"),
                    sep = "")
library(ggplot2)
library(dplyr)
diamonds %>% filter(cut == "Ideal" | cut == "Premium", carat >= 0.23) %>% slice(4:5)
