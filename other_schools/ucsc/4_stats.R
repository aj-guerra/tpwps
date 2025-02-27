# Load required packages
library(igraph)
library(ggraph)
library(viridis)
library(readr)
library(stats)
library(migraph)
library(broom)

major_stats <- tibble(readRDS('ucsc/subgraphs/all_major_stats.rds')) %>% 
   filter(degree_type %in% c('bs', 'ba')) %>% 
   drop_na()

major_stats 

pca_ms <- major_stats %>% 
   tibble() %>% 
   column_to_rownames('major') %>%
   select(-degree_type) %>% 
   prcomp(scale=TRUE) 

graph_pca <- pca_ms %>% 
   broom::augment(major_stats)

ggplot(data=graph_pca, aes(.fittedPC1, .fittedPC2, color=degree_type))+
   geom_text(aes(label=major), 
             check_overlap = TRUE, 
             size=2.5) 
