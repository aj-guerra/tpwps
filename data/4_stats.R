# Load required packages
library(igraph)
library(tidyverse)
library(ggraph)
library(viridis)
library(readr)
library(stats)
library(migraph)
library(broom)
library(factoextra)

major_stats <- tibble(readRDS('data/subgraphs/all_major_stats.rds'))

major_stats 

pca_ms <- major_stats %>% 
   tibble() %>% 
   column_to_rownames('major') %>%
   select(-c(degree_type, college, avg_indegree, avg_outdegree)) %>% 
   prcomp(scale=TRUE,
          rank. = 2) 

graph_pca <- pca_ms %>% 
   broom::augment(major_stats)

# identify ideal number of clusters
# fviz_nbclust(graph_pca %>% select(.fittedPC1, .fittedPC2), kmeans, method='wss') +
#    theme_minimal()
# 
# #cluster graph_pca based on .fittedPC1 and .fittedPC2
# kmeans_pca <- kmeans(graph_pca %>% select(.fittedPC1, .fittedPC2), 
#                      centers=4)

# centroid of bs and ab
centroids <- graph_pca %>%
   group_by(degree_type) %>%
   summarize(meanx=mean(.fittedPC1), meany=mean(.fittedPC2)) %>% 
   column_to_rownames('degree_type')

ce <- graph_pca %>% 
   filter(college == 'CE')

summary(lm(.fittedPC2 ~ .fittedPC1, data=ce))

cbs <- graph_pca %>% 
   filter(college == 'CBS')

summary(lm(.fittedPC2 ~ .fittedPC1, data=cbs))

caes <- graph_pca %>% 
   filter(college=='CAES')

summary(lm(.fittedPC2 ~ .fittedPC1, data=caes))

no_cls <- graph_pca %>% 
   filter(college!='CLS')

graph_pca %>% 
   group_by(college) %>% 
   summarize(pc1m=mean(.fittedPC1), pc1sd = sd(.fittedPC1))

# plot the clusters
# graph_pca %>% 
#    mutate(cluster=kmeans_pca$cluster) %>% 
#    ggplot(aes(.fittedPC1, .fittedPC2, color=as.factor(cluster)))+
   # geom_point()+
   # geom_point(data=centroids, aes(meanx, meany), color='black', size=5)+
#    geom_text(aes(label=major),
#              size=1.2)

#lines of best fit
ggplot(data = graph_pca, aes(.fittedPC1, .fittedPC2, color = college)) +
   geom_point(data = graph_pca, aes(.fittedPC1, .fittedPC2, color = college), size = 0.1) +
   geom_smooth(data = graph_pca, aes(.fittedPC1, .fittedPC2, color = college, group = college), 
               method = "lm", se = TRUE) 

ggplot(data=no_cls, aes(.fittedPC1, .fittedPC2, color=college))+
   geom_point(data=no_cls, aes(.fittedPC1, .fittedPC2, color=college), size = 1)
   # geom_text(aes(label=major),
   #           size=2.5)

ggplot(data=caes, aes(.fittedPC1, .fittedPC2, color=college))+
   geom_point(data=graph_pca, aes(.fittedPC1, .fittedPC2, color=college), size = 0.1)+
   # geom_point(data=centroids, aes(meanx, meany), color='black', size=5)+
   geom_text(aes(label=major),
             size=2.5)

ggplot(data=ce_cbs, aes(.fittedPC1, .fittedPC2, color=college))+
   geom_point(data=graph_pca, aes(.fittedPC1, .fittedPC2, color=college), size = 0.1)+
   # geom_point(data=centroids, aes(meanx, meany), color='black', size=5)+
   geom_text(aes(label=major),
             size=2.5)

ggplot(data=graph_pca, aes(.fittedPC1, .fittedPC2, color=college))+
   # geom_point()+
   # geom_point(data=centroids, aes(meanx, meany), color='black', size=5)+
   geom_text(aes(label=major),
             size=2.5)
