# Load required packages
library(igraph)
library(ggraph)
library(viridis)
library(readr)
library(migraph)
library(tidyverse)

# Function to calculate network statistics and plot the subgraph
create_subgraph_plot <- function(subgraph, major_name, image_out, graph_out) {
   # Calculate centrality measures
   degree_centrality <- degree(subgraph)
   
   # Calculate network statistics
   avg_degree <- mean(degree_centrality)
   graph_density <- edge_density(subgraph)
   
   # Create the graph plot
   graph_plot <- ggraph(subgraph, layout = 'sugiyama') +
      geom_edge_link(color = "black", alpha = .5) +  # Adjust edge width based on weight
      geom_node_point(aes(size = degree_centrality, color = dept)) +  # Size nodes by degree centrality and color by dept
      geom_node_text(aes(label = name), size = 1.5, repel = TRUE) +
      scale_color_viridis_d() +  # Use a discrete color scale
      scale_size_continuous(range = c(2, 10)) +  # Adjust size range for nodes
      labs(title = paste(major_name, "Courses"),
           subtitle = paste("Average Degree:", round(avg_degree, 2), "| Density:", round(graph_density, 2))) +
      theme_graph()
   
   # Save the plot
   plot_path <- file.path(image_out, paste0(major_name, ".png"))
   ggsave(plot_path, graph_plot, width = 8, height = 8, dpi = 300, units = "in")
   
}

subgraph_stats <- data.frame()

# Main function to automate the process for each RDS file in the input folder
subgraph_creation <- function(rds_folder, coursenet) {
   rds_files <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE)
   
   for (rds_path in rds_files) {
      courses <- readRDS(rds_path)
      
      # Extract the major name from the file name
      major_name_full <- tools::file_path_sans_ext(basename(rds_path))
      major_name_short <- gsub('-bs$', '', major_name_full)
      major_name_short <- gsub('-ab$', '', major_name_full)
      
      # Create and save the subgraph
      major_subgraph <- induced_subgraph(coursenet, vids = V(coursenet)[name %in% courses])
      
      major_subgraph <- set_vertex_attr(major_subgraph, 
                                  "main_connected", 
                                  value  = components(major_subgraph)$membership == which.max(components(major_subgraph)$csize))
      
      output_sg_path <- file.path(graph_out, paste0(major_name_full, ".rds"))
      saveRDS(major_subgraph, output_sg_path)
      
      # Create and save the subgraph plot
      create_subgraph_plot(major_subgraph, 
                           major_name_full, 
                           image_out, 
                           graph_out)
      
      # Add to the list of subgraphs
      sub_stats <- data.frame(major = major_name_short,
                              
                              # average indegree and outdegree
                              avg_indegree = mean(igraph::degree(major_subgraph, mode = 'in')),
                              avg_outdegree = mean(igraph::degree(major_subgraph, mode = 'out')),
                              
                              # connectivity measures
                              graph_density = edge_density(major_subgraph),
                              per_in_main = as.numeric(mean(V(major_subgraph)$main_connected,
                                                            na.rm = TRUE)),
                              
                              # certain types of courses
                              standalone_courses = sum(igraph::degree(major_subgraph, mode = 'all') == 0),
                              entry_courses = sum(igraph::degree(major_subgraph, mode = 'in') == 0) - sum(igraph::degree(major_subgraph, mode = 'all') == 0),
                              capstone_courses = sum(igraph::degree(major_subgraph, mode = 'out') == 0) - sum(igraph::degree(major_subgraph, mode = 'all') == 0),
                              
                              
                              # count stats
                              n = vcount(major_subgraph),
                              m = ecount(major_subgraph),
                              depts = length(unique(V(major_subgraph)$dept)),
                              mean_units = major_metadata %>% 
                                 filter(major == major_name_full) %>% 
                                 pull(mean_units),
                              college = major_metadata %>% 
                                 filter(major == major_name_full) %>% 
                                 pull(college),
                              
                              # 
                              net_diameter = as.numeric(migraph::network_diameter(major_subgraph)),
                              
                              degree_type = as.factor(case_when(str_detect(major_name_full, '-bs$') ~ 'bs',
                                                                str_detect(major_name_full, '-ab$') ~ 'ab',
                                                                TRUE ~ 'other'))
                              )
      
      subgraph_stats <- rbind(subgraph_stats, sub_stats)
   }
   
   saveRDS(subgraph_stats, "data/subgraphs/all_major_stats.rds")
}

rds_folder <- "data/major_courses"
graph_out <- "data/subgraphs/graphs"
image_out <- "data/subgraphs/images"
stats_out <- "data/subgraphs/all_major_stats.rds"

coursenet <- readRDS('data/coursenet.rds')
major_metadata <- readRDS('data/major_metadata.rds')
# 
# # Generate the sequences for UWP102A through UWP102L and UWP104A through UWP104J
# lower_div <- c('UWP001', 'UWP001Y', 'UWP001V', 
#                'UWP048', 'UWP049', 'COM001', 
#                'COM002', 'COM003', 'COM004', 
#                'NAS005', 
#                'ENL003', 'ENL003V')
# uwp101 <- 'UWP101'
# uwp102 <- c('UWP101', 'UWP101V', 'UWP101Y', paste0('UWP102', LETTERS[1:12]))
# uwp104 <- c(paste0('UWP104', LETTERS[1:10]), 'UWP104AV', 'UWP104AY', 
#             'UWP104FV', 'UWP104FY', 'UWP104T')
# # 
# # # Combine with the existing list
# en_req <- c(lower_div, uwp101, uwp102, uwp104)
# # 
# # coursenet <- delete_vertices(coursenet, V(coursenet)[name %in% en_req])

subgraph_creation(rds_folder, coursenet)

