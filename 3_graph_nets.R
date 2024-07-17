# Load required packages
library(igraph)
library(ggraph)
library(viridis)
library(readr)
library(migraph)

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

subgraph_stats <- data.frame(major = character(),
                             avg_degree = numeric(),
                             graph_density = numeric(),
                             n = integer(),
                             m = integer())

# Main function to automate the process for each RDS file in the input folder
subgraph_creation <- function(rds_folder, coursenet, output_folder) {
   rds_files <- list.files(rds_folder, pattern = "\\.rds$", full.names = TRUE)
   
   for (rds_path in rds_files) {
      courses <- readRDS(rds_path)
      
      # Extract the major name from the file name
      major_name <- tools::file_path_sans_ext(basename(rds_path))
      
      # Create and save the subgraph
      subgraph <- induced_subgraph(coursenet, vids = V(coursenet)[name %in% courses])
      output_sg_path <- file.path(graph_out, paste0(major_name, ".rds"))
      saveRDS(subgraph, output_sg_path)
      
      # Create and save the subgraph plot
      # create_subgraph_plot(subgraph, major_name, image_out, graph_out)
      
      # Add to the list of subgraphs
      sub_stats <- data.frame(major = major_name,
                              avg_degree = mean(degree(subgraph)),
                              graph_density = edge_density(subgraph),
                              n = vcount(subgraph),
                              m = ecount(subgraph),
                              net_diameter = as.numeric(migraph::network_diameter(subgraph)),
                              net_components = as.numeric(migraph::network_components(subgraph)),
                              net_cohesion = as.numeric(migraph::network_cohesion(subgraph)),
                              net_adhesion = as.numeric(migraph::network_adhesion(subgraph)),
                              net_degree = as.numeric(migraph::network_degree(subgraph)),
                              net_betweenness = as.numeric(migraph::network_betweenness(subgraph)),
                              net_eigenvector = as.numeric(migraph::network_eigenvector(subgraph)),
                              # net_core = migraph::network_core(subgraph),
                              net_reciprocity = as.numeric(migraph::network_reciprocity(subgraph)),
                              net_transitivity = as.numeric(migraph::network_transitivity(subgraph)),
                              net_assortativity = as.numeric(migraph::network_assortativity(subgraph))
                              )
      subgraph_stats <- rbind(subgraph_stats, sub_stats)
      output_stats_path <- file.path(stats_out)
      saveRDS(subgraph_stats, output_stats_path)
   }
}

# Example usage
rds_folder <- "ucdavis_courses"
graph_out <- "ucdavis_subgraphs/graphs"
image_out <- "ucdavis_subgraphs/images"
stats_out <- "ucdavis_subgraphs/all_major_stats.rds"

coursenet <- readRDS('coursenet.rds')

# Generate the sequences for UWP102A through UWP102L and UWP104A through UWP104J
uwp102 <- c('UWP101', 'UWP101V', 'UWP101Y', paste0('UWP102', LETTERS[1:12]))
uwp104 <- c(paste0('UWP104', LETTERS[1:10]), 'UWP104AV', 'UWP104AY', 'UWP104FV', 'UWP104FY', 'UWP104T')

# Combine with the existing list
en_req <- c(uwp102, uwp104)

coursenet <- delete_vertices(coursenet, V(coursenet)[name %in% en_req])

subgraph_creation(rds_folder, coursenet)

# Load the saved subgraph statistics
subgraph_stats <- tibble(readRDS(stats_out))
subgraph_stats %>% arrange(desc(avg_degree)) 

corr_subgraph <- subgraph_stats %>% select(-major)

ggcorrplot(cor(corr_subgraph), lab = TRUE, method = "square", type = "lower", tl.cex = 0.7)

