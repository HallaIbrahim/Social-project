# Load your dataset from a .txt file
data <- read.table("D:\\downloads\\Email-EuAll.txt", header = FALSE)
colnames(data)  # To see the column names
str(data)       # To view the structure of the dataset

# Check the first few rows of the dataset
head(data)

# Identify the structure of the data to ensure it contains the necessary information for graph creation

# Attempt to create a graph
graph <- graph_from_data_frame(data, directed = TRUE, vertices = NULL)

# Check if 'graph' is created successfully
summary(graph)

# Assuming 'graph' is your created graph
# Basic network statistics
cat("Number of Nodes:", vcount(graph), "\n")

cat("Number of Edges:", ecount(graph), "\n")
cat("Network Density:", edge_density(graph), "\n")
cat("Average Degree:", mean(degree(graph)), "\n")

#1. Degree centrality
n_deg<-degree(graph ,mode=c("All"))
V(graph)$degree<-n_deg
V(graph)$degree
which.max(n_deg)
#1. Plotting a network with the degree centrality

# Plotting the graph based on node degrees
plot(graph, 
     edge.color = 'black',
     vertex.label.cex = 0.4,
     vertex.color = heat.colors(max(n_deg)),  # Color nodes based on degrees
     vertex.size = n_deg *0.1 ,  # Adjust the vertex size based on degrees
     edge.width = 1,                   # Adjust the edge width as needed
     layout = layout.fruchterman.reingold
)


in_degreee <- degree(graph, mode = "in")


hist(in_degreee, main = "In-degree Distribution", xlab = "In-degree", ylab = "Frequency")

# Plotting a histogram with adjusted parameters
hist(in_degreee, 
     col = "skyblue",        # Set color of bars
     border = "black",       # Set color of bar borders
     breaks = 30,            # Adjust the number of bins/breaks
     main = "Histogram",     # Title of the plot
     xlab = "X-axis label",  # Label for the x-axis
     ylab = "Frequency",     # Label for the y-axis
     xlim = c(0, 400),        # Set x-axis limits
     ylim = c(0, 40000),       # Set y-axis limits
     axes = TRUE             # Display axes (set to FALSE to hide)
)
out_degreee <- degree(graph, mode = "out")
# Plotting a histogram with adjusted parameters
hist(out_degreee, 
     col = "skyblue",        # Set color of bars
     border = "black",       # Set color of bar borders
     breaks = 30,            # Adjust the number of bins/breaks
     main = "Histogram",     # Title of the plot
     xlab = "X-axis label",  # Label for the x-axis
     ylab = "Frequency",     # Label for the y-axis
     xlim = c(0, 400),        # Set x-axis limits
     ylim = c(0, 40000),       # Set y-axis limits
     axes = TRUE             # Display axes (set to FALSE to hide)
)


# Get the total number of nodes
total_nodes <- vcount(graph)
# Randomly select 'sample_size' number of nodes from the graph

# Define the nodes you want to extract

nodes_to_extract <- 1:50

# Extract the subgraph containing nodes 1 to 20 and their connections
subgraph <- induced_subgraph(graph, nodes_to_extract)

# Ensure only nodes 1 to 20 are present in the subgraph
subgraph <- delete_vertices(subgraph, V(subgraph)[V(subgraph)$name > 50])

# Plot the refined subgraph
plot(subgraph, edge.arrow.size = 0.2,
     vertex.label.cex = 0.6,
     vertex.label = V(subgraph)$name,  # Display node labels
     main = "Nodes 1 to 50 with Connections")        # Add a main title to the plot

total_nodess <- vcount(subgraph)
# Calculate centrality measures on the subgraph
in_degree <- degree(subgraph, mode = "in")
hist(in_degree)


# Plotting a histogram with adjusted parameters
hist(in_degree, 
     col = "skyblue",        # Set color of bars
     border = "black",       # Set color of bar borders
     breaks = 30,            # Adjust the number of bins/breaks
     main = "Histogram",     # Title of the plot
     xlab = "X-axis label",  # Label for the x-axis
     ylab = "Frequency",     # Label for the y-axis
     xlim = c(0, 12),        # Set x-axis limits
     ylim = c(0, 80),       # Set y-axis limits
     axes = TRUE             # Display axes (set to FALSE to hide)
)


# Find the node(s) with maximum in-degree
max_in_degree_nodes <- which(in_degree == max(in_degree))

# Print or return the node(s) with maximum in-degree
print(paste("Node(s) with maximum in-degree:", max_in_degree_nodes))

out_degree <- degree(subgraph, mode = "out")

# Find the node(s) with maximum out-degree
max_out_degree_nodes <- which(out_degree == max(out_degree))

# Print or return the node(s) with maximum out-degree
print(paste("Node(s) with maximum in-degree:", max_out_degree_nodes))

#1. Degree centrality
n_degree<-degree(subgraph ,mode=c("All"))
V(subgraph)$degree<-n_degree
V(subgraph)$degree
which.max(n_degree)

# Plotting the graph based on node degrees
plot(subgraph, 
     edge.color = 'black',
     vertex.label.cex = 0.4,
     vertex.color = heat.colors(max(n_degree)),  # Color nodes based on degrees
     vertex.size = n_degree *5 ,  # Adjust the vertex size based on degrees
     edge.width = 0.5,                   # Adjust the edge width as needed
     edge.arrow.size = 0.2,
     layout = layout.fruchterman.reingold
)
# betweenness Centrality
betweenness_values <- betweenness(subgraph)

# Create a histogram for betweenness centrality
hist(betweenness_values,
     xlab = "Betweenness Centrality",
     main = "Betweenness Centrality Distribution",
     col = "skyblue",  
     
)
# Closeness Centrality
closeness_values <- closeness(subgraph)
clustering_coef <- transitivity(subgraph)

# Create a scatter plot for closeness centrality
plot(V(subgraph)$name, closeness_values,
     xlab = "Nodes",
     ylab = "Closeness Centrality",
     main = "Closeness Centrality of Nodes"
)
# Calculate eigenvector centrality
eigenvector_centrality <- eigen_centrality(subgraph)$vector
# Create a box plot for eigenvector centrality
boxplot(eigenvector_centrality,
        xlab = "Eigenvector Centrality",
        ylab = "Centrality Value", 
        main = "Eigenvector Centrality Distribution",
        col = "skyblue",                              # Box color
        border = "black"                          # Border color
)

cat("In-Degree Centrality:\n")
cat("Max In-Degree:", max(in_degree), "\n")
cat("Min In-Degree:", min(in_degree), "\n")
cat("Average In-Degree:", mean(in_degree), "\n")

cat("\nOut-Degree Centrality:\n")
cat("Max Out-Degree:", max(out_degree), "\n")
cat("Min Out-Degree:", min(out_degree), "\n")
cat("Average Out-Degree:", mean(out_degree), "\n")

cat("\nBetweenness Centrality:\n")
cat("Max Betweenness:", max(betweenness_values), "\n")
cat("Min Betweenness:", min(betweenness_values), "\n")
cat("Average Betweenness:", mean(betweenness_values), "\n")

cat("\nCloseness Centrality:\n")
cat("Max Closeness:", max(closeness_values), "\n")
cat("Min Closeness:", min(closeness_values), "\n")
cat("Average Closeness:", mean(closeness_values), "\n")

cat("\nClustering Coefficient:\n")
cat("Clustering Coefficient:", clustering_coef, "\n")



communities_infomap <- cluster_infomap(subgraph)
communities_infomap
# Shortest Paths
shortest_paths <- shortest.paths(subgraph)
max(shortest_paths)
# ... analyze shortest path information

#  plot in degree distribution
hist(in_degree, main = "In-degree Distribution", xlab = "In-degree", ylab = "Frequency")
#  plot out degree distribution
hist(out_degree, main = "out-degree Distribution", xlab = "out-degree", ylab = "Frequency")
# Calculate the density of the network
network_density <- edge_density(subgraph)

# Display network density
print(paste("Network Density:", network_density))
# we conclude that network debsity is very low and very few edges are exist 
#there are isolated nodes and lack of edges and information
# Modularity-based community detection
communities_infomap <- cluster_infomap(subgraph)

# Plot the graph with community colors
plot(communities_infomap, subgraph)
# Plot the sampled subgraph
plot(subgraph, vertex.label = V(subgraph)$name)

#data preprocessing
# Check connectivity and remove isolated nodes or disconnected components
graphh <- delete_vertices(subgraph, which(degree(subgraph, mode = "in") == 0 & degree(subgraph, mode = "out") == 0))
graphh
cat("Number of Nodes:", vcount(graphh), "\n")
# Modularity-based community detection
communities_infomap <- cluster_infomap(graphh)

# Generate layout for the graph
layout <- layout_with_fr(graphh)  # You can use a different layout algorithm if needed



# Plot the graph with adjusted size, community colors, and node labels
plot(communities_infomap, graphh, layout = layout,
     vertex.label = V(graphh)$name,        
     vertex.label.cex = 0.4,               
     vertex.color = communities_infomap$membership,
     edge.arrow.size = 0.2,
     main = "Community Structure Plot",
     asp = 1,  # Set the aspect ratio to 0 for automatic adjustment
     rescale = TRUE) 
# Extract the largest connected component
largest_component <- induced_subgraph(subgraph, which.max(sizes(clusters(subgraph))))
# Generate layout for the largest component
layoutt <- layout_with_fr(largest_component)  # You can use a different layout algorithm if needed

# Plot the largest component
plot(largest_component, layout = layoutt,
     vertex.label = V(largest_component)$name,  # Display node labels (if available)
     vertex.label.cex = 0.8,                    # Adjust label size if needed
     main = "Largest Connected Component Plot")  # Add a main title to the plot

# Find nodes with in/out degree 1 in the subgraph
low_connectivity_nodes <- which(degree(subgraph, mode = "in") == 1 & degree(subgraph, mode = "out") == 1)

# Check if edge weights exist
if ("weight" %in% edge_attr_names(graphh)) {
  print("Edge weights are present.")
} else {
  print("No edge weights found.")
}







