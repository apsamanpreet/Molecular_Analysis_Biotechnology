library(dendextend)
library(circlize)

data <- rawdata[ ,-1]
rownames(data) <- c(rawdata$Samples)
# Perform hierarchical clustering
dist_matrix <- dist(data, method = "euclidean")  # Compute distance matrix
hc <- hclust(dist_matrix, method = "ward.D2")    # Perform hierarchical clustering
# "average" method for UPGMA
# Convert to dendrogram
dend <- as.dendrogram(hc)

# Customize dendrogram appearance
dend <- dend %>%
  set("branches_k_color", k = 6) %>%  # Color branches by 3 clusters
  set("branches_lwd", 1.5) %>%        # Set branch thickness
  set("labels_col", k = 6) %>%  # Color labels
  set("labels_cex", 1.2)              # Adjust label size

# Create circular dendrogram
circlize_dendrogram(dend, labels_track_height = 0.2, dend_track_height = 0.5)
