###  Manifold Learning / PCA

#Check the legend labels

X = PPP
X$ProjectStatus =as.factor(X$ProjectStatus)

x11(width = 16/2.54, height = 12/2.54)
plot_colors <- c("blue","red","orange")
pairs(X[,1:16],oma=c(3,3,8,3),col = plot_colors[as.numeric(X$ProjectStatus)], gap = 0)
legend("top",col = plot_colors,legend = levels(X$ProjectStatus), pch = 1, 
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)

pca_result = princomp(X[,1:16]);
summary(pca_result);
loadings(pca_result);
plot(pca_result,main="Scree Plot",type="lines",col="blue");

scores = pca_result$scores

plot(scores[,1:2],col= plot_colors[as.numeric((X$ProjectStatus))],pch=1)
legend("topright", legend = levels(X$ProjectStatus),pch = 16, col = plot_colors,cex=0.75,
       inset=c(0.02))

#############
# Find outlier

distances <- sqrt(scores[,1]^2 + scores[,2]^2)
reverse = rev(sort(distances))
reverse

outlier_index = which.max(distances)
outlier_index
print(X[outlier_index, ])

# Plot the outlier

plot(scores[,1:2], col = plot_colors[as.numeric(X$ProjectStatus)], pch = 1)
points(scores[outlier_index,1], scores[outlier_index,2], col = "black", pch = 19, cex = 2)  # Highlight

X = X[-outlier_index, ]

##############
##############

x11(width = 16/2.54, height = 12/2.54)
plot_colors <- c("blue","red","green", "orange")
pairs(X[,1:16],oma=c(3,3,8,3),col = plot_colors[as.numeric(X$ProjectStatus)], gap = 0)
legend("top",col = plot_colors,legend = levels(X$ProjectStatus), pch = 1, 
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)

pca_result = princomp(X[,1:16]);
summary(pca_result);
loadings(pca_result);
plot(pca_result,main="Scree Plot",type="lines",col="blue");

scores = pca_result$scores

## Better plot to zoom in

plot(scores[,1:2],col= plot_colors[as.numeric((X$ProjectStatus))],pch=1)
legend("topright", legend = levels(X$ProjectStatus),pch = 16, col = plot_colors,cex=0.75,
       inset=c(0.02))
install.packages("plotly")
library(plotly)
####
pca_data <- as.data.frame(scores[, 1:2])
kmeans_result = kmeans(pca_data, centers = 3)
pca_data$Cluster <- as.factor(kmeans_result$cluster)  # Add cluster labels

# Interactive scatter plot
p <- plot_ly(
  data = pca_data,
  x = ~Comp.1, y = ~Comp.2,
  color = ~Cluster,
  colors = c("blue", "red", "green", "orange"),
  type = 'scatter',
  mode = 'markers',
  text = ~paste("Cluster:", Cluster),  # Add hover text
  marker = list(size = 10, opacity = 0.7)
)

# Display the plot
p

plot3d(scores[,1:3],col= plot_colors[as.numeric((X$ProjectStatus))],pch=1)
legend3d("topright", legend = levels(X$ProjectStatus),pch = 16, col = plot_colors,cex=0.75,
         inset=c(0.02))

## Clustering after the PCA, which effectively does a dimensionality reduction

# Only use the first three components that have the most variability
pca_data = scores[, 1:3]

set.seed(12345)

kmeans_result = kmeans(pca_data, centers = 3)

print(kmeans_result$cluster)
plot(pca_data, col=kmeans_result$cluster, pch=clusym[kmeans_result$cluster])

# Pick out an interesting cluster to see the specific data

cluster_assignments <- kmeans_result$cluster

cluster_1 = X[cluster_assignments == 1, ]
cluster_2 = X[cluster_assignments == 2, ]
cluster_3 = X[cluster_assignments == 3, ]

status_counts_1= table(cluster_1$ProjectStatus)
status_counts_2 = table(cluster_2$ProjectStatus)
status_counts_3= table(cluster_3$ProjectStatus)

status_counts_1
status_counts_2
status_counts_3


test = X[cluster_assignments == 2, ]
test

for (cluster in unique(cluster_assignments)) {
  cat("\nCluster", cluster, "averages:\n")
  
  # Subset data for the current cluster
  cluster_data <- X[cluster_assignments == cluster, ]
  
  # Loop over each column
  for (col_name in colnames(cluster_data)) {
    # Check if the column is numeric
    if (is.numeric(cluster_data[[col_name]])) {
      avg <- mean(cluster_data[[col_name]], na.rm = TRUE)  # Calculate mean for numeric columns
      cat(col_name, ":", avg, "\n")
    } else {
      cat(col_name, ": Not numeric, skipped\n")  # Skip non-numeric columns
    }
  }
}


#cluster_2_data
