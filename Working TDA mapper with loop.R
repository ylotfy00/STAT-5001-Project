if (!require("ggplot2")) install.packages("ggplot2") 
library(ggplot2)

if (!require(heplots)) install.packages("heplots");
library(heplots)

if (!require(ks)) install.packages("ks");
library(ks)
if (!require("TDAmapper")) install.packages("TDAmapper")
library(TDAmapper)

if (!require("rgl")) install.packages("rgl")
library(rgl)

if (!require("igraph")) install.packages("igraph")
library(igraph)

if (!require("fastcluster")) install.packages("fastcluster") 
library(fastcluster) 

if (!require("MASS")) install.packages("MASS")

library(MASS)


if (!require("fpc")) install.packages("fpc")

library(fpc)


if (!require("NbClust")) install.packages("NbClust")

library(NbClust)

if (!require("remotes")) install.packages("remotes")

if (!require("devtools")) install.packages("devtools")

if (!require("TDAmapper")) remotes::install_github("corybrunson/tdaunif")
library(tdaunif)

devtools::install_github("TianshuFeng/STA")
library(STA)


#TDA Mapper


X = PPP_full # Revert

X$ProjectStatus =as.factor(X$ProjectStatus)

x11(width = 16/2.54, height = 12/2.54)
plot_colors <- c("blue", "red", "orange")
pairs(X[,1:15],oma=c(3,3,8,3),col = plot_colors[(X$ProjectStatus)], gap = 0)
legend("top",col = plot_colors,legend = levels(X$ProjectStatus), pch = 1, 
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)

plot3d(X[,1:15],col= plot_colors[(X$ProjectStatus)],pch=1)
legend3d("topright", legend = unique(X$ProjectStatus),pch = 16, col = plot_colors, cex=0.75,
         inset=c(0.02))

num_cols <- ncol(X[,1:15])

X[,1:15] <- scale(X[,1:15],center=FALSE)
filter_function <-  kde(X[,1:15],H = diag(1, num_cols),eval.points = X[,1:15], binned = FALSE)$estimate

#Part B

#Add a for loop here

# Define percent overlap values and number of intervals
percent_overlaps <- c(40, 50, 60, 70)
num_intervals <- c(3, 5, 6, 7)

# Loop through combinations of percent overlaps and number of intervals
for (overlap in percent_overlaps) {
  for (interval in num_intervals) {
    
    # Print the current combination of overlap and intervals being tested
    print(paste("Testing percent overlap:", overlap, "%, and number of intervals:", interval))
    
    # Run the mapper.sta function with the current overlap and interval values
    Xmap <- mapper.sta(dat = X[, 1:15],
                       filter_values = filter_function,
                       num_intervals = interval,
                       NbClust_cluster_method = "single",
                       cluster_index = "ccc",
                       percent_overlap = overlap)
    
    # Visualize the map
    simple_visNet(Xmap, filter = filter_function, color_filter = FALSE, color_code = NULL, groups_ind = X[, 15])
  }
}


#Part C

for(i in 1:length(Xmap$points_in_vertex)){
  print(paste("Summaries of Data and Filter Value for Vertex",i))
  print(as.data.frame(table(X[c(Xmap$points_in_vertex[[i]]),5])))
  print(colMeans(X[c(Xmap$points_in_vertex[[i]]),1:4]))
  print(mean(filter_function[c(Xmap$points_in_vertex[[i]])]))
}
