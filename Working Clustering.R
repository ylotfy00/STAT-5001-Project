if (!require("MASS")) install.packages("MASS")

library(MASS)


if (!require("fpc")) install.packages("fpc")

library(fpc)


if (!require("NbClust")) install.packages("NbClust")

library(NbClust)

PPP = read.csv('PPP data Encoded.csv')

set.seed(12345)

kmeans_ccc <- NbClust(PPP,distance ="euclidean",method ="kmeans",min.nc=2,max.nc=15,index="ccc")

kmeans_PPP <- kmeans(PPP,as.numeric(kmeans_ccc$Best.nc[1]),nstart=100)
plot(PPP,col=kmeans_PPP$cluster,pch=clusym[kmeans_PPP$cluster])

opar <- par(mfrow=c(1,2), mex=0.6, mar=c(5,4,3,2)+.3)

#To check groupings

PPP[,1:5][which(kmeans_PPP$cluster =='1'),]
PPP[,1:5][which(kmeans_PPP$cluster =='2'),]
PPP[,1:5][which(kmeans_PPP$cluster =='3'),]


#To access specific plots:

plot(PPP$TotalInvestment, PPP$GDP, 
     col = kmeans_PPP$cluster, 
     pch = clusym[kmeans_PPP$cluster],
     xlab = "Project Status", 
     ylab = "Country",
     main = "Cluster Visualization")

# Single Linkage
PPP_clusters <-  hclust(dist(PPP),method="single")
plot(PPP_clusters)
I_ccc <- NbClust(PPP,distance="euclidean",min.nc=2,max.nc=15,method="single",index="ccc")
PPP_clusters <- cutree(PPP_clusters,as.numeric(I_ccc$Best.nc[1]))
plot(PPP,col=takens_clusters,pch=clusym[PPP_clusters])