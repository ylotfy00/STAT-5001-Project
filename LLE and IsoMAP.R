if (!require("rgl")) install.packages("rgl")
library(rgl)
if (!require("RDRToolbox")) install.packages("RDRToolbox")
library(RDRToolbox)
if (!require("lle")) install.packages("lle")
library(lle)

### LLE and IsoMap

#Check tommorow how to mmkae it work

X = PPP
X=X[,1:16]
X=as.matrix(X)
X = X[-outlier_index, ]

#best k is k = 9 best dimension = 2
for (k in 5:10){
  X_IM = Isomap(X,dims=1:3,k,plotResiduals=TRUE,verbose=FALSE)
  title(main = paste("Isomap Residual Variance Plot for k =",k))
  plot(X_IM$dim2,pch=1,xlab="Coordinate 1", ylab="Coordinate 2",
       main=paste("2D Isomap Coordinates for k = ",k))
}


for(t in 2:5){
  calc_k(X,t,kmin=1,kmax=10,plotres=TRUE)
  title(main = "LLE Residual Variance Plot for Intrinsic Dimension t= ",t)
}
#for 2d use 8 for 3d use 10
k = 8
X_LLE = lle(X,2,k)
plot(X_LLE$Y,pch=1,xlab="Coordinate 1", ylab="Coordinate 2",
     main=paste("2D LLE Coordinates with k = ",k))