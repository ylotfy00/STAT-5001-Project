if (!require("rgl")) install.packages("rgl")
library(rgl)
if (!require("RDRToolbox")) install.packages("RDRToolbox")
library(RDRToolbox)
install.packages("/Users/yousseflotfy/Documents/Missouri S&T/Fall 2024/STAT 5001 TDA/spe_1.1.2.tar", repos = NULL, type = "source")
library('spe')
install.packages("remotes")
#remotes::install_github("kcf-jackson/spe")
remotes::install_github("kcf-jackson/maniTools")
library(maniTools)
if (!require("diffusionMap")) install.packages("diffusionMap")
library(diffusionMap)

set.seed(12345)
PPP = read.csv('PPP data Encoded.csv')    
X = PPP
X=as.matrix(X)
X = X[-outlier_index, ]
X <- scale(X)

#Laplacian Eigenmap
LE_data <- Laplacian_Eigenmaps(X,k=4,d=2)
plot(LE_data$eigenvectors,main="PPP Laplacian Eigenmap Coordinates")

#embedding does not perform too well

## Diffusion Map
dmap = diffuse(dist(X),neigen=2)
plot(dmap$X,main="PPP Difussion Map Coordinates")

##Hessian Eigenmaps
HLLE_data <- Hessian_LLE(X,k=8,d=2)
plot(HLLE_data$projection,main="Swiss Roll Hessian Eigenmap Coordinates")