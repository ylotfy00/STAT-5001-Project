set.seed(5)


X <- PPP[,1:16]
#X <- as.data.frame(sample_ellipse(100,ar =1.0,sd = 0.01))
#plot(X)

maxscale <- 2 
maxdimension <- 1
Diag <- ripsDiag(X=X,maxdimension=maxdimension,maxscale=maxscale,
                 dist="euclidean",library ="Dionysus",printProgress=TRUE)
plot(Diag[["diagram"]],main="Persistence Diagram")
plot(Diag[["diagram"]],barcode=TRUE,main="Barcode")