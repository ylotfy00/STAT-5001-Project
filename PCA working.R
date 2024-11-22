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


plot3d(scores[,1:3],col= plot_colors[as.numeric((X$ProjectStatus))],pch=1)
legend3d("topright", legend = levels(X$ProjectStatus),pch = 16, col = plot_colors,cex=0.75,
         inset=c(0.02))