
### Add this code to the clustering code to be able to run the clustering after scaling.
### Add it after the line to import the dataset

X = PPP
X$ProjectStatus = as.factor(X$ProjectStatus)  # Convert ProjectStatus to factor
X[, -which(names(X) == "ProjectStatus")] = scale(X[, -which(names(X) == "ProjectStatus")])
PPP = X
PPP= PPP[,1:16]

# Add this to the PCA code to try PCA after scaling. Add it after 'X=PPP'

X$ProjectStatus = as.factor(X$ProjectStatus)  # Convert ProjectStatus to factor
X[, -which(names(X) == "ProjectStatus")] = scale(X[, -which(names(X) == "ProjectStatus")])