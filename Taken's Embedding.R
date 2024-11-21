########################################################
# Persistent Homology for Time Series                  #
########################################################

data = read.csv("data1.csv")
########################################################
# Takens Embedding                                     #
########################################################
# Embedding parameters
n = 500
d = 2
tau = 10  # You can adjust tau based on your needs

# Initialize an empty list to store Takens embeddings for each column
Takens_results <- list()

# Loop over each column in the data
for (col_name in colnames(data[1:10])) {
  
  # Extract the current column
  embeding_data = data[[col_name]]
  
  # Convert to numeric (if necessary)
  ts = as.numeric(embeding_data)
  
  # Takens embedding
  Takens = t(purrr::map_dfc(1:(n-(d-1)*tau), ~ ts[seq(from = .x, by = tau, length.out = d)]))
  
  # Store the Takens embedding for the current column
  Takens_results[[col_name]] = Takens
  
  # Plot the Takens embedding
  plot(Takens, xlab = "x1", ylab = "x2", main = paste("Takens Embedding for", col_name))
  
  # Apply ripsDiag to the Takens embedding (or any other analysis)
  diag = ripsDiag(Takens, maxdimension = 1, maxscale = max(dist(Takens)))
  
  # Plot the persistence diagram
  plot(diag$diagram, main = paste("Persistence Diagram for", col_name))
  
  # Optionally, plot the time series itself
  ts.plot(ts, main = paste("Time Series for", col_name))
}

# Optionally, inspect the results for any specific column (e.g., the first column)
print(Takens_results[[2]])