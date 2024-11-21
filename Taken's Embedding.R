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

# Determine the number of columns and number of rows for the Takens embedding
num_columns <- length(colnames(data[1:10]))
num_rows <- n - (d - 1) * tau

# Initialize a 3D array to store Takens embeddings for each column
Takens_results <- array(NA, dim = c(num_rows, d, num_columns))

# Loop over each column in the data
for (col_idx in seq_along(colnames(data[1:10]))) {
  
  # Extract the current column
  embeding_data <- data[[colnames(data[1:10])[col_idx]]]
  
  # Convert to numeric (if necessary)
  ts <- as.numeric(embeding_data)
  
  # Takens embedding
  Takens <- t(purrr::map_dfc(1:(n - (d - 1) * tau), ~ ts[seq(from = .x, by = tau, length.out = d)]))
  
  # Store the Takens embedding in the 3D array
  Takens_results[,,col_idx] <- as.matrix(Takens)
  
  # Plot the Takens embedding
  plot(Takens, xlab = "x1", ylab = "x2", main = paste("Takens Embedding for Column", col_idx))
  
  # Apply ripsDiag to the Takens embedding (or any other analysis)
  diag <- ripsDiag(Takens, maxdimension = 1, maxscale = max(dist(Takens)))
  
  # Plot the persistence diagram
  plot(diag$diagram, main = paste("Persistence Diagram for Column", col_idx))
  
  # Optionally, plot the time series itself
  ts.plot(ts, main = paste("Time Series for Column", col_idx))
}

# Accessing Takens embedding for a specific column
print(Takens_results[,,2]) # Example: Takens embedding for the second column