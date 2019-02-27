# Make an example table
a <- matrix(rnorm(n=100), nrow=100, ncol=100)
b <- matrix(rnorm(n=100), nrow=100, ncol=100)
c <- matrix(rnorm(n=100), nrow=100, ncol=100)

# Get values in upper triangle
values <- getUpperTriangle(a)
output <- getUpperTriangleOfMatrices(a, b, c)

#############
# FUNCTIONS #
#############

getUpperTriangleOfMatrices <- function(genetic, spatial, temporal){
  
  # Initialise a dataframe to store the values in the upper triangle from each matrix
  output <- data.frame("Genetic"=NA, "Spatial"=NA, "Temporal"=NA)
  row <- 0
  
  # Use nested loops to visit each entry in upper trianle
  for(i in 1:nrow(genetic)){
    for(j in 1:ncol(genetic)){
      
      # Ignore upper triangle and self comparisons
      if(i >= j){
        next
      }
      
      # Note progress
      #Sys.sleep(1) # Make computer sleep for 1 second
      #cat(paste("Current row =", i, "\tCurrent column =", j, "\n"))
      
      # Increment the row in the output dataframe
      row <- row + 1
      
      # Store values from upper triangles of matrices
      output[row, "Genetic"] <- genetic[i, j]
      output[row, "Spatial"] <- spatial[i, j]
      output[row, "Temporal"] <- temporal[i, j]
    }
  }
  
  return(output)
}

getUpperTriangle <- function(matrix){
  
  # Initialise a vector to store the values in the upper triangle
  vector <- c()
  
  # Use nested loops to visit each entry in upper trianle
  for(i in 1:nrow(matrix)){
    for(j in 1:ncol(matrix)){
      
      # Ignore upper triangle and self comparisons
      if(i >= j){
        next
      }
      
      # Note progress
      #Sys.sleep(1) # Make computer sleep for 1 second
      #cat(paste("Current row =", i, "\tCurrent column =", j, "\n"))
      
      # Store value
      vector[length(vector) + 1] <- matrix[i, j]
    }
  }
  
  return(vector)
}