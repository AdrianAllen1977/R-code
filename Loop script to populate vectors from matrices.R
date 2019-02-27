# Create two tables
geneticDistances <- matrix(rnorm(n=100), nrow=10, ncol=10)
hostRelatedness <- matrix(rnorm(n=100), nrow=10, ncol=10)

# Initialise a table to store the paired values of these tables as columns
table <- data.frame("Genetic"=NA, "HostRelatedness"=NA)
row <- 0

# Fill the vector by selecting each value in the lower triangle
for(i in 1:nrow(geneticDistances)){
        for(j in 1:ncol(geneticDistances)){

                # Skip the upper triangle and self comparisons
                if(i >= j){
                        next
                }

                # Print where you currently are in table
                cat(paste("Current row = ", i, "\tCurrent column = ", j, "\n", sep=""$

                # Store current value
                row <- row + 1
                table[row, "Genetic"] <- geneticDistances[i, j]
                table[row, "HostRelatedness"] <- hostRelatedness[i, j]
        }
}
