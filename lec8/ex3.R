estimate_pi <- function(num_points) {
  points <- matrix(runif(num_points * 2), ncol = 2)  # Generate random points in a 2D space
  
  # Count the number of points that lie within the unit circle
  inside_circle <- sum(sqrt(points[, 1]^2 + points[, 2]^2) < 1)
  
  # Calculate the proportion of points inside the circle
  proportion <- inside_circle / num_points
  
  # Estimate the value of Pi
  pi_estimate <- 4 * proportion
  
  return(pi_estimate)
}

# Number of random points
num_points <- 1000000

# Estimate Pi using the Monte Carlo method
pi_estimate <- estimate_pi(num_points)

print(paste("Estimated value of Pi:", pi_estimate))
