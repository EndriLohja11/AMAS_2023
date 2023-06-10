lottery_draw <- function() {
  numbers <- 1:49  # Create a vector of numbers from 1 to 49
  draw <- sample(numbers, size = 6, replace = FALSE)  # Select 6 unique numbers
  
  return(sort(draw))
}

# Simulate a lottery draw
lottery_numbers <- lottery_draw()

# Print the lottery numbers
cat("Lottery numbers:", lottery_numbers, "\n")
