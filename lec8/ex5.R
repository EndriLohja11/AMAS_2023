# Simulate 10^5 numbers from a standard normal distribution
numbers <- rnorm(10^5)

# Calculate the number of points lying more than 5 standard deviations away from 0
count <- sum(abs(numbers) > 5)

# Calculate the proportion
proportion <- count / length(numbers) * 100

print(paste("Proportion of points more than 5 standard deviations away:", proportion, "%"))
