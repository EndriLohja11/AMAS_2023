library(deSolve)
library(ggplot2)

# Function defining the Lanchester combat model
lanchester_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dA <- r1 - k1 * B - b1 * A
    dB <- r2 - k2 * A - b2 * B
    
    return(list(c(dA, dB)))
  })
}

# Initial conditions
A0 <- 100  # Initial number of planes on side A
B0 <- 120  # Initial number of planes on side B

# Parameters
k1 <- 0.1  # Combat effectiveness rate for side A
k2 <- 0.2  # Combat effectiveness rate for side B
b1 <- 0.01  # Breakdown rate for side A
b2 <- 0.02  # Breakdown rate for side B
r1 <- 2  # Reinforcements rate for side A
r2 <- 1  # Reinforcements rate for side B

# Time points
times <- seq(0, 10, by = 0.1)

# Solve the system of equations
initial_state <- c(A = A0, B = B0)
parameters <- c(k1 = k1, k2 = k2, b1 = b1, b2 = b2, r1 = r1, r2 = r2)
solution <- ode(y = initial_state, times = times, func = lanchester_model, parms = parameters)

# Plot the results
df <- as.data.frame(solution)
ggplot(df, aes(x = time)) +
  geom_line(aes(y = A, color = "Side A")) +
  geom_line(aes(y = B, color = "Side B")) +
  labs(x = "Time", y = "Number of planes", color = "Side") +
  theme_minimal()

