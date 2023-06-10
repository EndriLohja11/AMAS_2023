library(deSolve)

# Function defining the SIR model
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -alpha * S * I
    dI <- alpha * S * I - beta * I
    dR <- beta * I
    
    return(list(c(dS, dI, dR)))
  })
}

# Initial conditions
S0 <- 1000  # Initial number of susceptible individuals
I0 <- 10    # Initial number of infected individuals
R0 <- 0     # Initial number of recovered individuals

# Parameters
alpha <- 0.1  # Infection rate
beta <- 0.05  # Recovery rate

# Time points
times <- seq(0, 100, by = 0.1)

# Solve the system of equations
initial_state <- c(S = S0, I = I0, R = R0)
parameters <- c(alpha = alpha, beta = beta)
solution <- ode(y = initial_state, times = times, func = sir_model, parms = parameters)

# Plot the results
plot(solution, xlab = "Time", ylab = "Population", main = "SIR Model")
legend("topright", legend = c("Susceptible", "Infected", "Recovered"), col = c("blue", "red", "green"), lty = 1)
