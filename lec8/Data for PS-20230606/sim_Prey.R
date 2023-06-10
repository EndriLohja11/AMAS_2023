# Prey and Predator


# install.packages("deSolve", dependencies = TRUE)

# Include library

library(deSolve)

# For more information
?deSolve

# There are five model parameters: a, b, K, g and e that are defined first. 
# Parameters are stored
# as a vector with assigned names and values:
parameters <- c(a = 0.05, b = 0.0002, K = 500, g = 0.8, e = 0.03)

# The three state variables are also created as a vector, and their initial values given:
state <- c(X = 300, Y = 10)

# Model equations
Prey <- function(t, state, parameters) { 
  with(as.list(c(state, parameters)),{
  dX <- a*X*(1-X/K) - b*X*Y
  dY <- g*b*X*Y - e*Y
  list(c(dX, dY)) })
  }

# Time specification
times <- seq(0, 1000, by = 0.5)

# Model integration

out <- ode(state, times, Prey, parameters)

# Plotting results
par(oma = c(1, 1, 1, 1)) # Set outside margins
par(mar = c(4, 5, 2, 1)) # Set plot margins

plot(out, xlab = "time", ylab = "Changes in time")

par(mfrow=c(1,1))
plot(out[, "X"], out[, "Y"], pch = ".", xlab='Prey', ylab='Predator')

mtext(outer = TRUE, side = 3, "Prey predator model", cex = 1.2)



plot(out[,"X"], type='l', col = "darkgreen", ylim = c(0, 450), xlim = c(0, 1000), xlab='time', lwd = 2, ylab = "", main ='Prey green, predator red') 
lines(out[,"Y"], col = "red", lwd = 2, lty=2)

########################################################
# Clear workspace
rm(list = ls())



