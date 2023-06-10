## Chaos in the atmosphere
# The Lorenz model describes the dynamics of three state variables, X, Y and Z.
# download package deSolve

install.packages("deSolve", dependencies = TRUE)

# Include library

library(deSolve)

# For more information
?deSolve

# There are three model parameters: a, b, and c that are defined thirst. Parameters are stored
# as a vector with assigned names and values:
parameters <- c(a = -8/3, b = -10, c = 28)

# The three state variables are also created as a vector, and their initial values given:
state1 <- c(X = 1, Y = 1, Z = 1)
state2 <- state1 + c(1e-6, 0, 0)

# Model equations
Lorenz <- function(t, state, parameters) 
  { with(as.list(c(state, parameters)),
         {
           dX <- a*X + Y*Z
           dY <- b*(Y-Z)
           dZ <- -X*Y + c*Y - Z
           list(c(dX, dY, dZ))
           })  
  }

# Time specification
times <- seq(0, 50, by = 0.01)

# Model integration
# Function ode returns an object of class deSolve with a matrix that contains 
# the values of the state variables (columns) at the requested output times.
out1 <- ode(state1, times, Lorenz, parameters)
out2 <- ode(state2, times, Lorenz, parameters)

# Plotting results
par(oma = c(1, 1, 1, 1)) # Set outside margins
par(mar = c(4, 5, 2, 1)) # Set plot margins
plot(out1, out2, xlab = "time", ylab = "", lwd = 2, lty = 1, mfrow = c(1, 3))

if (require(scatterplot3d))
  scatterplot3d(out1[,-1], type = "l")

plot(out1[, "X"], out1[, "Z"], pch = ".", main = "Lorenz butterfly", xlab = "x", ylab = "y", mfrow = c(1, 1), mfcol = c(1,1))


plot(out1, out2, col = c("blue", "orange"),
     main = c("X", "Y", "Z"),
     xlim = list(c(20, 30), c(25, 30), NULL), mfrow = c(1, 3))


########################################################
# Clear workspace
rm(list = ls())

