library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)

############################################################
# Load Data
############################################################

data = read.csv("~/Documents/Uni/Advanced TSA/CompEx3_E18/ex2_overflow copy.csv")
data



# Create data
.data = data.frame(
  t = as.numeric(difftime(data$timestamp, min(data$timestamp), units = "hours")),
  Ut = as.numeric(data$rainfall),
  y = as.numeric(data$stormwater)  # Rename X2 to y here
)

.data
############################################################
# Linear Model
############################################################

# Create model object
model = ctsmTMB$new()

# Set name of model (and the created .cpp file)
model$setModelname("model_2_2_linear")

# Add system equations
model$addSystem(
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - n/K * X2 * dt + sigma * dw2,
  dX3 ~ (n/K)* X2 * dt - n/K * X3 * dt + sigma * dw3
  )

# Add observation equations
model$addObs(
  y ~ X3
)

# Set observation equation variances
model$setVariance(
  y ~ sigma_y^2
)

# Add vector input
model$addInput(Ut)

# Specify parameter initial values and lower/upper bounds in estimation
model$setParameter(
  n = 2,
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=1, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
)

# Set initial state mean and covariance
model$setInitialState(list(c(.data$Ut[1], .data$Ut[1], .data$y[1]), 1e-1 * diag(3)))

# Carry out estimation with default settings (extended kalman filter)
fit <- model$estimate(data=.data, method="ekf", compile=T)
pred_1k <-model$predict(.data,k.ahead=1)

model$summary(correlation = TRUE)
summary(fit)

plot(fit, main="")
par(mfrow = c(2, 1))
plot(.data$t, .data$y, type = "l", col = "black", 
     ylab = "Stormwater", xlab = "", main = "Stormwater and Rainfall over time, event 5", xaxt = "n")
axis(1, at = seq(0, max(.data$t), by = 2), labels = seq(0, max(.data$t), by = 2))
lines(pred_1k$states$t.j, pred_1k$observations$y, col = "red", lty = 2)  # Blue dashed line for predictions


plot(.data$t, .data$Ut, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "", xaxt = "n")
axis(1, at = seq(0, max(.data$t), by = 2), labels = seq(0, max(.data$t), by = 2))


  
  
  # Extract residuals
  residuals <- fit$residuals$mean$y
library(ggplot2)
residuals
residual_plot <- ggplot(data.frame(t = .data$t, residuals = residuals), aes(x = t, y = residuals)) +
  geom_line() +
  labs(title = "Residuals over Time", x = "Time", y = "Residuals") +
  theme_minimal()

print(residual_plot)



############################################################
# Overflow Model
############################################################

# Create model object
model = ctsmTMB$new()

# Set name of model (and the created .cpp file)
model$setModelname("model_2_2_overflow")

# Add system equations
model$addSystem(
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - (n/K) * X2 * dt - 1/(1+exp(-a*(X2 - b))) * X2 * dt + sigma * dw2,
  dS ~ 1/(1+exp(-a*(X2 - b))) * X2 * dt + sigma * dw3
)

# Add observation equations
model$addObs(
  #y ~ x
  y ~ S
)

# Set observation equation variances
model$setVariance(
  y ~ sigma_y^2
)

# Add vector input
#model$addInput(u)
model$addInput(Ut)

# Specify parameter initial values and lower/upper bounds in estimation
model$setParameter(
  n = 2,
  a = c(initial=1, lower=-100, upper=100),
  b = c(initial=1, lower=-100, upper=100),
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=1, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)

# Set initial state mean and covariance
model$setInitialState(list(c(.data$Ut[1], .data$Ut[1], .data$y[1]), 1e-1 * diag(3)))

# Carry out estimation with default settings (extended kalman filter)
fit <- model$estimate(data=.data, method="ekf", compile=T)
pred_1k <-model$predict(.data,k.ahead=1)


model$summary(correlation = TRUE)
summary(fit)

plot(fit)
par(mfrow = c(1, 1))
plot(.data$t, .data$y, type = "l", col = "black", 
     ylab = "Stormwater", xlab = "", main = "Stormwater and Rainfall over time, event 5", xaxt = "n")
axis(1, at = seq(0, max(.data$t), by = 2), labels = seq(0, max(.data$t), by = 2))
lines(pred_1k$states$t.j, pred_1k$states$S, col = "red", lty = 2)  # Blue dashed line for predictions


plot(.data$t, .data$Ut, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "", xaxt = "n")
axis(1, at = seq(0, max(.data$t), by = 2), labels = seq(0, max(.data$t), by = 2))
  
  
  # Extract residuals
  residuals <- fit$residuals$mean$y
library(ggplot2)
residuals
residual_plot <- ggplot(data.frame(t = .data$t, residuals = residuals), aes(x = t, y = residuals)) +
  geom_line() +
  labs(title = "Residuals over Time", x = "Time", y = "Residuals") +
  theme_minimal()

print(residual_plot)



# Plot sigmoid


a <- 19.93198693  # Adjust 'a' as desired
b <- 6.89051683  # Adjust 'b' as desired

# Define the sigmoid function
sigmoid <- function(X2, a, b) {
  1 / (1 + exp(-a * (X2 - b)))
}

# Create a range for X2
X2_values <- seq(0, 15, length.out = 100)

# Calculate sigmoid values for the specified range of X2
sigmoid_values <- sigmoid(X2_values, a, b)

# Plot the sigmoid function
plot(X2_values, sigmoid_values, type = "l", col = "blue", lwd = 2,
     xlab = "X", ylab = "Sigmoid(X)", main = "Sigmoid Function Plot", xaxt = "n")
axis(1, at = seq(0, 15, by = 1))

