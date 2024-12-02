library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)
library(gridExtra)

data = read.csv("C:/Users/lucas/Documents/GitHub/02427-AdvancedTimeSeries-CEX3/ex3_largecase copy.csv")

full_data <- data
full_data$hours_since_start <- as.numeric(difftime(full_data$Timestamp, min(full_data$Timestamp), units = "hours"))
full_data

# Scale the data
scale_factor <- 1/2500
.data1 <- data.frame(
  t = as.numeric(difftime(full_data$Timestamp, min(full_data$Timestamp), units = "hours")),
  R = as.numeric(full_data$Rainfall),
  P = as.numeric(full_data$Pumpflow) * scale_factor * 60, # Scale and convert to hourly
  y = as.numeric(full_data$Volume) * scale_factor         # Scale volume
)

# Model setup
model <- ctsmTMB$new()
model$setModelname("model_2_3_4")

# System equations
model$addSystem(
  dG ~ A * R * dt - (n/K) * G * dt + sigma * dw1,
  dC ~ (n/K)* G * dt - (n/K) * C * dt - 1/(1+exp(-a*(C - b))) * C * dt + sigma * dw2,
  dT ~ 1/(1+exp(-a*(C - b))) * C * dt - (n/K) * T * dt + sigma * dw3,
  dS ~ (n/K) * T * dt - P * dt + sigma * dw4
)

# Observation equations
model$addObs(
  y ~ S
)

# Observation variance
model$setVariance(
  y ~ sigma_y^2
)

# Add inputs
model$addInput(R)
model$addInput(P)

# Parameter initial values and bounds
model$setParameter(
  n = c(initial = 2, lower = 0.1, upper = 10),
  a = c(initial = 5, lower = 0, upper = 50),
  b = c(initial = 3, lower = 0, upper = 10),
  A = c(initial = 1, lower = 0, upper = 20),
  K = c(initial = 5, lower = 1, upper = 100),
  sigma = log(c(initial = 1, lower = 1e-10, upper = 1)),
  sigma_y = log(c(initial = 1, lower = 1, upper = 5))
)
#
# Initial state
model$setInitialState(
  list(c(.data1$R[1], .data1$P[1], .data1$P[1], .data1$y[1]), 1e-1 * diag(4))
)

# Estimate parameters
#fit <- model$estimate(data = .data1, method = "ekf", compile = TRUE, ode.timestep = 0.1)
fit <- model$estimate(data = .data1, method = "ekf")

# Summary and plots
model$summary(correlation = TRUE)
summary(fit)
plot(fit)

pred_1k <-model$predict(.data1,k.ahead=16)

pred_1k$states$S



# Set up a 2-row layout
par(mfrow = c(3, 1))

# Determine a common y-axis range
y_range <- range(c(.data1$y, pred_1k$states$S), na.rm = TRUE)
y_range <- c(0, 30)

# Plot 1: Rainwater over time (black line)
plot(.data1$t, .data1$R, type = "l", col = "black",  # Use common y-axis range
     ylab = "Stormwater", xlab = "Time (hours)", 
     main = "Observed Rainwater over time (Event 1)", xaxt = "n")
axis(1, at = seq(0, max(.data1$t), by = 2), labels = seq(0, max(.data1$t), by = 2))

# Plot 2: Stormwater over time (black line)
plot(.data1$t, .data1$y, type = "l", col = "black", 
     ylim = y_range,  # Use common y-axis range
     ylab = "Stormwater", xlab = "Time (hours)", 
     main = "Observed Stormwater over time (Event 1)", xaxt = "n")
axis(1, at = seq(0, max(.data1$t), by = 2), labels = seq(0, max(.data1$t), by = 2))

# Plot 3: Predicted Stormwater (red line)
plot(pred_1k$states$t.j, pred_1k$states$S, type = "l", col = "red", lty = 2, 
     ylim = y_range,  # Use the same y-axis range
     ylab = "Stormwater", xlab = "Time (hours)", 
     main = "Predicted Stormwater over time (Event 1)", xaxt = "n")
axis(1, at = seq(0, max(.data1$t), by = 2), labels = seq(0, max(.data1$t), by = 2))


