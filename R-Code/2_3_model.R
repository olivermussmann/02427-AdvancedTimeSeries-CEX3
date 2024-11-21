library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)
library(gridExtra)

data = read.csv("~/Documents/Uni/Advanced TSA/CompEx3_E18/ex3_largecase copy.csv")
data

event1_data <- data[which(data$Event_ID==1),]
event1_data$hours_since_start <- as.numeric(difftime(event1_data$Timestamp, min(event1_data$Timestamp), units = "hours"))
event1_data # 4-18

# Scale the data
scale_factor <- 1/1000
.data1 <- data.frame(
  t = as.numeric(difftime(event1_data$Timestamp, min(event1_data$Timestamp), units = "hours")),
  R = as.numeric(event1_data$Rainfall),
  P = as.numeric(event1_data$Pumpflow) * scale_factor * 60, # Scale and convert to hourly
  y = as.numeric(event1_data$Volume) * scale_factor         # Scale volume
)

# Model setup
model <- ctsmTMB$new()
model$setModelname("model_2_3_2")

# System equations
model$addSystem(
  dG ~ A * R * dt - (n/K1) * G * dt + sigma * dw1,
  dC ~ (n/K1)* G * dt - (n/K2) * C * dt - 1/(1+exp(-a*(C - b))) * C * dt + sigma * dw2,
  dT ~ 1/(1+exp(-a*(C - b))) * C * dt - (n/K3) * T * dt + sigma * dw3,
  dS ~ (n/K3) * T * dt - P * dt + sigma * dw4
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
  n = 2,
  a = c(initial = 5, lower = -10, upper = 10),
  b = c(initial = 5, lower = -10, upper = 10),
  A = c(initial = 10, lower = 0, upper = 100),
  K1 = c(initial = 4, lower = 1, upper = 100),
  K2 = c(initial = 6, lower = 1, upper = 100),
  K3 = c(initial = 6, lower = 1, upper = 100),
  sigma = log(c(initial = 0.5, lower = 1e-10, upper = 10)),
  sigma_y = log(c(initial = 1, lower = 1, upper = 10))
)

# Initial state
model$setInitialState(
  list(c(.data1$R[1], .data1$P[1], .data1$P[1], .data1$y[1]), 1e-1 * diag(4))
)

# Estimate parameters
#fit <- model$estimate(data = .data1, method = "ekf", compile = TRUE, ode.timestep = 0.1)
fit <- model$estimate(data = .data1, method = "ekf", compile = TRUE)

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


