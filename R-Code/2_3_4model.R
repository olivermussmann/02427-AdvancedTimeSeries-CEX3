# Load necessary libraries
library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)
library(gridExtra)

data = read.csv("C:/Users/agent/OneDrive/Skrivebord/02427-AdvancedTimeSeries-CEX3/ex3_largecase copy.csv")
full_data <- data
full_data$Timestamp <- seq(0, nrow(full_data) - 1)


# Plot Rainfall
p1 <- ggplot(full_data, aes(x = Timestamp, y = Rainfall)) +
  geom_line(color = "blue") +  # Specify line color
  ggtitle("Rainfall Over Time") +
  xlab("Time") +
  ylab("Rainfall (mm)") +
  theme_minimal()

# Plot Pumpflow
p2 <- ggplot(full_data, aes(x = Timestamp, y = Pumpflow)) +
  geom_line(color = "red") +  # Specify line color
  ggtitle("Pumpflow Over Time") +
  xlab("Time") +
  ylab("Pumpflow") +
  theme_minimal()

# Plot Volume
p3 <- ggplot(full_data, aes(x = Timestamp, y = Volume)) +
  geom_line(color = "green") +  # Specify line color
  ggtitle("Volume Over Time") +
  xlab("Time") +
  ylab("Volume") +
  theme_minimal()

# Combine plots using gridExtra
grid.arrange(p1, p2, p3, nrow = 3)

# Scale the data
scale_factor <- 1/2500
data_fit <- data.frame(
  t = as.numeric(full_data$Timestamp),
  R = as.numeric(full_data$Rainfall),
  P = as.numeric(full_data$Pumpflow) * scale_factor * 60, # Scale and convert to hourly
  y = as.numeric(full_data$Volume) * scale_factor         # Scale volume
)






# Model setup
model <- ctsmTMB$new()
model$setModelname("stormwater_model10")

# System equations
model$addSystem(
  dG ~ A * R * dt - (n/K) * G * dt + sigma* dw1,
  dC ~ (n/K) * G * dt - (n/K) * C * dt - 1 / (1 + exp(-a * (C - b))) * C * dt + sigma * dw2,
  dT ~ 1 / (1 + exp(-a * (C - b))) * C * dt - (n/K) * T * dt + sigma * dw3,
  dS ~ (n/K) * T * dt - P * dt + sigma * dw4
)

# Observation equations
model$addObs(
  y ~ S
)

# Observation variance - Softplus
model$setVariance(
  y ~ (1 + exp(a1 + a2 * R + a3 * P))^2 + sigma_y^2
)

# Add inputs
model$addInput(R)
model$addInput(P)

# Parameter initial values and bounds
# Load necessary libraries
library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)
library(gridExtra)

data = read.csv("C:/Users/agent/OneDrive/Skrivebord/02427-AdvancedTimeSeries-CEX3/ex3_largecase copy.csv")
full_data <- data
full_data$Timestamp <- seq(0, nrow(full_data) - 1)


# Plot Rainfall
p1 <- ggplot(full_data, aes(x = Timestamp, y = Rainfall)) +
  geom_line(color = "blue") +  # Specify line color
  ggtitle("Rainfall Over Time") +
  xlab("Time") +
  ylab("Rainfall (mm)") +
  theme_minimal()

# Plot Pumpflow
p2 <- ggplot(full_data, aes(x = Timestamp, y = Pumpflow)) +
  geom_line(color = "red") +  # Specify line color
  ggtitle("Pumpflow Over Time") +
  xlab("Time") +
  ylab("Pumpflow") +
  theme_minimal()

# Plot Volume
p3 <- ggplot(full_data, aes(x = Timestamp, y = Volume)) +
  geom_line(color = "green") +  # Specify line color
  ggtitle("Volume Over Time") +
  xlab("Time") +
  ylab("Volume") +
  theme_minimal()

# Combine plots using gridExtra
grid.arrange(p1, p2, p3, nrow = 3)

# Scale the data
scale_factor <- 1/2500
data_fit <- data.frame(
  t = as.numeric(full_data$Timestamp),
  R = as.numeric(full_data$Rainfall),
  P = as.numeric(full_data$Pumpflow) * scale_factor * 60, # Scale and convert to hourly
  y = as.numeric(full_data$Volume) * scale_factor         # Scale volume
)






# Model setup
model <- ctsmTMB$new()
model$setModelname("stormwater_model11")

# System equations
model$addSystem(
  dG ~ A * R * dt - (n/K) * G * dt + sigma* dw1,
  dC ~ (n/K) * G * dt - (n/K) * C * dt - 1 / (1 + exp(-a * (C - b))) * C * dt + sigma * dw2,
  dT ~ 1 / (1 + exp(-a * (C - b))) * C * dt - (n/K) * T * dt + sigma * dw3,
  dS ~ (n/K) * T * dt - P * dt + sigma * dw4
)

# Observation equations
model$addObs(
  y ~ S
)

# Observation variance - Softplus
model$setVariance(
  y ~ (1 + exp(a1 * R + a2 * P))^2 + sigma_y^2
)

# Add inputs
model$addInput(R)
model$addInput(P)

# Parameter initial values and bounds



#sigma = log(c(initial = 0.5, lower = 1e-10, upper = 4)),
model$setParameter(
  n = 2,
  a = c(initial = 2, lower = 0.1, upper = 50),
  b = c(initial = 2, lower = 0.1, upper = 50),
  A = c(initial = 3, lower = 0.1, upper = 50),
  K = c(initial = 25, lower = 10, upper = 100),
  a1 = c(initial = 1, lower = -1, upper = 5),
  a2 = c(initial = 1, lower = -1, upper = 5),
  sigma = log(c(initial = 1e-3, lower = 1e-3, upper = 2)),
  sigma_y = log(c(initial = 1e-3, lower = 1e-3, upper = 2))
)

#
# Initial state
model$setInitialState(
  list(c(data_fit$R[1], data_fit$P[1], data_fit$P[1], data_fit$y[1]), 1e-1 * diag(4))
)


# Estimate parameters
#fit <- model$estimate(data = .data1, method = "ekf", compile = TRUE, ode.timestep = 0.1)
fit <- model$estimate(data = data_fit, method = "ekf", ode.timestep = 0.1)

# Summary and plots
model$summary(correlation = TRUE)
summary(fit)
plot(fit)

pred_1k <-model$predict(data_fit,k.ahead=1)

pred_1k$states$S



# Set up a 3-row layout
par(mfrow = c(3, 1), mar = c(5, 5, 3, 1), cex.axis = 0.8, cex.lab = 0.9)

# Determine x-axis tick intervals dynamically for better readability
x_ticks <- seq(0, max(data_fit$t), by = max(data_fit$t) / 10)
y_range <- c(0, 30)
# Plot 1: Rainwater over time
plot(data_fit$t, data_fit$R, type = "l", col = "blue", 
     ylab = "Rainfall (mm)", xlab = "Time (hours)",
     main = "Observed Rainfall over Time", xaxt = "n", lwd = 1.5, cex.main = 1.2, cex.lab = 1, cex.axis = 0.9)
axis(1, at = x_ticks, labels = round(x_ticks, 1), las = 1, cex.axis = 0.8)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Plot 2: Observed Stormwater over time
plot(data_fit$t, data_fit$y, type = "l", col = "black", 
     ylim = y_range, ylab = "Stormwater Volume", xlab = "Time (hours)",
     main = "Observed Stormwater over Time", xaxt = "n", lwd = 1.5, cex.main = 1.2, cex.lab = 1, cex.axis = 0.9)
axis(1, at = x_ticks, labels = round(x_ticks, 1), las = 1, cex.axis = 0.8)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Plot 3: Predicted Stormwater over time
plot(pred_1k$states$t.j, pred_1k$states$S, type = "l", col = "red", lty = 2, ylim = y_range, ylab = "Stormwater Volume", xlab = "Time (hours)",
     main = "Predicted Stormwater over Time", xaxt = "n", lwd = 1.5, cex.main = 1.2, cex.lab = 1, cex.axis = 0.9)
axis(1, at = x_ticks, labels = round(x_ticks, 1), las = 1, cex.axis = 0.8)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

plot_data <- data.frame(
  Time = pred_1k$states$t.j,  # Time values
  StormwaterVolume = pred_1k$states$S  # Stormwater volume values
)

