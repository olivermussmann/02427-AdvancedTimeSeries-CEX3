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
  dS ~ 1/(1+exp(-a*(C - b))) * C * dt - P * dt + sigma * dw3
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
  sigma = log(c(initial = 0.5, lower = 1e-10, upper = 10)),
  sigma_y = log(c(initial = 1, lower = 1, upper = 10))
)

# Initial state
model$setInitialState(
  list(c(.data1$R[1], .data1$P[1], .data1$y[1]), 1e-1 * diag(3))
)

# Estimate parameters
fit <- model$estimate(data = .data1, method = "ekf", compile = TRUE, ode.timestep = 0.1)

# Summary and plots
model$summary(correlation = TRUE)
summary(fit)
plot(fit)





############################################################
# First Model
############################################################

# Create model object
model = ctsmTMB$new()

# Set name of model (and the created .cpp file)
model$setModelname("model_2_3_2")

# Add system equations
model$addSystem(
  dG ~ A * R * dt - (n/K1) * G * dt + sigma * dw1,
  dC ~ (n/K1)* G * dt - (n/K2) * C * dt - 1/(1+exp(-a*(C - b))) * C * dt + sigma * dw2,
  dS ~ 1/(1+exp(-a*(C - b))) * C * dt - P * dt + sigma * dw3
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
model$addInput(R)
model$addInput(P)

# Specify parameter initial values and lower/upper bounds in estimation
model$setParameter(
  n = 2,
  a = c(initial = 1, lower = -100, upper = 100),
  b = c(initial = 1, lower = -100, upper = 100),
  A = c(initial = 1.5, lower = 0, upper = 1000),
  K1 = c(initial = 1.5, lower = 1, upper = 1000),
  K2 = c(initial = 1.5, lower = 1, upper = 1000),
  sigma = log(c(initial = 1, lower = 1e-10, upper = 30)),
  sigma_y = log(c(initial = 1, lower = 1, upper = 30)) # Increased lower bound to 1
)


# Set initial state mean and covariance
model$setInitialState(
  list(c(.data1$R[1], .data1$P[1], .data1$y[1]), 1e-1 * diag(3))
)

# Carry out estimation with default settings (extended kalman filter)
fit <- model$estimate(data = .data1, method = "ekf", compile = TRUE, ode.timestep =0.1)

model$summary(correlation = TRUE)
summary(fit)

plot(fit)

fit$nll
  