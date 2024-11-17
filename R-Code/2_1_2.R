library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)

############################################################
# Load Data
############################################################

data = read.csv("C:/Users/lucas/Documents/GitHub/02427-AdvancedTimeSeries-CEX3/R-code/ex1_rainfallrunoff copy.csv")
data

# Create data
.data = data.frame(
  t = as.numeric(difftime(data$timestamp, min(data$timestamp), units = "hours")),
  Ut = as.numeric(data$rainfall),
  y = as.numeric(data$stormwater)  # Rename X2 to y here
)

.data
############################################################
# Model creation and estimation
############################################################

# Create model object
model = ctsmTMB$new()

# Set name of model (and the created .cpp file)
model$setModelname("model_2_1_2")

# Add system equations
model$addSystem(
  dX1 ~ A * Ut * dt    - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - (n/K) * X2 * dt + sigma * dw2,
  dX3 ~ (n/K)* X2 * dt - (n/K) * X3 * dt + sigma * dw3,
  dX4 ~ (n/K)* X3 * dt - (n/K) * X4 * dt + sigma * dw4,
  dX5 ~ (n/K)* X4 * dt + sigma * dw5
)

# Add observation equations
model$addObs(
  #y ~ x
  y ~ X4
)

# Set observation equation variances
model$setVariance(
  y ~ sigma_y^2
)

# Add vector input
model$addInput(Ut)

# Specify parameter initial values and lower/upper bounds in estimation
model$setParameter(
  n = 3,
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=0, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)

# Set initial state mean and covariance
model$setInitialState(list(c(0, 0, 0, 0, .data$y[1]), 1e-1 * diag(5)))

# Carry out estimation with default settings (extended kalman filter)
fit <- model$estimate(data=.data, method="ekf", compile=T)

fit
model$summary(correlation=TRUE)
plot(fit)

