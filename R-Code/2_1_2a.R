library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)

data = read.csv("~/Documents/Uni/Advanced TSA/CompEx3_E18/ex1_rainfallrunoff copy.csv")
data

# Create data
.data = data.frame(
  t = as.numeric(difftime(data$timestamp, min(data$timestamp), units = "hours")),
  Ut = as.numeric(data$rainfall),
  y = as.numeric(data$stormwater)  # Rename X2 to y here
)
.data

# Create model object
model = ctsmTMB$new()
model$setModelname("model_2_1_2")
model$addSystem(
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1
)
model$addObs(
  y ~ X1
)
model$setVariance(
  y ~ sigma_y^2
)
model$addInput(Ut)
model$setParameter(
  n = 0,
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=0, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)
model$setInitialState(list(c(.data$y[1]), 1e-1 * diag(1)))
fit <- model$estimate(data=.data, method="ekf", compile=T)
cat("Summary for model with", 1, "states:\n")
print(model$summary(correlation = TRUE))
plot(fit)



# Create model object
model = ctsmTMB$new()
model$setModelname("model_2_1_2")
model$addSystem(
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - (n/K) * X2 * dt + sigma * dw2
)
model$addObs(
  y ~ X2
)
model$setVariance(
  y ~ sigma_y^2
)
model$addInput(Ut)
model$setParameter(
  n = 1,
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=0, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)
model$setInitialState(list(c(0, .data$y[1]), 1e-1 * diag(2)))
fit <- model$estimate(data=.data, method="ekf", compile=T)
cat("Summary for model with", 2, "states:\n")
print(model$summary(correlation = TRUE))
plot(fit)




# Create model object
model = ctsmTMB$new()
model$setModelname("model_2_1_2")
model$addSystem(
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - (n/K) * X2 * dt + sigma * dw2,
  dX3 ~ (n/K)* X2 * dt + sigma * dw3
)
model$addObs(
  y ~ X3
)
model$setVariance(
  y ~ sigma_y^2
)
model$addInput(Ut)
model$setParameter(
  n = 2,
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=0, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)
model$setInitialState(list(c(0, 0, .data$y[1]), 1e-1 * diag(3)))
fit <- model$estimate(data=.data, method="ekf", compile=T)
cat("Summary for model with", 3, "states:\n")
print(model$summary(correlation = TRUE))
plot(fit)


# Create model object
model = ctsmTMB$new()
model$setModelname("model_2_1_2")
model$addSystem(
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - (n/K) * X2 * dt + sigma * dw2,
  dX3 ~ (n/K)* X2 * dt - (n/K) * X3 * dt + sigma * dw3,
  dX4 ~ (n/K)* X3 * dt + sigma * dw4
  #dX5 ~ (n/K)* X5 * dt + sigma * dw5
)
model$addObs(
  y ~ X4
)
model$setVariance(
  y ~ sigma_y^2
)
model$addInput(Ut)
model$setParameter(
  n = 3,
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=0, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)
model$setInitialState(list(c(0, 0, 0, .data$y[1]), 1e-1 * diag(4)))
fit <- model$estimate(data=.data, method="ekf", compile=T)
cat("Summary for model with", 4, "states, i.e. n= 3:\n")
print(model$summary(correlation = TRUE))
plot(fit)




# --------------------
# Create model object
model = ctsmTMB$new()
model$setModelname("model_2_1_2")
model$addSystem(
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - (n/K) * X2 * dt + sigma * dw2,
  dX3 ~ (n/K)* X2 * dt - (n/K) * X3 * dt + sigma * dw3,
  dX4 ~ (n/K)* X3 * dt - (n/K) * X4 * dt + sigma * dw4,
  dX5 ~ (n/K)* X4 * dt + sigma * dw5
)
model$addObs(
  y ~ X5
)
model$setVariance(
  y ~ sigma_y^2
)
model$addInput(Ut)
model$setParameter(
  n = 4,
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=0, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)
model$setInitialState(list(c(0, 0, 0, 0, .data$y[1]), 1e-1 * diag(5)))
fit <- model$estimate(data=.data, method="ekf", compile=T)
cat("Summary for model with", 5, "states:\n")
print(model$summary(correlation = TRUE))
plot(fit)



# --------------------

model = ctsmTMB$new()
model$setModelname("model_2_1_2")
model$addSystem(
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - (n/K) * X2 * dt + sigma * dw2,
  dX3 ~ (n/K)* X2 * dt - (n/K) * X3 * dt + sigma * dw3,
  dX4 ~ (n/K)* X3 * dt - (n/K) * X4 * dt + sigma * dw4,
  dX5 ~ (n/K)* X4 * dt - (n/K) * X5 * dt + sigma * dw5,
  dX6 ~ (n/K)* X5 * dt + sigma * dw6
)
model$addObs(
  y ~ X6
)
model$setVariance(
  y ~ sigma_y^2
)
model$addInput(Ut)
model$setParameter(
  n = 5,
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=0, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)
model$setInitialState(list(c(0, 0, 0, 0, 0, .data$y[1]), 1e-1 * diag(6)))
fit <- model$estimate(data=.data, method="ekf", compile=T)
cat("Summary for model with", 6, "states:\n")
print(model$summary(correlation = TRUE))
plot(fit)


# --------------------

model = ctsmTMB$new()
model$setModelname("model_2_1_2")
model$addSystem(
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - (n/K) * X2 * dt + sigma * dw2,
  dX3 ~ (n/K)* X2 * dt - (n/K) * X3 * dt + sigma * dw3,
  dX4 ~ (n/K)* X3 * dt - (n/K) * X4 * dt + sigma * dw4,
  dX5 ~ (n/K)* X4 * dt - (n/K) * X5 * dt + sigma * dw5,
  dX6 ~ (n/K)* X5 * dt - (n/K) * X6 * dt + sigma * dw6,
  dX7 ~ (n/K)* X6 * dt + sigma * dw7
)
model$addObs(
  y ~ X7
)
model$setVariance(
  y ~ sigma_y^2
)
model$addInput(Ut)
model$setParameter(
  n = 6,
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=0, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)
model$setInitialState(list(c(0, 0, 0, 0, 0, .data$y[1]), 1e-1 * diag(6)))
fit <- model$estimate(data=.data, method="ekf", compile=T)
cat("Summary for model with", 7, "states:\n")
print(model$summary(correlation = TRUE))
plot(fit)



# --------------------

model = ctsmTMB$new()
model$setModelname("model_2_1_2")
model$addSystem(
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - (n/K) * X2 * dt + sigma * dw2,
  dX3 ~ (n/K)* X2 * dt - (n/K) * X3 * dt + sigma * dw3,
  dX4 ~ (n/K)* X3 * dt - (n/K) * X4 * dt + sigma * dw4,
  dX5 ~ (n/K)* X4 * dt - (n/K) * X5 * dt + sigma * dw5,
  dX6 ~ (n/K)* X5 * dt - (n/K) * X6 * dt + sigma * dw6,
  dX7 ~ (n/K)* X6 * dt - (n/K) * X7 * dt + sigma * dw7,
  dX8 ~ (n/K)* X7 * dt + sigma * dw8
)
model$addObs(
  y ~ X8
)
model$setVariance(
  y ~ sigma_y^2
)
model$addInput(Ut)
model$setParameter(
  n = 7,
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=0, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)
model$setInitialState(list(c(0, 0, 0, 0, 0, .data$y[1]), 1e-1 * diag(6)))
fit <- model$estimate(data=.data, method="ekf", compile=T)
cat("Summary for model with", 8, "states:\n")
print(model$summary(correlation = TRUE))
plot(fit)
