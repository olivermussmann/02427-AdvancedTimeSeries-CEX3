library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)

############################################################
# Data simulation
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
model$setModelname("model_2_1_1")

# Add system equations
model$addSystem(
  dX1 ~ A * Ut * dt - (1/K) * X1 * dt + sigma * dw1,
  dX2 ~ (1/K)* X1 * dt - 1/K * X2 * dt + sigma * dw2
  )

# Add observation equations
model$addObs(
  y ~ X2
)

# Set observation equation variances
model$setVariance(
  y ~ sigma_y^2
)

# Add vector input
model$addInput(Ut)

# Specify parameter initial values and lower/upper bounds in estimation
model$setParameter(
  A = c(initial=1.5, lower=0, upper=1000),
  K = c(initial=1.5, lower=1, upper=1000),
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)

# Set initial state mean and covariance
model$setInitialState(list(c(.data$Ut[1], .data$y[1]), 1e-1 * diag(2)))

# Carry out estimation with default settings (extended kalman filter)
fit <- model$estimate(data=.data, method="ekf", compile=T)

pred_1k <-model$predict(.data,k.ahead=16)


pred_1k
model$summary(correlation = TRUE)

pred_1k$observations$


data$hours_since_start <- as.numeric(difftime(data$timestamp, min(data$timestamp), units = "hours"))



par(mfrow = c(2, 1))
plot(.data$t, .data$y, type = "l", col = "black", 
     ylab = "Stormwater", xlab = "", main = "Stormwater and Rainfall over time, event 5", xaxt = "n")
axis(1, at = seq(0, max(.data$t), by = 2), labels = seq(0, max(.data$t), by = 2))
lines(pred_1k$states$t.j, pred_1k$states$X2, col = "red", lty = 2)  # Blue dashed line for predictions
lines(pred_1k$states$t.j, pred_1k$observations$y, col = "blue", lty = 2)  # Blue dashed line for predictions


plot(.data$t, .data$Ut, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "", xaxt = "n")
axis(1, at = seq(0, max(.data$t), by = 2), labels = seq(0, max(.data$t), by = 2))

summary(fit)

plot(fit)



  

# Extract residuals
residuals <- fit$residuals$mean$y
library(ggplot2)
residuals
residual_plot <- ggplot(data.frame(t = .data$t, residuals = residuals), aes(x = t, y = residuals)) +
  geom_line() +
  labs(title = "Residuals over Time", x = "Time", y = "Residuals") +
  theme_minimal()

print(residual_plot)


# Obtain the correlation matrix for the estimated parameters
correlation_matrix <- fit$cor.fixed
print(correlation_matrix)


# Check parameter estimates against truth
p0 = fit$par.fixed
p0


# Create plot of one-step predictions, simulated states and observations
t.est = fit$states$mean$prior$t
x.mean = fit$states$mean$prior$x
x.sd = fit$states$sd$prior$x
plot1 = ggplot() +
  geom_ribbon(aes(x=t.est, ymin=x.mean-2*x.sd, ymax=x.mean+2*x.sd),fill="grey", alpha=0.9) +
  geom_line(aes(x=t.est, x.mean),col="steelblue",lwd=1) +
  geom_line(aes(x=t.sim,y=x)) + 
  geom_point(aes(x=t.obs,y=y),col="tomato",size=1) +
  labs(title="1-Step State Estimates vs Observations", x="Time", y="") +
  theme_minimal()

# Predict to obtain k-step-ahead predictions to see model forecasting ability
pred.list = model$predict(data=.data, 
                          k.ahead=10, 
                          method="ekf",
)

# Create plot all 10-step predictions against data
pred = pred.list$states
pred10step = pred %>% dplyr::filter(k.ahead==10)
plot2 = ggplot() +
  geom_ribbon(aes(x=pred10step$t.j, 
                  ymin=pred10step$x-2*sqrt(pred10step$var.x),
                  ymax=pred10step$x+2*sqrt(pred10step$var.x)),fill="grey", alpha=0.9) +
  geom_line(aes(x=pred10step$t.j,pred10step$x),color="steelblue",lwd=1) +
  geom_point(aes(x=t.obs,y=y),color="tomato",size=1) +
  labs(title="10 Step Predictions vs Observations", x="Time", y="") +
  theme_minimal()

# Perform full prediction without data update
pred.list = model$predict(data=.data, 
                          k.ahead=1e6, 
                          method="ekf",
)

# Perform full simulation without data update
sim.list = model$simulate(data=.data, 
                          k.ahead=1e6, 
                          method="ekf"
)

# Collapse simulation data for easy use with ggplot 
sim.df = sim.list$states$x$i0 %>%
  select(!c("i","j","t.i","k.ahead")) %>%
  reshape2::melt(., id.var="t.j")

# Plot all full simulations and the full prediction against observations
# (full means no data-update at all)
plot3 = ggplot() +
  geom_line(data=sim.df, aes(x=t.j, y=value, group=variable),color="grey") +
  geom_line(aes(x=pred.list$states$t.j,y=pred.list$states$x),color="steelblue") +
  geom_point(aes(x=t.obs,y=y),color="tomato",size=1) +
  labs(title="No Update Prediction and Simulations vs Observations", x="Time", y="") +
  theme_minimal() + theme(legend.position = "none")

# Draw both plots
patchwork::wrap_plots(plot1, plot2, plot3, ncol=1)

# Plot one-step-ahead residual analysis using the command below
# plot(fit)

