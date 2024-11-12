library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)

############################################################
# Load Data
############################################################

data = read.csv("~/Documents/Uni/Advanced TSA/CompEx3_E18/ex1_rainfallrunoff copy.csv")
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
  dX1 ~ A * Ut * dt - (n/K) * X1 * dt + sigma * dw1,
  dX2 ~ (n/K)* X1 * dt - (n/K) * X2 * dt + sigma * dw2,
  dX3 ~ (n/K)* X2 * dt + sigma * dw3,
  dX4 ~ (n/K)* X3 * dt + sigma * dw4
  #dX5 ~ (n/K)* X5 * dt + sigma * dw5
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
model$setInitialState(list(c(0, 0, 0, .data$y[1]), 1e-1 * diag(4)))

# Carry out estimation with default settings (extended kalman filter)
fit <- model$estimate(data=.data, method="ekf", compile=T)

fit
model$summary(correlation=TRUE)
plot(fit)


# Check parameter estimates against truth
p0 = fit$par.fixed
p0
cbind(c(exp(p0[1]),p0[2],exp(p0[3]),exp(p0[4])), pars)

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
