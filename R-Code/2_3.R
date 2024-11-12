library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)
library(gridExtra)

############################################################
# Load Data
############################################################

data = read.csv("~/Documents/Uni/Advanced TSA/CompEx3_E18/ex3_largecase copy.csv")
start_time <- as.POSIXct("2018-08-12 04:00:00")

# Calculate hours since the start time
data$hours_since_start <- as.numeric(difftime(data$Timestamp, start_time, units = "hours"))


event1_data <- data[which(data$Event_ID==1),]
event1_data$hours_since_start <- as.numeric(difftime(event1_data$Timestamp, min(event1_data$Timestamp), units = "hours"))
event1_data # 4-18

event2_data <- data[which(data$Event_ID==2),]
event2_data$hours_since_start <- as.numeric(difftime(event2_data$Timestamp, min(event2_data$Timestamp), units = "hours"))
event2_data # 7-21

event3_data <- data[which(data$Event_ID==3),]
event3_data$hours_since_start <- as.numeric(difftime(event3_data$Timestamp, min(event3_data$Timestamp), units = "hours"))
event3_data # 4-18

event4_data <- data[which(data$Event_ID==4),]
event4_data$hours_since_start <- as.numeric(difftime(event4_data$Timestamp, min(event4_data$Timestamp), units = "hours"))
event4_data # 21-11

event5_data <- data[which(data$Event_ID==5),]
event5_data$hours_since_start <- as.numeric(difftime(event5_data$Timestamp, min(event5_data$Timestamp), units = "hours"))
event5_data # 00-14

event6_data <- data[which(data$Event_ID==6),]
event6_data$hours_since_start <- as.numeric(difftime(event6_data$Timestamp, min(event6_data$Timestamp), units = "hours"))
event6_data # 12 - 02

############################################################
# Plot Data
############################################################

par(mfrow = c(2, 1))
plot(event1_data$hours_since_start, event1_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Volume and Rainfall over time, event 1", xaxt = "n")
axis(1, at = seq(0, max(event1_data$hours_since_start), by = 2), labels = seq(0, max(event1_data$hours_since_start), by = 2))
plot(event1_data$hours_since_start, event1_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "", xaxt = "n")
axis(1, at = seq(0, max(event1_data$hours_since_start), by = 2), labels = seq(0, max(event1_data$hours_since_start), by = 2))

par(mfrow = c(2, 1))
plot(event2_data$hours_since_start, event2_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Volume and Rainfall over time, event 2", xaxt = "n")
axis(1, at = seq(0, max(event2_data$hours_since_start), by = 2), labels = seq(0, max(event2_data$hours_since_start), by = 2))
plot(event2_data$hours_since_start, event2_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "", xaxt = "n")
axis(1, at = seq(0, max(event2_data$hours_since_start), by = 2), labels = seq(0, max(event2_data$hours_since_start), by = 2))

par(mfrow = c(2, 1))
plot(event3_data$hours_since_start, event3_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Volume and Rainfall over time, event 3", xaxt = "n")
axis(1, at = seq(0, max(event3_data$hours_since_start), by = 2), labels = seq(0, max(event3_data$hours_since_start), by = 2))
plot(event3_data$hours_since_start, event3_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "", xaxt = "n")
axis(1, at = seq(0, max(event3_data$hours_since_start), by = 2), labels = seq(0, max(event3_data$hours_since_start), by = 2))

par(mfrow = c(2, 1))
plot(event4_data$hours_since_start, event4_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Volume and Rainfall over time, event 4", xaxt = "n")
axis(1, at = seq(0, max(event4_data$hours_since_start), by = 2), labels = seq(0, max(event4_data$hours_since_start), by = 2))
plot(event4_data$hours_since_start, event4_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "", xaxt = "n")
axis(1, at = seq(0, max(event4_data$hours_since_start), by = 2), labels = seq(0, max(event4_data$hours_since_start), by = 2))


par(mfrow = c(2, 1))
plot(event5_data$hours_since_start, event5_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Volume and Rainfall over time, event 5", xaxt = "n")
axis(1, at = seq(0, max(event5_data$hours_since_start), by = 2), labels = seq(0, max(event5_data$hours_since_start), by = 2))
plot(event5_data$hours_since_start, event5_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "", xaxt = "n")
axis(1, at = seq(0, max(event5_data$hours_since_start), by = 2), labels = seq(0, max(event5_data$hours_since_start), by = 2))


par(mfrow = c(2, 1))
plot(event6_data$hours_since_start, event6_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Volume and Rainfall over time, event 6", xaxt = "n")
axis(1, at = seq(0, max(event6_data$hours_since_start), by = 2), labels = seq(0, max(event6_data$hours_since_start), by = 2))
plot(event6_data$hours_since_start, event6_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "", xaxt = "n")
axis(1, at = seq(0, max(event6_data$hours_since_start), by = 2), labels = seq(0, max(event6_data$hours_since_start), by = 2))



# Set up the plotting area to have 6 rows and 2 columns
pdf("combined_plot_2_3.pdf", width = 8, height = 12)  # Adjust dimensions as needed
par(mfrow = c(6, 2), mar = c(4, 4, 2, 1))  # Adjust margins for better spacing

# Event 1: Volume and Rainfall
plot(event1_data$hours_since_start, event1_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Event 1 - Volume", xaxt = "n")
axis(1, at = seq(0, max(event1_data$hours_since_start), by = 2), labels = seq(0, max(event1_data$hours_since_start), by = 2))

plot(event1_data$hours_since_start, event1_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "Event 1 - Rainfall", xaxt = "n")
axis(1, at = seq(0, max(event1_data$hours_since_start), by = 2), labels = seq(0, max(event1_data$hours_since_start), by = 2))

# Event 2: Volume and Rainfall
plot(event2_data$hours_since_start, event2_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Event 2 - Volume", xaxt = "n")
axis(1, at = seq(0, max(event2_data$hours_since_start), by = 2), labels = seq(0, max(event2_data$hours_since_start), by = 2))

plot(event2_data$hours_since_start, event2_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "Event 2 - Rainfall", xaxt = "n")
axis(1, at = seq(0, max(event2_data$hours_since_start), by = 2), labels = seq(0, max(event2_data$hours_since_start), by = 2))

# Event 3: Volume and Rainfall
plot(event3_data$hours_since_start, event3_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Event 3 - Volume", xaxt = "n")
axis(1, at = seq(0, max(event3_data$hours_since_start), by = 2), labels = seq(0, max(event3_data$hours_since_start), by = 2))

plot(event3_data$hours_since_start, event3_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "Event 3 - Rainfall", xaxt = "n")
axis(1, at = seq(0, max(event3_data$hours_since_start), by = 2), labels = seq(0, max(event3_data$hours_since_start), by = 2))

# Event 4: Volume and Rainfall
plot(event4_data$hours_since_start, event4_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Event 4 - Volume", xaxt = "n")
axis(1, at = seq(0, max(event4_data$hours_since_start), by = 2), labels = seq(0, max(event4_data$hours_since_start), by = 2))

plot(event4_data$hours_since_start, event4_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "Event 4 - Rainfall", xaxt = "n")
axis(1, at = seq(0, max(event4_data$hours_since_start), by = 2), labels = seq(0, max(event4_data$hours_since_start), by = 2))

# Event 5: Volume and Rainfall
plot(event5_data$hours_since_start, event5_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Event 5 - Volume", xaxt = "n")
axis(1, at = seq(0, max(event5_data$hours_since_start), by = 2), labels = seq(0, max(event5_data$hours_since_start), by = 2))

plot(event5_data$hours_since_start, event5_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "Event 5 - Rainfall", xaxt = "n")
axis(1, at = seq(0, max(event5_data$hours_since_start), by = 2), labels = seq(0, max(event5_data$hours_since_start), by = 2))

# Event 6: Volume and Rainfall
plot(event6_data$hours_since_start, event6_data$Volume, type = "l", col = "black", 
     ylab = "Volume", xlab = "", main = "Event 6 - Volume", xaxt = "n")
axis(1, at = seq(0, max(event6_data$hours_since_start), by = 2), labels = seq(0, max(event6_data$hours_since_start), by = 2))

plot(event6_data$hours_since_start, event6_data$Rainfall, type = "l", col = "black", 
     ylab = "Rainfall", xlab = "Time (hours)", main = "Event 6 - Rainfall", xaxt = "n")
axis(1, at = seq(0, max(event6_data$hours_since_start), by = 2), labels = seq(0, max(event6_data$hours_since_start), by = 2))

dev.off()


############################################################
# First Model
############################################################


data = read.csv("~/Documents/Uni/Advanced TSA/CompEx3_E18/ex3_largecase copy.csv")

event1_data <- data[which(data$Event_ID==1),]
event1_data$hours_since_start <- as.numeric(difftime(event1_data$Timestamp, min(event1_data$Timestamp), units = "hours"))
event1_data # 4-18
event1_data$Timestamp2 = as.numeric(event1_data$Timestamp) + 1
event1_data
# Create data
.data = data.frame(
  t = as.numeric(event1_data$Timestamp),
  R = as.numeric(event1_data$Rainfall),
  P = as.numeric(event1_data$Pumpflow),
  y = as.numeric(event1_data$Volume)  # Rename X2 to y here
)

.data

# Create model object
model = ctsmTMB$new()

# Set name of model (and the created .cpp file)
model$setModelname("model_2_3_2")

# Add system equations
model$addSystem(
  dG ~ A * R * dt - (n/K1) * G * dt + sigma * dw1,
  dC ~ (n/K1)* G * dt - (n/K2) * C * dt - 1/(1+exp(-a*(C - b))) * C * dt + sigma * dw2,
  dT ~ 1/(1+exp(-a*(C - b))) * C * dt - (n/K3)* T * dt + sigma * dw3,
  dS ~ (n/K3)* T * dt + - P * dt + sigma * dw4
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
  n = 3,
  a = c(initial=1, lower=-100, upper=100),
  b = c(initial=1, lower=-100, upper=100),
  A = c(initial=1.5, lower=0, upper=1000),
  K1 = c(initial=1.5, lower=1, upper=1000),
  K2 = c(initial=1.5, lower=1, upper=1000),
  K3 = c(initial=1.5, lower=1, upper=1000),
  
  sigma  = log(c(initial=1e-1, lower=1e-10, upper=30)),
  sigma_y  = log(c(initial=1e-1, lower=1e-10, upper=30))
  
)

# Set initial state mean and covariance
model$setInitialState(list(c(.data$R[1], .data$R[1], .data$R[1], .data$y[1]), 1e-1 * diag(4)))

# Carry out estimation with default settings (extended kalman filter)
fit <- model$estimate(data=.data, method="ekf", compile=T)

model$summary(correlation = TRUE)
summary(fit)

plot(fit)

fit$states$mean$
  
  
  # Extract residuals
  residuals <- fit$residuals$mean$y
library(ggplot2)
residuals
residual_plot <- ggplot(data.frame(t = .data$t, residuals = residuals), aes(x = t, y = residuals)) +
  geom_line() +
  labs(title = "Residuals over Time", x = "Time", y = "Residuals") +
  theme_minimal()

print(residual_plot)


