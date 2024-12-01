library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)
library(ctsmTMB)
library(gridExtra)
library(cowplot)

############################################################
# Load Data
############################################################

data <- read.csv("C:/Users/lucas/Documents/GitHub/02427-AdvancedTimeSeries-CEX3/ex3_largecase copy.csv")

# Convert 'Timestamp' to proper datetime format
data$Timestamp <- as.POSIXct(data$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Separate the data by EventID
events <- unique(data$Event_ID)

# List to store plots for all events
all_plots <- list()

# Loop through each Event and create subplots
for (event in events) {
  event_data <- data %>% filter(Event_ID == event)
  
  # Plot Rainfall
  rainfall_plot <- ggplot(event_data, aes(x = Timestamp, y = Rainfall)) +
    geom_line(color = "blue") +
    scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M") +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    labs(title = paste("Rainfall - Event", event), x = "Time", y = "Rainfall (µm/min)") +
    theme_minimal()
  
  # Plot Pumpflow
  pumpflow_plot <- ggplot(event_data, aes(x = Timestamp, y = Pumpflow)) +
    geom_line(color = "red") +
    scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M") +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    labs(title = paste("Pumpflow - Event", event), x = "Time", y = "Pumpflow (m^3/min)") +
    theme_minimal()
  
  # Plot Volume
  volume_plot <- ggplot(event_data, aes(x = Timestamp, y = Volume)) +
    geom_line(color = "green") +
    scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M") +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    labs(title = paste("Volume - Event", event), x = "Time", y = "Volume (m^3)") +
    theme_minimal()
  
  # Store the plots for this event
  all_plots <- append(all_plots, list(rainfall_plot, pumpflow_plot, volume_plot))
}

# Combine all plots into a grid with consistent row heights
combined_plot <- plot_grid(
  plotlist = all_plots,
  ncol = 3,
  align = "v", # Align vertically for consistent heights
  axis = "l"   # Keep left alignment of axes
)

# Display and save the combined plot
print(combined_plot)

ggsave("C:/Users/lucas/Documents/GitHub/02427-AdvancedTimeSeries-CEX3/combined_plot_2_3_1.png",
       plot = combined_plot, width = 8.27, height = 11.69, units = "in")












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


