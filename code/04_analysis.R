# 04_analysis.R
# Main statistical analysis: modelling and inference.

# 1. SETUP & LIBRARIES
library(tidyverse)
library(here)
library(broom)
library(car)
library(naniar)
library(MASS)

# Load clean dataset
data <- read_csv(here("data", "processed", "data_clean.csv"))


# 2. MISSING DATA DIAGNOSTICS
print("Summary of Missing Values:")
miss_var_summary(data)
data %>% slice_sample(n=10000) %>% 
  vis_miss()

# 3. FEATURE ENGINEERING
# Creating performance and delay metrics for downstream analysis
data <- data %>%
  mutate(
    ontime_flights = arr_flights - arr_del15,
    delay_rate     = arr_del15 / arr_flights,
    ontime_rate    = ontime_flights / arr_flights,
    avg_delay_per_delayed_flight = ifelse(arr_del15 > 0, 
                                          arr_delay / arr_del15, NA)
  )

# 4. EXPLORATORY DISTRIBUTION ANALYSIS
print("Summary Statistics for Delay Rate:")
summary(data$delay_rate)


# Plotting the distribution of arrival delays
ggplot(data, aes(x = arr_delay)) +
  geom_histogram(bins = 50, fill = "skyblue") +
  theme_minimal() +
  labs(title = "Distribution of Arrival Delays",
       x = "Arrival delay (minutes)", y = "Count")

# Q-Q Plot to visually assess the normality assumption
ggplot(data, aes(sample = arr_delay)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth = 1.5) +
  theme_minimal() +
  labs(title = "Q-Q Plot for Arrival Delays", 
       x = "Expected values for a perfect normal distribution", 
       y = "Actual delay (in minutes)", 
       subtitle = "Comparison of Arrival Delays with an Ideal 
       Normal Distribution")

# 5. RQ1: DESCRIPTIVE STATISTICS & GROUP COMPARISONS

summary(delay$arr_delay)
mean(delay$arr_delay, na.rm = TRUE)

# Group data by carrier and compute mean, median delay and number of flights
delay %>%
  group_by(carrier_name) %>%
  summarise(
    mean_delay = mean(arr_delay, na.rm = TRUE),
    median_delay = median(arr_delay, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(mean_delay))

boxplot(arr_delay ~ carrier, data = delay)


# TODO: Add group comparisons (e.g., boxplots across carriers/airports) and 
# temporal/seasonal trend analysis (including Covid-19 anomaly checks).


# 6. RQ2: CORRELATION & RELATIONSHIP ANALYSIS


# TODO: Add scatterplots and correlation heatmaps to investigate the 
# relationship between flight volumes and specific delay counts.


# 7. RQ3: REGRESSION MODELLING & INFERENCE
# NOTE: 
# Since 'arr_delay' is the exact mathematical sum of all specific delay causes 
# (carrier, weather, nas, security, late_aircraft), an Ordinary Least Squares
# regression predicting 'arr_delay' directly would result in a R² of 1.0. 
# Therefore, we model 'arr_del15' (the count of delayed flights) instead.

# Baseline Model: Ordinary Least Squares (OLS) Linear Regression
model_lm <- lm(arr_del15 ~ arr_flights + carrier_delay + weather_delay 
               + nas_delay + late_aircraft_delay, data = data)
print("Linear Regression Summary:")
summary(model_lm)

# ASSUMPTION CHECK: Testing for Poisson suitability (Equidistribution)
# Classic Poisson regression requires Mean (μ) == Variance (σ²). 
proof_stats <- data %>%
  summarise(
    mean_mu = mean(arr_del15, na.rm = TRUE),
    variance_sigma2 = var(arr_del15, na.rm = TRUE)
  )
print("Proof of Overdispersion: ")
print(proof_stats)
                     
# Final Model: Quasi-Poisson Regression
# We chose the Quasi-Poisson Regression over standard Poisson due to extreme 
# overdispersion (Variance > Mean).
# This approach ensures mathematically stable estimates and robust 
# standard errors.
model_stable <- glm(arr_del15 ~ arr_flights + carrier_delay + weather_delay 
                    + nas_delay + late_aircraft_delay, 
                    data = data, family = quasipoisson(link = "log"))
summary(model_stable)

