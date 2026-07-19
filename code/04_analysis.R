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

# The boxplot shows the distribution of arrival delays across different carriers
delay %>%
  ggplot(aes(
    x = carrier,
    y = arr_delay
    )) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Arrival Delays by Carrier",
    x = "Carrier",
    y = "Arrival Delay (minutes)"
  )


# TODO: Add group comparisons (e.g., boxplots across carriers/airports) and 
# maybe add top 10 most delayed airpoints plots
# Temporal/seasonal trend analysis (including Covid-19 anomaly checks) -> Ziqi put this part into 03_eda 



# 6. RQ2: CORRELATION & RELATIONSHIP ANALYSIS between Flight Volume and Arrival Delays

## Data preparation

### Aggregating flight data by airport, year and month
airport_delays <- delay %>%
  group_by(airport, year, month) %>%
  summarise(
    arr_flights = sum(arr_flights, na.rm = TRUE),
    arr_del15 = sum(arr_del15, na.rm = TRUE),
    .groups = "drop"
  )

### Calculating delay rate
airport_delays <- airport_delays %>%
    filter(arr_flights > 0) %>%
    mutate(
      delay_rate = arr_del15 / arr_flights)


## Relationship between flight volume and arrival delays

### Absolute number of delayed flights

# This scatterplot examines whether airports with higher flight volumes
# also experience a higher number of delayed flights in absolute terms.
ggplot(airport_delays, aes(
  x = arr_flights,
  y = arr_del15
  )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(
    x = "Number of arriving flights",
    y = "Number of delayed flights"
  )

cor(
  airport_delays$arr_flights,
  airport_delays$arr_del15
  )
# A strong positive correlation indicates that airports with higher flight volumes
# tend to have more delayed flights in absolute numbers.


### Relative delay rate

# This scatterplot examines the relationship between the number of arriving flights
# and the proportion of delayed flights.
ggplot(airport_delays, aes(
  x = arr_flights,
  y = delay_rate
)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(
    x = "Number of arriving flights",
    y = "Delay rate"
  )

cor(
  airport_delays$arr_flights,
  airport_delays$delay_rate
  )
# The correlation is close to zero, indicating that flight volume does not have
# a meaningful linear relationship with the proportion of delayed flights.


# Larger airports experience more delays in absolute numbers, but not necessarily
# a higher delay rate.


## Regression analysis

### Predicting absolute delayed flights

# Linear regression model predicting absolute number of delayed flights
# based on flight volume
model_absolut <- lm(arr_del15 ~ arr_flights, data = airport_delays)

#### coefficient, p-value, R^2, Interpretation
coef(model_absolut) ["arr_flights"]
# The regression coefficient for arr_flights is 0.1837, indicating that
# each additional arriving flight is associated with an increase of approximately
# 0.184 delayed flights.

#### p-value
summary(model_absolut)$ coefficients["arr_flights", "Pr(>|t|)"]
# The relationship is statistically significant (p < 0.001), suggesting that
# flight volume is a significant predictor of the absolute number of delayed flights.

#### R²
summary(model_absolut)$r.squared
# The model explains approximately 85.3% of the variation in delayed flights
# (R² = 0.853), indicating that flight volume strongly predicts the absolute
# number of delays.


### Relative delay rate
# Can flight volume predict the proportion of delayed flights?
model_relative <- lm(delay_rate ~ arr_flights, data = airport_delays)

#### coefficient
coef(model_relative) ["arr_flights"]
# The regression coefficient for arr_flights is close to zero, indicating that
# flight volume has almost no effect on the delay rate.

#### p-value
summary(model_relative)$ coefficients["arr_flights", "Pr(>|t|)"]
# The relationship is not statistically significant (p = 0.185), suggesting that
# flight volume is not a meaningful predictor of the proportion of delayed flights.

#### R²
summary(model_relative)$r.squared
# The model explains only 0.005% of the variation in delay rate (R² ≈ 0),
# indicating that delay rates are largely independent of flight volume.



## Correlation matrix
library(corrplot)

cor_data <- airport_delays %>%
  select(arr_flights, arr_del15, delay_rate)

cor_matrix <- cor(cor_data, use = "complete.obs")

corrplot(cor_matrix,
         method = "color",
         addCoef.col = "black")

## Regression diagnostics

### Diagnostics for absolute delay model
par(mfrow = c(2, 2))
plot(model_absolut)

### Diagnostics for relative delay model
par(mfrow = c(2, 2))
plot(model_relative)

par(mfrow = c(1,1))


## Model comparison
data.frame(
 Model = c("Absolute delays", "Delay rate"),
 R_squared = c(
   summary(model_absolut)$r.squared,
   summary(model_relative)$r.squared
 )
)

## Conclusion for RQ2
# Flight volume has a strong positive relationship with the absolute number
# of delayed flights. However, it does not significantly affect the delay rate,
# indicating that larger flight volumes lead to more delays in total but not
# necessarily to a higher probability of delay.




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

