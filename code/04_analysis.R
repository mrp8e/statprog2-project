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

# Visulization: 
# 1. Predicted vs. Observed Values
data$predicted <- NA
data$predicted[complete.cases(data[, vars])] <-
  predict(model_stable, type = "response")

ggplot(data, aes(x = predicted, 
                 y = arr_del15, 
                 text = paste("Predicted:", round(predicted, 1), 
                              "<br>Observed:", arr_del15, 
                              "<br>Flights:", arr_flights))) +
  geom_point(alpha = 0.5, 
             color = "skyblue", 
             size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "red", linewidth = 1) + 
  labs(title = "Observed vs. Predicted Delayed Flights", 
       subtitle = "Quasi-Poission Regression Model", 
       x = "Predicted Number of Delayed Flights", 
       y = "Observed Number of Delayed Flights") + 
  theme_minimal(base_size = 14)

# VIF: Variance Inflation Factor (Multikollinearität)
vif(model_lm)

# 2. Estimated Regression Coefficients 
coef_df <- tidy(model_stable, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = case_when(
      term == "arr_flights" ~ "Number of Arriving Flights",
      term == "carrier_delay" ~ "Carrier Delay",
      term == "weather_delay" ~ "Weather Delay",
      term == "nas_delay" ~ "NAS Delay",
      term == "late_aircraft_delay" ~ "Late Aircraft Delay",
      term == "security_delay" ~"Security Delays", 
      TRUE ~ term
    )
  ) %>%
  arrange(estimate)

coef_df <- coef_df %>%
  mutate(
    estimate = estimate * 1000,
    conf.low = conf.low * 1000,
    conf.high = conf.high * 1000
  )
ggplotly(
  ggplot(coef_df, aes(x = estimate,
                         y = reorder(Predictor, estimate),
                         text = paste0(
                           "<b>", Predictor, "</b>",
                           "<br>Estimate: ", round(estimate, 3),
                           "<br>95% CI: ", round(conf.low, 3),
                           " to ", round(conf.high, 3)))) +
  
  ## confidence intervals
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.15,
                 linewidth = 1.8,
                 colour = "black") +
  
  ## coefficient estimates
  geom_point(shape = 21, size = 2, stroke = 0.8, colour = "skyblue") +
  
  ## reference line
  geom_vline(xintercept = 0, colour = "red",
             linetype = "dashed", linewidth = 0.8) +
  
  scale_x_continuous(labels = label_number(accuracy = 0.01), 
                     expand = expansion(mult = c(0.05, 0.08))) +
  
  labs(title = "Estimated Regression Coefficients",
       subtitle = "Points represent coefficient estimates; horizontal bars show 95% confidence intervals.",
       x = "Coefficient Estimate (×10⁻³)",
       y = NULL) + 
  
  theme_minimal(), 
  
  tooltip = "text")
