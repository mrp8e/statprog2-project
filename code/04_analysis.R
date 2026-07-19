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



# Extracting State and computing their flights mean delay
locationdelay <- delay %>% mutate(State = str_extract(airport_name, "(?<=, )[A-Z]{2}(?=:)"), mean_flight_delay = arr_delay / arr_flights)
locationdelaysum <- locationdelay %>% 
  group_by(State) %>% 
  summarize(
    mean_delay = sum(arr_delay, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE),
    n = n(),
    total_flights = sum(arr_flights, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_delay))

# Barplot of top 10 states  in term of mean arrival delay 
top10_states <- locationdelaysum %>% slice_head(n = 10) %>% arrange(desc(mean_delay))%>% pull(State)
bottom10_states <- locationdelaysum %>% slice_tail(n = 10) %>% pull(State)

locationdelaysum %>%
  filter(State %in% top10_states) %>%
  ggplot(aes(
    x = State,
    y = mean_delay
  )) +
  geom_col() +
  theme_minimal() +
  scale_x_discrete(limits = top10_states) +
  labs(
    title = "Arrival Delay Distribution: Top 10 States by Mean Delay",
    x = "State",
    y = "Mean Flight Delay (minutes)"
  )

#Barplot of bottom 10 states in term of mean arrival delay 
bottom10_states <- locationdelaysum %>% slice_tail(n = 10) %>% pull(State)

locationdelaysum %>%
  filter(State %in% bottom10_states) %>%
  ggplot(aes(
    x = State,
    y = mean_delay
  )) +
  geom_col() +
  theme_minimal() +
  scale_x_discrete(limits = bottom10_states) +
  labs(
    title = "Arrival Delay Distribution: Bottom 10 States by Mean Delay",
    x = "State",
    y = "Mean Flight Delay (minutes)"
  )
#Boxplot for Delay Distribution of all states 
locationdelay %>%
  ggplot(aes(x = reorder(State, -mean_flight_delay, FUN = median, na.rm = TRUE), y = mean_flight_delay)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 75)) +
  theme_minimal() +
  labs(title = "TStates by Mean Delay", x = "State", y = "Mean Flight Delay (minutes)") 


#Boxplot for top10 airports Delay Distribution and also All Flights in commparison
airportdelay <- delay %>%
  group_by(airport_name) %>%
  summarize(
    total_flights = sum(arr_flights, na.rm = TRUE)
  ) %>%
  arrange(desc(total_flights))

biggestairports <- airportdelay %>% slice_head(n = 10) %>% pull(airport_name)



airportrows <- delay %>%
  mutate(mean_flight_delay = arr_delay / arr_flights) %>%
  filter(airport_name %in% biggestairports, is.finite(mean_flight_delay))

allairportrows <- delay %>%
  mutate(mean_flight_delay = arr_delay / arr_flights,
         airport_name = "All Airports") %>%
  filter(is.finite(mean_flight_delay))


combined_airport <- bind_rows(airportrows, allairportrows)

combined_airport %>%
  ggplot(aes(
    x = reorder(airport_name, -mean_flight_delay, FUN = median, na.rm = TRUE),
    y = mean_flight_delay
  )) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 75)) +
  theme_minimal() +
  labs(
    title = "Delay Distribution: 10 Biggest Airports(here not ordered by total flights)",
    x = "Airport",
    y = "Mean Flight Delay (minutes)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



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
