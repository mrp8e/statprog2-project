# 03_eda.R
# Exploratory data analysis: discover hidden patterns and trends, spot anomalies, investigate hypothesis
# Figures are saved to docs/ for inclusion in report.qmd.
# TODO: add EDA plots and summaries

library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
library(moments)
library(quantreg)

data <- read_csv(here("data", "raw", "delay_short.csv"))
glimpse(data) 

# summarize data
summary(data)

# check missing values
colSums(is.na(data))

# check duplicate rows
sum(duplicated(data))

# measure relationships of variables
cor(data$arr_del, data$arr_flights, use = "complete.obs")
cov(data$weather_ct, data$nas_ct, use = "complete.obs")
#===============================================================================
# check unique years
unique(data$year)

# check the months completeness
is_month_complete <-data %>%
  group_by(year) %>%
  summarise(
    missing_months = paste(setdiff(1:12, unique(month)), collapse = ", ")
  )
print(is_month_complete, n = Inf) # print years with incomplete months

# define delay_causes
delay_causes <- c("carrier_delay", "weather_delay", "nas_delay",
                  "security_delay", "late_aircraft_delay")

# aggregate monthly data into yearly totals
yearly_df <- data %>%
  group_by(year) %>%
  summarise(
    across(all_of(delay_causes), sum, na.rm = TRUE)
  )

# convert to long format for ggplot
yearly_long <- yearly_df %>%
  pivot_longer(
    cols = -year,
    names_to = "delay_cause",
    values_to = "total_delay_minutes"
  )

# plot yearly trends of each delay cause
ggplot(yearly_long, aes(x = year, y = total_delay_minutes, 
                        color = delay_cause, group = delay_cause)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = yearly_df$year) +
  labs(
    x = "Year",
    y = "Total delay minutes",
    title = "Yearly trends of delay causes",
    color = "Delay cause"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#===============================================================================
# create interactive plot
fig <- plot_ly()

# add each delay cause as a line
for (cause in delay_causes) {
  
  fig <- fig %>%
    add_trace(
      data = yearly_df,
      x = ~year,
      y = yearly_df[[cause]],
      type = "scatter",
      mode = "lines+markers",
      name = cause,
      line = list(width = 3),
      hovertemplate = paste(
        "Year: %{x}<br>",
        cause, ": %{y:,.0f} minutes",
        "<extra></extra>"
      )
    )
}

# add labels at the end of each line
for (cause in delay_causes) {
  
  fig <- fig %>%
    add_annotations(
      x = max(yearly_df$year),
      y = yearly_df[[cause]][nrow(yearly_df)],
      text = cause,
      showarrow = FALSE,
      xshift = 50,
      yshift = 0
    )
}

# update layout
fig <- fig %>%
  layout(
    width = 1200,
    height = 600,
    
    title = list(
      text = "Yearly Trends of Flight Delay Causes",
      x = 0.5
    ),
    
    xaxis = list(
      title = "Year",
      range = c(
        min(yearly_df$year),
        max(yearly_df$year) + 1
      ),
      tickmode = "array",
      tickvals = yearly_df$year
    ),
    
    yaxis = list(
      title = "Total Delay Minutes"
    ),
    
    showlegend = FALSE,
    hovermode = "x unified"
  )

# display interactive plot
fig
#===============================================================================
# calculate additional variables 
total_arrivals <- sum(data$arr_flights, na.rm = TRUE)
total_delay_count <- sum(data[c("carrier_ct", "weather_ct", "nas_ct", "security_ct", "late_aircraft_ct")], na.rm = TRUE)
total_delay_ratio <- total_delay_count / total_arrivals # total delay ratio of ALL delay causes

# calculate delay rate of total arrivals for each delay cause
carrier_del_rate <- sum(data$carrier_ct, na.rm = TRUE) /total_arrivals
weather_del_rate <- sum(data$weather_ct, na.rm = TRUE) /total_arrivals
nas_del_rate<- sum(data$nas_ct, na.rm = TRUE) /total_arrivals
security_del_rate<-sum(data$security_ct, na.rm = TRUE) /total_arrivals
late_aircraft_del_rate<-sum(data$late_aircraft_ct, na.rm = TRUE) /total_arrivals

# compute carrier delay rate of each row and median
data <- data %>%
  mutate(
    carrier_del_row = carrier_ct / arr_flights
  ) # calculate carrier delay ratio of each row

carrier_del_median <- median(data$carrier_del_row, na.rm = TRUE)

# visualize the carrier delay ratio histogram
ggplot(data,
       aes(x = carrier_delay_ratio)) +
  geom_histogram(
    bins = 50,
    fill = "skyblue",
    color = "black"
  ) +
  geom_vline(
    aes(xintercept = carrier_del_rate,
        linetype = "Mean"),
    linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = carrier_del_median,
        linetype = "Median"),
    linewidth = 1
  ) +
  theme_minimal() +
  labs(
    title = "Distribution of Carrier Delay Ratio",
    x = "Carrier delay ratio",
    y = "Frequency counts",
    linetype = ""
  )
#===============================================================================
# validate distributions and measure skewness and kurtosis
# calculate each delay cause ratio
data <- data %>%
  mutate(
    carrier_del_row = carrier_ct / arr_flights,
    weather_del_row = weather_ct / arr_flights,
    nas_del_row = nas_ct / arr_flights,
    security_del_row = security_ct / arr_flights,
    late_aircraft_del_row = late_aircraft_ct / arr_flights
  )


# convert to long format 
delay_ratio_long <- data %>%
  select(
    carrier_del_row,
    weather_del_row,
    nas_del_row,
    security_del_row,
    late_aircraft_del_row
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "delay_cause",
    values_to = "delay_ratio"
  )

# compare each distributions 
ggplot(delay_ratio_long,
aes(x = delay_ratio)) +
  geom_histogram(
    bins = 50,
    fill = "skyblue",
    color = "black"
  ) +
  facet_wrap(~delay_cause, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Distribution of Delay Ratios by Cause",
    x = "Delay ratio",
    y = "Frequency counts"
  )

#-> right-skewed, approximate to chi-squared distribution

# compute skewness and kurtosis score
moments::kurtosis(data$arr_del15, na.rm = TRUE)
moments::skewness(data$arr_del15, na.rm = TRUE)

#-> positive right skew
#-> extremely high kurtosis score. Flight arrival delay data has an incredibly sharp peak with very heavy, long tails
#-> most flights have minimal delays, but a few flights have extreme, massive delays.
#===============================================================================
# aggregate monthly delay data
monthly_delay <- data %>%
  group_by(year, month) %>%
  summarise(
    total_arrivals = sum(arr_flights, na.rm = TRUE),
    total_delay_count = sum(carrier_ct+weather_ct+nas_ct+security_ct+late_aircraft_ct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    delay_ratio = total_delay_count / total_arrivals
  )

# plot heatmap to detect seasonal pattern
ggplot(monthly_delay, aes(
  x = factor(month),
  y = factor(year),
  fill = delay_ratio
)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", delay_ratio)), size = 3) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "red",
    name = "Ratio"
  ) +
  labs(
    title = "Probability of delays across months",
    x = "Month",
    y = "Year"
  ) +
  theme_minimal()
#===============================================================================
# investigate hypothesis
model <- rq(arr_del15 ~ nas_ct+late_aircraft_ct, data = data, tau = 0.1)
#model <- rq(arr_del15 ~ weather_ct, data = data, tau = 0.5)
#model <- rq(arr_del15 ~ security_ct, data = data, tau = 0.9)
summary(model)

ggplot(data, aes(nas_ct+late_aircraft_ct, arr_del15)) +
  geom_point(size = 0.01) + 
  geom_abline(intercept=coef(model)[1], slope=coef(model)[2]) +
  geom_smooth(method="lm", se=F)










