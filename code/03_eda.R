# 03_eda.R
# Exploratory data analysis: distributions, missingness, relationships, discover hidden patterns, understand baselines, check assumptions
# Figures are saved to docs/ for inclusion in report.qmd.

library(tidyverse)
library(here)

data <- read_csv(here("data", "processed", "data_clean.csv"))

# TODO: add EDA plots and summaries
glimpse(data)


# Multi-lingual interactive analysis with Python & R (Ziqi)
install.packages("reticulate") #set up embedded python environment 
library(reticulate)

# import python libraries


# 1. Overall seasonality with cohort map
# 2. PCA to reduce dimensions especially with multi-collinear variables
# 3. prepare time-series data
# 3.0 check the year and month completeness
# 3.1 isolate outliers, smooth out noises
# 3.2 STL decomposition for seasonality, cynical patterns
# 4. validate initial assumptions
# !make plots interactive, ggplotly or gganimate

