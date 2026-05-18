# 03_eda.R
# Exploratory data analysis: distributions, missingness, relationships.
# Figures are saved to docs/ for inclusion in report.qmd.

library(tidyverse)
library(here)

data <- read_csv(here("data", "processed", "data_clean.csv"))

# TODO: add EDA plots and summaries
glimpse(data)
