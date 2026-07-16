# 03_eda.R
# Exploratory data analysis: discover hidden patterns and trends, spot anomalies, check assumption
# Figures are saved to docs/ for inclusion in report.qmd.

library(tidyverse)
library(here)

data <- read_csv(here("data", "processed", "data_clean.csv"))

# TODO: add EDA plots and summaries; Jupyter Notebook Python code in R umwandeln 
glimpse(data)







