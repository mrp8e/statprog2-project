# 02_clean.R
# Reads raw data, applies cleaning steps, writes to data/processed/.

library(tidyverse)
library(here)

raw <- read_csv(here("data", "raw", "delay_short.csv"))

delay <- raw |>
  # TODO: add cleaning steps
  
  # Convert month to categorical variable to capture seasonality in analyses
  mutate(month = as.factor(month)) |>
  
  mutate(nas_ct = ifelse(nas_ct < 0, 0, nas_ct)) |>
  mutate(nas_delay = ifelse(nas_delay < 0, NA, nas_delay)) |>
  identity()

write_csv(delay, here("data", "processed", "data_clean.csv"))
message("Wrote data/processed/data_clean.csv")



