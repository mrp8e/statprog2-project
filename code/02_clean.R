# 02_clean.R
# Reads raw data, applies cleaning steps, writes to data/processed/.

library(tidyverse)
library(here)

raw <- read_csv(here("data", "raw", "data.csv"))

cleaned <- raw |>
  # TODO: add cleaning steps
  identity()

write_csv(cleaned, here("data", "processed", "data_clean.csv"))
message("Wrote data/processed/data_clean.csv")
