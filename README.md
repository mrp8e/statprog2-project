# Airline Punctuality & Delay Causes US

This project investigates flight delays in the United States using monthly airline performance data. The analysis focuses on delay behaviour across airlines, airports, and time. Statistical methods and visualisations are used to identify patterns in delays and to explore potential factors associated with delayed flights.

## Research Questions

1.  How do flight delays vary across airlines, airports and over time? What is the distribution of different delay causes across flights, airlines and airports? What temporal and seasonal patterns can be observed?

2.  Is there a relationship between the number of flights (arr_flights) and the number of delayed flights (arr_del15)? Can we predict total arrival delays based on flight volume and delay causes?

3.  Can arrival delays (arr_del15) be predicted based on flight characteristics and different delay causes such as carrier, weather, NAS, security, and late aircraft delays? What factors are significant predictors of flight delays?

## Dataset

- **Source:** U.S. Bureau of Transportation Statistics (BTS)
- **Licence:** Public Domain (U.S. Government Data)
- **Description:** The dataset is structured as a monthly summary grouped by airline and airport and collected from the U.S. federal government. Each row in the dataset gives information about how a specific carrier performed at a specific airport during a specific month and year. Variables include temporal information (year and month), airline and airport identifiers, flight counts, delay statistics, cancellations, diversions, delay durations and summary performance metrics such as on-time and delay rates. A detailed description of all variables is provided in the data dictionary.

## Group Members

| Name          | GitHub username |
|---------------|-----------------|
| Emil Sitka    | mrp8e           |
| Luise Killich | lkillich09      |
| Ziqi Ang      | z7oon           |
| Franka Konold | franka570       |

## Repository Structure

```         
data/raw/        read-only raw data and licence documentation
data/processed/  cleaned data produced by code/02_clean.R
code/            numbered R scripts (01 download → 02 clean → 03 EDA → 04 analysis)
docs/            rendered Quarto website output (auto-generated, do not edit)
proposal.qmd     W07 project proposal
report.qmd       final analysis report
```

## How to reproduce

``` r
# 1. Install dependencies
renv::restore()   # if using renv, otherwise install packages manually

# 2. Run the pipeline in order
source("code/01_download.R")
source("code/02_clean.R")
source("code/03_eda.R")
source("code/04_analysis.R")

# 3. Render the website
quarto::quarto_render()
```
