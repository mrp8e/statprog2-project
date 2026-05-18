# 01_download.R
# Downloads raw data from the source URL and saves to data/raw/.
# Run once; data/raw/ is read-only after this point.

library(here)

# url <- "https://..."
# download.file(url, destfile = here("data", "raw", "data.csv"))

message("Raw data already present — delete data/raw/ and re-run to refresh.")
