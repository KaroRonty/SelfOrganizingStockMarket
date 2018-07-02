library(readxl)
library(tidyr)
library(dplyr)

source("shiller_goyal_data_retriever.r")

# Get the BLS data and format it
bls_data <- read_xlsx("bls_data.xlsx", sheet = 1, skip = 10)
bls_data <- bls_data %>% gather(month, UnEmp, Jan:Dec)

# Make the months into sortable and arrange by date
bls_data$month <- factor(bls_data$month, levels = month.abb)
bls_data <- bls_data %>% arrange(Year, month)

# Add month numbers and leading zeros, paste months and years together
bls_data$months <- rep(1:12, tail(bls_data$Year, 1) - head(bls_data$Year, 1) + 1)
bls_data$months <- sprintf("%02d", as.numeric(bls_data$months))
bls_data$dates <- paste(bls_data$Year, bls_data$months, sep = "-")
bls_data <- bls_data %>% select(dates, UnEmp)

# Combine with Shiller's & Goyal's data
full_data <- full_join(full_data, bls_data, by = "dates")
# -----------------------------------------------------
library(kohonen)
