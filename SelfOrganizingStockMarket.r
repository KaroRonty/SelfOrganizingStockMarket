library(readxl)
library(tidyr)
library(dplyr)
library(visdat)
library(kohonen)

# Get data from Shiller & Goyal
source("shiller_goyal_data_retriever.r")

# Load unemployment data from BLS
bls_data <- read_xlsx("bls_data.xlsx", sheet = 1, skip = 10)
bls_data <- bls_data %>% gather(month, UnEmp, Jan:Dec)

# Make the months into sortable and arrange by date
bls_data$month <- factor(bls_data$month, levels = month.abb)
bls_data <- bls_data %>% arrange(Year, month)

# Add month numbers and leading zeros, paste month and year together
bls_data$months <- rep(1:12, tail(bls_data$Year, 1) - head(bls_data$Year, 1) + 1)
bls_data$months <- sprintf("%02d", as.numeric(bls_data$months))
bls_data$dates <- paste(bls_data$Year, bls_data$months, sep = "-")
bls_data <- bls_data %>% select(dates, UnEmp)


# Join the  data with data from Shiller & Goyal by month
full_data <- full_join(full_data, bls_data, by = "dates")

# Calculate more needed variables
full_data <- full_data %>% mutate(
  "PE" = P/E,
  "PB" = 1/as.numeric(bm),
  "PD" = P/D
)

# Check NAs
vis_dat(full_data)
# -------------------------------------------------------


# Calculate returns for the next 10 years


# Keep only months where all data is available
omitted_data <- na.omit(full_data)

# Keep only variables to be used and scale the data
som_data <- omitted_data %>% select(CAPE,PE, PB, PD, UnEmp, `Rate GS10`, tenyear)
som_data_scaled <- apply(som_data, 2, scale)
