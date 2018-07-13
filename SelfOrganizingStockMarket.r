library(readxl)
library(tidyr) # gather
library(dplyr)
library(visdat) # missmap
library(fields) # heatColors
library(RColorBrewer) # heatColors
library(kohonen)

# Get data from Shiller & Goyal
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

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
  "PE" = P / E,
  "PB" = 1 / as.numeric(bm),
  "PD" = P / D
)

# Check NAs
vis_dat(full_data)
# -------------------------------------------------------

# Set palette
heatColors <- function(n, alpha = 1) {
  rev(designer.colors(n = n, col = brewer.pal(9, "Spectral")))
}

# Keep only months where all data is available
omitted_data <- na.omit(full_data)

# Keep only variables to be used and scale the data
som_data <- omitted_data %>% select(CAPE, PE, PB, PD, UnEmp, infl, `Rate GS10`, tenyear)
som_data_scaled <- apply(som_data, 2, scale)

# Make the SOM grid and model using all of the variables
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_model <- som(som_data_scaled,
                 grid = som_grid,
                 rlen = 100, alpha = c(0.05, 0.01), keep.data = T
)

# Function for plotting SOM heatmaps
plot_som <- function(variable) {
  unit_colors <- aggregate(data.frame(som_data[, variable])[, 1],
                           by = list(som_model$unit.classif), FUN = mean, simplify = T
  )
  plot(som_model,
       type = "property", shape = "straight", property = unit_colors[, 2],
       main = variable, palette.name = heatColors
  )
}

# Plot all the variables
par(mfrow = c(2, 2))
for (i in seq_along(colnames(som_data))) {
  plot_som(colnames(som_data)[i])
}

# Plot the qality plots
plot(som_model, type = "counts", shape = "straight", main = "Node Counts")
plot(som_model, type = "quality", shape = "straight", main = "Node Quality/Distance")
plot(som_model, type = "dist.neighbours", shape = "straight", main = "SOM neighbour distances",
     palette.name = grey.colors)
plot(som_model, shape = "straight", type = "codes")
