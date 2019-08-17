library(readxl)
library(tidyr) # gather
library(dplyr)
library(fields) # heatColors
library(RColorBrewer) # heatColors
library(kohonen)

# Get data from Shiller & Goyal
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

# Load unemployment data from BLS, format dates
bls_data <- read_xlsx("bls_data.xlsx",
                      sheet = 1,
                      skip = 10) %>%
  gather(month, UnEmp, Jan:Dec) %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(Year, month) %>%
  mutate(months = sprintf("%02d", match(month, month.abb)),
         dates = paste(Year, months, sep = "-")) %>%
  select(dates, UnEmp)

# Join the data with data from Shiller & Goyal by year and month
# Calculate more needed variables, keep only months where all data is available
combined_data <- full_data %>%
  full_join(bls_data, by = "dates") %>%
  mutate("PE" = P / E,
         "PB" = 1 / as.numeric(bm),
         "PD" = P / D) %>%
  na.omit()

# Keep only the variables to be used
som_data <- combined_data %>%
  select(CAPE,
         PE,
         PB,
         PD,
         UnEmp,
         infl,
         `Rate GS10`,
         tenyear_real)

# Scale the data for SOM
som_data_scaled <- apply(som_data, 2, scale)

# Make the SOM grid and model using all of the variables
set.seed(5)
som_grid <- somgrid(xdim = 6, ydim = 6, topo = "hexagonal")
som_model <- som(som_data_scaled,
                 grid = som_grid,
                 rlen = 100,
                 alpha = c(0.05, 0.01),
                 keep.data = TRUE)

# Set heat colors palette
heatColors <- function(n, alpha = 1) {
  rev(designer.colors(n = n, col = brewer.pal(9, "Spectral")))
}

# Function for plotting SOM heatmaps
plot_som <- function(variable) {
  unit_colors <- aggregate(data.frame(som_data[, variable]),
                           by = list(som_model$unit.classif),
                           FUN = mean,
                           simplify = TRUE)
  
  plot(som_model,
       type = "property",
       shape = "straight",
       property = unit_colors[, 2],
       main = variable,
       palette.name = heatColors)
}

# Plot all the variables
par(mfrow = c(2, 2))
for (i in seq_along(colnames(som_data))) {
  plot_som(colnames(som_data)[i])
}

# Plot the quality plots
plot(som_model,
     type = "counts",
     shape = "straight",
     main = "Node Counts")

plot(som_model,
     type = "quality",
     shape = "straight",
     main = "Node Quality/Distance")

plot(som_model,
     type = "dist.neighbours",
     shape = "straight",
     main = "SOM neighbour distances",
     palette.name = grey.colors)

plot(som_model,
     shape = "straight",
     type = "codes")

# Cluster and plot with cluster boundaries
par(mfrow = c(1, 1))
palette <- c("#F25F73", "#98C94C", "#888E94", "#33A5BF", "#F7D940")
som_cluster <- cutree(hclust(dist(as.data.frame(som_model$codes))), 4)

plot(som_model,
     type = "mapping",
     bgcol = palette[som_cluster],
     main = "Clusters",
     shape = "straight")

add.cluster.boundaries(som_model, som_cluster)
