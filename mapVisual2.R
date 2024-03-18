# Load necessary libraries
library(ggplot2)
library(ggmap)
library(dplyr)

# Set your Google API key
register_google(key = "AIzaSyAn0ucCBVnBAOvhO2KUbN_gxW7bt6umiuw")

# Read the geolocation data
geolocations <- read.csv("Geolocations.csv")

# Filter out points not in Wisconsin
geolocations_filtered <- geolocations %>%
  filter(latitude > 42, latitude < 47, longitude > -93, longitude < -86)

# Define the bounding box for Wisconsin
focused_bbox <- c(left = -90, bottom = 42.5, right = -86, top = 43.7)

# Get a Google map for Wisconsin
focused_map <- get_googlemap(center = c(lon = mean(focused_bbox[c("left", "right")]), 
                                        lat = mean(focused_bbox[c("bottom", "top")])), 
                             zoom = 11, scale = 2)

# Plot the map with filtered data points
ggmap(focused_map) +
  geom_point(data = geolocations_filtered, aes(x = longitude, y = latitude), color = "red", size = 3) +
  theme_minimal() +
  labs(title = "Geolocation Points in Focused Area of Wisconsin", x = "Longitude", y = "Latitude")
