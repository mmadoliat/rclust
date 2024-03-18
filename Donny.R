load("gokidgoweb.Rda")
library(readr)
library(ggplot2)
library(ggmap)
library(ggforce) 
library(dplyr)

z_score_standardization <- function(x) {
  return ((x - mean(x)) / sd(x))
}

normalized_distance <- z_score_standardization(route_data$distance)
normalized_time <- z_score_standardization(route_data$time)

combined_metric <- data.frame(normalized_distance, normalized_time)

hc <- hclust(dist(normalized_time), method = "complete")

plot(hc)
plot(hc$height, type = "b")

# Read the geolocation data
geolocations <- read.csv("Geolocations.csv")

# Filter out points not in Wisconsin
geolocations_filtered <- geolocations %>%
  filter(latitude > 42, latitude < 47, longitude > -93, longitude < -86)

route_data$cluster <- cutree(hc, k = 7)

#add long and lat data to route_data
route_data$latitude <- geolocations_filtered$latitude
route_data$longitude <- geolocations_filtered$longitude


route_data <- as.data.frame(route_data)
cluster_sizes <- route_data %>%
  group_by(cluster) %>%
  summarise(size = n())

final_data <- aggregate(cbind(latitude, longitude) ~ cluster, data=route_data, mean)
final_data <- merge(final_data, cluster_sizes, by = "cluster")

focused_bbox <- c(left = -90, bottom = 42.5, right = -86, top = 43.7)

# Get a Google map for Wisconsin
focused_map <- get_googlemap(center = c(lon = mean(focused_bbox[c("left", "right")]), 
                                        lat = mean(focused_bbox[c("bottom", "top")])), 
                             zoom = 11, scale = 2)

ggmap(focused_map) +
  geom_point(data = final_data, aes(x = longitude, y = latitude, size = size), color = "red") +
  scale_size_continuous(range = c(3, 10)) + # Adjust the min and max size as needed
  theme_minimal() +
  labs(title = "Cluster Centroids and Sizes in Wisconsin", x = "Longitude", y = "Latitude")

