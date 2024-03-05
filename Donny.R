load("gokidgoweb.Rda")
library(readr)

z_score_standardization <- function(x) {
  return ((x - mean(x)) / sd(x))
}

normalized_distance <- z_score_standardization(route_data$distance)
normalized_time <- z_score_standardization(route_data$time)

combined_metric <- data.frame(normalized_distance, normalized_time)

hc <- hclust(dist(normalized_time), method = "complete")

plot(hc)
plot(hc$height, type = "b")

route_data$cluster <- cutree(hc, k = 7)