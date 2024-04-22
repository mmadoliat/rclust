library(readr)
library(ggplot2)
library(ggmap)
library(ggforce) 
library(dplyr)

load("pdist.Rda")
load("ptime.Rda")
load("data.Rda")


hc <- test::rhclust(pdist, ptime, 0.5, data = data)

plot(hc)

library(googleway)
set_key("AIzaSyAn0ucCBVnBAOvhO2KUbN_gxW7bt6umiuw")

findLocation <- function(loc) {
  first_char <- substr(loc, 1, 1)
  num <- as.numeric(substr(loc, 2, nchar(loc)))
  if (first_char == "p") { # If it starts with 'p', use 'num' to index 'formatted_home'
    location <- data$formatted_home[num]
  } else if (first_char == "d") { # If it starts with 'd', use 'num' to index 'formatted_destination'
    location <- data$formatted_destination[num]
  }
  return (location)
}

getDetails <- function(numClusters) {
  if (numClusters == 5) {
    origins <- list()
    destinations <- list()
    
    # Loop over each sublist to extract origins and destinations
    for (i in 1:5) {
      # Extract the origin from the first element if it exists
      if (length(hc$merge.route[[i]]) >= 1) {
        origins[[i]] <- findLocation(hc$merge.route[[i]][[1]])
      } else {
        origins[[i]] <- NULL  # Placeholder in case there's no first element
      }
      
      # Extract the destination from the fourth element if it exists
      if (length(hc$merge.route[[i]]) >= 4) {
        destinations[[i]] <- findLocation(hc$merge.route[[i]][[4]])
      } else {
        destinations[[i]] <- NULL  # Placeholder in case there's no fourth element
      }
    }
   
    waypoint_vectors <- list()
    
    # Loop over each of the first five sublists
    for (i in 1:5) {
      # Check if the sublist has at least three elements to avoid indexing errors
      if (length(hc$merge.route[[i]]) >= 3) {
        # Create a vector of the second and third elements using findLocation
        waypoints <- c(findLocation(hc$merge.route[[i]][[2]]), findLocation(hc$merge.route[[i]][[3]]))
        # Add this vector to the list
        waypoint_vectors[[i]] <- waypoints
      } else {
        # If there are not enough elements in the sublist, add a NULL or suitable placeholder
        waypoint_vectors[[i]] <- NULL
      }
    }
    return(list(origins = origins, destinations = destinations, waypoints = waypoint_vectors))
  }
}
locations <- getDetails(5)

library(googleway)

directions <- google_directions(origin = locations$origins[[1]], 
                                destination = locations$destinations[[1]],
                                mode = "driving",
                                waypoints = locations$waypoints[[1]],
                                alternatives = TRUE,
                                key = "AIzaSyAn0ucCBVnBAOvhO2KUbN_gxW7bt6umiuw")

print(directions$routes)

map <- google_map(key = "AIzaSyAn0ucCBVnBAOvhO2KUbN_gxW7bt6umiuw")
map <- add_polylines(map, data = directions$routes$overview_polyline, polyline_id = "route")

