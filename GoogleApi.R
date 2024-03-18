library("gmapsdistance")
#set.api.key("Enter api Key")


library(readr)
library(dplyr)

#Load Data
gkgw <- read_csv("GKGWdata.csv") %>% 
  mutate(across(c(2, 3), as.character))

#Prepare Destinations
destination <- unique(unlist(c(gkgw[,2], gkgw[,3])))
destination <- destination[-5]
dest <- gsub(" ", "+", destination) #format properly for api

#Initialize matrices
dist <- time <- matrix(0, nr=length(dest), nc=length(dest))

#Calculate distances and times
for (i in 2:length(dest)) {
  for (j in 1:(i-1)) {
    DaT <- gmapsdistance(origin = dest[i], destination = dest[j], mode = "driving") #api function
    time[i, j] <- DaT$Time
    dist[i, j] <- DaT$Distance
  }
}

# Fill in other half of matrix
time <- time + t(time)
dist <- dist + t(dist)

route_data <- list(distance = dist, time = time, destinations = dest)
save(route_data, file="gokidgoweb.Rda")