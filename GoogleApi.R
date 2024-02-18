library("gmapsdistance")
set.api.key("AIzaSyB76lzKhfu1lbvbArWYlPL8R21xMcgqCmo")

library(readr)
library(dplyr)

#Load Data
gkgw <- read_csv("GKGW data.csv") %>% 
  mutate(across(c(2, 3), as.character))

#Prepare Destinations
destination <- unique(c(gkgw[,2], gkgw[,3])) #Extract 2nd and 3rd column
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

save(time, dist, dest, file="gokidgoweb.Rda")

load(file="gokidgoweb.Rda")

for (j in 2:3) gkgw[,j] <- apply(outer(gkgw[,j],dest,"=="),1,which)

# https://towardsdatascience.com/how-does-uber-use-clustering-43b21e3e6b7d