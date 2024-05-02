library("gmapsdistance")
library(readr)
library(dplyr)

set.api.key("AIzaSyAn0ucCBVnBAOvhO2KUbN_gxW7bt6umiuw")
#Load Data
data <- read_csv("rclust-data.csv")
#Reformat starting location zipcode
data$ZIP_CODE_home <- substr(data$ZIP_CODE_home, 1, 5)
#Format address in Google's preference
data$formatted_home <- paste(data$HSE_NBR_home, data$STREET_home, data$STTYPE_home, data$ZIP_CODE_home, sep = "+")
data$formatted_destination <- paste(data$HSE_NBR, data$STREET, data$STTYPE, data$ZIP_CODE, sep = "+")
data$formatted_home <- gsub(" ", "+", data$formatted_home)
data$formatted_destination <- gsub(" ", "+", data$formatted_destination)

#Extract destination and location names as vectors
destinations <- c(as.vector(data$formatted_home), as.vector(data$formatted_destination))
location_names <- c(paste("p", seq_along(data$formatted_home), sep = ""), #ex:p1, d1
                    paste("d", seq_along(data$formatted_destination), sep = ""))


# Initialize matrices for distances and times
dist <- time <- matrix(0, nr=length(destinations), nc=length(destinations), dimnames = list(location_names, location_names))


#Fill in the left diagonal half of the matrix
for (i in 2:length(destinations)) {
  for (j in 1:(i-1)) {
      DaT <- gmapsdistance(origin = destinations[i], destination = destinations[j], mode = "driving") #api call
      time[i, j] <- DaT$Time
      dist[i, j] <- DaT$Distance
  }
}

# Fill in other half of matrix
ptime <- time + t(time)
pdist <- dist + t(dist)

save(data, file="data.Rda")
save(ptime, file='ptime.Rda')
save(pdist, file='pdist.Rda')
