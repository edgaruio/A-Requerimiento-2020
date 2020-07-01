# Test distancias

#define random data
centers<-data.frame(x=c(44,44, 50, 50), y=c(44, 50, 44, 50))
pts<-data.frame(x=runif(25, 40, 55), y=runif(25, 40, 55))

#allocate space
distance<-matrix(-1, nrow = length(pts$x), ncol= length(centers$x))

library(geosphere)
#calculate the dist matrix - the define centers to each point
#columns represent centers and the rows are the data points
dm<-apply(data.frame(1:length(centers$x)), 1, function(x){ replace(distance[,x], 1:length(pts$x), distGeo(centers[x,], pts))})

#find the column with the smallest distance
closestcenter<-apply(dm, 1, which.min)
length(closestcenter)

#color code the original data for verification
colors<-c("black", "red", "blue", "green")
plot(pts , col=colors[closestcenter], pch=19) 
