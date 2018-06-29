ggplot() +
  geom_point(data=building0.floor.val.df, aes(building0.floor.val.df$LATITUDE, building0.floor.val.df$LONGITUDE, color= "validation")) +
  geom_point(aes(predictions.svm.b0.lat,predictions.svm.b0.long,color= "prediction"))+labs(title = "BUILDING 0")+xlab("Latittude")+ylab("Longitude")

ggplot() +
  geom_point(data=building2.floor.val.df, aes(building2.floor.val.df$LATITUDE, building2.floor.val.df$LONGITUDE, color= "validation")) +
  geom_point(aes(predictions.svm.b2.lat,predictions.svm.b2.long,color= "prediction"))+labs(title = "BUILDING 2")+xlab("Latittude")+ylab("Longitude")

ggplot() +
  geom_point(data=building1.floor.val.df, aes(building1.floor.val.df$LATITUDE, building1.floor.val.df$LONGITUDE, color= "validation")) +
  geom_point(aes(predictions.svm.b1.lat,predictions.svm.b1.long,color= "prediction"))+labs(title = "BUILDING 1")+xlab("Latittude")+ylab("Longitude")




# Error Correction
# Error due to 3 things :
# Overfit
# Noise
# Missing Data
# Weight your errors,check which error points
meanoferror <- function(actual,predicted){
  error <- actual - predicted
  median(error)
}

building0.lat.error <- meanoferror(building0.floor.val.df$LATITUDE,predictions.svm.b0.lat)
building1.lat.error <- meanoferror(building1.floor.val.df$LATITUDE,predictions.svm.b1.lat)
building2.lat.error <- meanoferror(building2.floor.val.df$LATITUDE,predictions.svm.b2.lat)


building0.long.error <- meanoferror(building0.floor.val.df$LONGITUDE,predictions.svm.b0.long)
building1.long.error <- meanoferror(building1.floor.val.df$LONGITUDE,predictions.svm.b1.long)
building2.long.error <- meanoferror(building2.floor.val.df$LONGITUDE,predictions.svm.b2.long)

predictions.svm.b0.long <- building0.long.error+predictions.svm.b0.long
predictions.svm.b1.long <- building1.long.error+predictions.svm.b1.long
predictions.svm.b2.long <- building2.long.error+predictions.svm.b2.long

predictions.knn.b2.long <- building2.long.error+predictions.knn.b2.long


predictions.svm.b0.lat <- building0.lat.error+predictions.svm.b0.lat
predictions.svm.b1.lat <- building1.lat.error+predictions.svm.b1.lat
predictions.svm.b2.lat <- building2.lat.error+predictions.svm.b2.lat


# Error
error
#GOOGLE MAP----
#Universitat of Jaume I:
univ <- c(-0.067417, 39.992871)
map1 <- get_map(univ, zoom = 20, scale = 1)
ggmap(map1)

plotlyTrainingData <- plot_ly(type = "scatter3d" , mode = "markers" , x = trainingData.df$LATITUDE, y = trainingData.df$LONGITUDE, z = as.matrix(xyz), color = trainingData.df$FLOOR, colors = c('#d600fc','green' ,'#0C4B8E')) %>%
  add_trace(type = "scatter3d" , mode = "markers" , x = trainingData.df$LATITUDE, y = trainingData.df$LONGITUDE, z = as.matrix(xyz), colors = "black") %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Latitude'),
                      yaxis = list(title = 'Longitude'),
                      zaxis = list(title = 'Floor'))) %>% add_surface()

plotlyTrainingData
#SPATIAL ANALYSIS----
xy <- building0[building0$FLOOR==0,c(405,406)]
write.csv(xy,"hello.csv")
#spdf <- SpatialPointsDataFrame(coords = xy, data = copy.df,
#proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
x<-copy.df$LONGITUDE
y<-copy.df$LATITUDE
library(proj4)
proj4string <- "+proj=utm +zone=30T +south +ellps=ETRS89 +datum=WGS84 +units=m +no_defs "

# Source data
xy <- data.frame(x=x, y=y)

# Transformed data
pj <- project(xy, proj4string, inverse=TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)
print(latlon)
plot(pj$x,pj$y,lwd=0.2,cex=0.1)
ggplot(xy, aes(xy$x,xy$y)) + # the data
  geom_point()
ipfPlotLoc(copy.df[, 405:406])


WAP_Location<-ipfEstimateBeaconPositions(copy.df[,1:404], xyz, method = "wcentroid",
                                         rssirange = c(-100, 0), norssi = -105)
xyz<-as.matrix(xy)
xyz<-cbind(xy,copy.df$FLOOR)
colnames(xyz)<-c("x","y","z")

write.csv(xyz,"xyz.csv")
names(building0)
building0.xyz<-building0[,405:407]
building1.xyz<-building1[,405:407]
building2.xyz<-building2[,405:407]
write.csv(building0.xyz,"b0xyz.csv")
write.csv(building1.xyz,"b1xyz.csv")
write.csv(building2.xyz,"b2xyz.csv")

#Spatial conversions
#coords<-read.csv("D:/coords_utms.csv")
coordinates(building0.xyz)<-c("LONGITUDE","LATITUDE","FLOOR")
coords_man<-SpatialPoints(coords, proj4string = CRS("++proj=utm +zone=14"))
longlat<-spTransform(coords_man,CRS("+proj=longlat"))



#PLotly
library(plotly)
x<-c(building0.xyz[,"LONGITUDE"])
y<-c(building0.xyz[,"LATITUDE"])
z<-c(building0.xyz[,"FLOOR"])
df.list <- list(x = as.numeric(x),
                y = as.numeric(y),
                z = as.numeric(z))

df.dataframe <- data.frame(x = as.numeric(x),
                           y = as.numeric(y),
                           z = as.numeric(z))


# Works fine
plot_ly(df.dataframe, x = x, y = y, z = z, size = 5249, type = "surface")

# Doesn't work
# dimension of the z parameter != dim(x) x dim(y)
plot_ly(df.dataframe, x = x, y = y, z = z, type = "surface")