#Buildings Rotation and Translation
#First introduce your own coordinate system
#Rotate and scale your points according to this system
#Final take-away building0.df, building1.df, building2.df
# PRE-PROCESSING----

# Building 0----

#  Remove ZV
building0.df <- subset(train.df,BUILDINGID == "0")
temp <- building0.df[,406]
building0.df <- building0.df[,-(zerovariance(building0.df))]
building0.df <- cbind(building0.df,temp)
temp <- cbind(building0.df[,c(164,165)],building0.df[,-c(164,165)])

#  Rotate
M2 <- t(rotm %*% (
  t(temp[,1:2])-c(temp[1,1],temp[1,2])
)+c(temp[1,1],temp[1,2]))

#  Save back to original dataframe
temp[,1] <- M2[,1]
temp[,2] <- M2[,2]
building0.df <- temp

#  Translate
temp <- data.frame( lapply(building0.df[,c(1,2)], translate) ,building0.df[,-c(1,2)])
building0.df <- temp 
plot(temp$LATITUDE,temp$LONGITUDE,col = "red", pch = 16)

# Building 1----

#  Remove ZV
building1.df <- subset(train.df,BUILDINGID == "1")
temp <- building1.df[,406]
building1.df <- building1.df[,-(zerovariance(building1.df))]
building1.df <- cbind(building1.df,temp)
temp <- cbind(building1.df[,c(184,185)],building1.df[,-c(184,185)])

#  Rotate
M2 <- t(rotm %*% (
  t(temp[,1:2])-c(temp[1,1],temp[1,2])
)+c(temp[1,1],temp[1,2]))

#  Save back to original dataframe
temp[,1] <- M2[,1]
temp[,2] <- M2[,2]
building1.df <- temp
#  Translate
temp <- data.frame( lapply(building1.df[,c(1,2)], translate) ,building1.df[,-c(1,2)])
building2.df <- temp 
plot(temp$LATITUDE,temp$LONGITUDE,col = "red", pch = 16)

# Building 2----

#  Remove ZV
building2.df <- subset(train.df,BUILDINGID == "2")
temp <- building2.df[,406]
building2.df <- building2.df[,-(zerovariance(building2.df))]
building2.df <- cbind(building2.df,temp)
temp <- cbind(building2.df[,c(178,179)],building2.df[,-c(178,179)])

#  Rotate
M2 <- t(rotm %*% (
  t(temp[,1:2])-c(temp[1,1],temp[1,2])
)+c(temp[1,1],temp[1,2]))

#  Save back to original dataframe
temp[,1] <- M2[,1]
temp[,2] <- M2[,2]
building2.df <- temp
#  Translate
temp <- data.frame( lapply(building2.df[,c(1,2)], translate) ,building2.df[,-c(1,2)])
building2.df <- temp 
plot(temp$LATITUDE,temp$LONGITUDE,col = "red", pch = 16)

#  Removing Redundant Rows

#  Building 0
keep <- apply(building0.df[,3:164], 1, function(x) length(unique(x)) != 1)
building0.df <- building0.df[keep, ]

#  Building 1
keep <- apply(building1.df[,3:184], 1, function(x) length(unique(x)) != 1)
building1.df <- building1.df[keep, ]

#  Building 2
keep <- apply(building2.df[,3:179], 1, function(x) length(unique(x)) != 1)
building2.df <- building2.df[keep, ]

#NEXT STEPS 1)Rotation of all parallel to x axis
#           2)Changing the scale
#           3)Distance Meters
#           4)Start Analysis
#           5)Boundary creation, find dimensions and scape
#           6)Superimpose on Google MAPS
#           7)Explore ipft PACKAGE
#           8)Get FLoor Plan From Gabriel
#           9)Invidual Analysis of BUILDINGS,FACTOR BUILDING.FLOOR.SPACE

# FUNCTIONS AND ROTATION FORMULA----

zerovariance.col <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}

translate <- function(x) {
 y<- x-min(x)
 y
}

# Start of Rotation

#  Manual Calculation through interactive plotting

alpha <- -atan((55.65032 - 42.68899)/(68.84382 -  45.33731))

#  Interactive plotting
#  "gatepoints" allows you to draw a gate returning your points of interest.
#  Plot in a separate x11 window by first opening a new x11 device:

library(gatepoints)
X11()
plot(M2, col = "red", pch = 16)
selectedPoints <- fhs(M2)

#rotation matrix

rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)

#  Shift, Rotate, Shift back
M2 <- t(rotm %*% (
  t(temp[,1:2])-c(temp[1,1],temp[1,2])
)+c(temp[1,1],temp[1,2]))