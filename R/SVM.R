# Support Vector Machine

#  Building----
#   Using normalised data
normalised.df <- as.data.frame(lapply(train.df[, 1:404], normalize))
normalised.df <- cbind(normalised.df,train.df[,c("LATITUDE","LONGITUDE",
                                                 "FLOOR","BUILDINGID")])
normalised.df <- unique(normalised.df)

normalised.df <- normalised.df %>% 
  group_by(.dots=c(WAPS,"FLOOR","BUILDINGID")) %>% 
  summarise(
  LATITUDE   = mean(LATITUDE),
  LONGITUDE  = mean(LONGITUDE)
)

# intrain <- createDataPartition(y = normalised.df$BUILDINGID, p= 0.7, list = FALSE)
# train.svm <- normalised.df[intrain,]
# test.svm <- normalised.df[-intrain,]

trctrl <- trainControl(method = "repeatedcv", number = 5)

set.seed(9890)

svm.BUILDING <- caret::train(f, 
                    data = normalised.df, 
                    method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("zv"),
                    tuneLength = 10)

plot(svm.BUILDING)
predict.svm <- predict(svm.BUILDING,test.svm)
confusionMatrix(test.svm$BUILDINGID,predict.svm)

#   Using melted.df to predict BUILDING ID----

melt.building.SVM.df <- melted.df[,c("variable","value","BUILDINGID")]
melt.building.SVM.df <- unique(melt.building.SVM.df)

melt.building.SVM.df <- melt.building.SVM.df %>% 
  group_by(.dots=c("variable","BUILDINGID")) %>% 
  summarise(
    value   = mean(value)
  )

#   Removing weaker WAPS prevalent in other buildings
melt.building.SVM.df <- melt.building.SVM.df %>% 
  group_by(variable) %>% slice(which.max(value))

#   Rather than dummifying, assigning a numeric value to diff. WAPs

melt.building.SVM.df$variable <- 
  gsub("WAP", "", melt.building.SVM.df$variable)
melt.building.SVM.df$variable <- 
  as.numeric(as.character(melt.building.SVM.df))


set.seed(9890)
trctrl <- trainControl(method = "repeatedcv", number = 10,
                       repeats = 2, verboseIter = TRUE)

set.seed(3195)

melt.svm.BUILDING <- caret::train(BUILDINGID~.-value , 
                             data = melt.building.SVM.df, 
                             method = "svmLinear",
                             trControl=trctrl,
                             tuneLength = 10)

plot(melt.svm.BUILDING)



#  Floor----

#   building0----

#   Pre-processing for SVM 

building0.svm.df <- as.data.frame(lapply(building0.df[, 3:164], normalize))
building0.svm.df <- cbind(building0.svm.df,building0.df[,c(1,2,165)])
building0.svm.df$BUILDINGID <- NULL
building0.svm.df$FLOOR <- factor(building0.svm.df$FLOOR, levels=c(1,2,3), ordered=F)
#   Create Partition

set.seed(9890)
intrain <- createDataPartition(y = building0.svm.df$FLOOR, p= 0.7, list = FALSE)
train.svm.b0 <- building0.svm.df[intrain,]
test.svm.b0 <- building0.svm.df[-intrain,]

#   Modelling (Just Run this for predictions)
trctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

set.seed(3195)

svm.b0.floor <- caret::train(FLOOR ~ .-LONGITUDE -LATITUDE, 
                    data = building0.svm.df,
                    preProcess = c("zv"),
                    method = "svmRadial",
                    trControl=trctrl,
                    tuneLength = 10)

plot(svm.b0.floor)
predict.svm.b0 <- predict(svm.b0.floor,test.svm.b0)
confusionMatrix(test.svm.b0$FLOOR,predict.svm.b0)

#   building1----

#   Pre-processing for SVM 

building1.svm.df <- as.data.frame(lapply(building1.df[, 3:184], normalize))
building1.svm.df <- cbind(building1.svm.df,building1.df[,c(192,185)])
building1.svm.df$BUILDINGID <- NULL

#   Create Partition

set.seed(9890)
intrain <- createDataPartition(y = building1.svm.df$FLOOR, p= 0.7, list = FALSE)
train.svm.b1 <- building1.svm.df[intrain,]
test.svm.b1 <- building1.svm.df[-intrain,]

#   Modelling (Just Run this for predictions)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)


set.seed(3195)

trctrl <- trainControl(method = "none", verboseIter = TRUE)

svm.b1.floor <- caret::train(FLOOR ~ .-LONGITUDE -LATITUDE, 
                             data = building1.svm.df,
                             preProcess = c("zv"),
                             method = "svmRadial",
                             trControl=trctrl,
                             tuneLength = 10)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

svm.b1.floor <- caret::train(FLOOR ~ .-LONGITUDE -LATITUDE, 
                          data = building1.svm.df, 
                          method = "svmRadial",
                          trControl=trctrl,
                          tuneLength = 10)

plot(svm.b1.floor)
predict.svm.b1 <- predict(svm.b1.floor,test.svm.b1)
confusionMatrix(test.svm.b1$FLOOR,predict.svm.b1)



#   building2----

#   Pre-processing for SVM 
#    normalize WAP
building2.svm.df <- as.data.frame(lapply(building2.df[, 3:178], normalize))

building2.svm.df <- cbind(building2.svm.df,building2.df[,c(186,179)])
building2.svm.df$BUILDINGID <- NULL
#   Create Partition

set.seed(9890)
intrain <- createDataPartition(y = building2.svm.df$FLOOR, p= 0.7, list = FALSE)
train.svm.b2 <- building2.svm.df[intrain,]
test.svm.b2 <- building2.svm.df[-intrain,]

#   Modelling (Just Run this for predictions)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

set.seed(3195)

svm.b2.floor <- caret::train(FLOOR ~ .-LONGITUDE -LATITUDE, 
                             data = building2.svm.df, 
                             method = "svmRadial",
                             preProcess=c("zv"),
                             trControl=trctrl,
                             tuneLength = 10)

plot(svm.b2.floor)
predict.svm.b2 <- predict(svm.b2.floor,test.svm.b2)
confusionMatrix(test.svm.b2$FLOOR,predict.svm.b2)

#  Latitude----
#   building0----
#   Binding latitude and longitude

building0.svm.df <- cbind(building0.svm.df,building0.df[,c(1,2)])

#   Create Partition
#    Use building0.floor.df because floor is dummified
set.seed(9890)
intrain <- createDataPartition(y = building0.svm.df$LATITUDE, p= 0.7, list = FALSE)
train.svm.b0.lat <- building0.svm.df[intrain,]
test.svm.b0.lat <- building0.svm.df[-intrain,]

#   Modelling (Just Run this for predictions)
trctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

set.seed(3195)

svm.b0.latitude <- caret::train(LATITUDE ~ . -LONGITUDE, 
                          data = building0.floor.df,
                          preProcess = c("zv"),
                          method = "svmRadial",
                          trControl=trctrl,
                          tuneLength = 10)

plot(svm.b0.latitude)
predict.svm.b0.lat <- predict(svm.b0.latitude,test.svm.b0.lat)
postResample(test.svm.b0.lat$LATITUDE,predict.svm.b0.lat)


#   building1----
#   Binding latitude and longitude

building1.svm.df <- cbind(building1.svm.df,building1.df[,c(1,2)])

#   Create Partition

set.seed(9890)
intrain <- createDataPartition(y = building1.svm.df$LATITUDE, p= 0.7, list = FALSE)
train.svm.b1.lat <- building1.svm.df[intrain,]
test.svm.b1.lat <- building1.svm.df[-intrain,]

#   Modelling (Just Run this for predictions)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

set.seed(3195)

svm.b1.latitude <- caret::train(LATITUDE ~ . -LONGITUDE, 
                             data = building1.floor.df,
                             preProcess = c("zv"),
                             method = "svmRadial",
                             trControl=trctrl,
                             tuneLength = 10)

plot(svm.b1.latitude)
predict.svm.b1.lat <- predict(svm.b1.latitude,test.svm.b1.lat)
postResample(test.svm.b1.lat$LATITUDE,predict.svm.b1.lat)

#   building2----
#   Binding latitude and longitude

building2.svm.df <- cbind(building2.svm.df,building2.df[,c(1,2)])

#   Create Partition

set.seed(9890)
intrain <- createDataPartition(y = building2.svm.df$LATITUDE, p= 0.7, list = FALSE)
train.svm.b2.lat <- building2.svm.df[intrain,]
test.svm.b2.lat <- building2.svm.df[-intrain,]

#   Modelling (Just Run this for predictions)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

set.seed(3195)

svm.b2.latitude <- caret::train(LATITUDE ~ .-LONGITUDE, 
                                data = building2.floor.df,
                                preProcess = c("zv"),
                                method = "svmRadial",
                                trControl=trctrl,
                                tuneLength = 10)

plot(svm.b2.latitude)
predict.svm.b2.lat <- predict(svm.b2.latitude,test.svm.b2.lat)
postResample(test.svm.b2.lat$LATITUDE,predict.svm.b2.lat)
#  Longitude----

#   building0----
#   Create Partition

DummyVar <- dummyVars("~FLOOR", data = building0.svm.df, fullRank=T)
DummyVarDF <- data.frame(predict(DummyVar, newdata = building0.svm.df))
building0.floor.df<-cbind(building0.svm.df, DummyVarDF)
building0.floor.df$FLOOR <- NULL

set.seed(9890)
intrain <- createDataPartition(y = building0.svm.df$LONGITUDE, p= 0.7, list = FALSE)
train.svm.b0.long <- building0.svm.df[intrain,]
test.svm.b0.long <- building0.svm.df[-intrain,]

#   Modelling (Just Run this for predictions)
trctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

set.seed(3195)

svm.b0.longitude <- caret::train(LONGITUDE ~ . -LATITUDE , 
                                data = building0.floor.df,
                                preProcess = c("zv"),
                                method = "svmRadial",
                                trControl=trctrl,
                                tuneLength = 10)

trctrl <- trainControl(method = "none", verboseIter = TRUE)

svm.b0.longitude <- caret::train(LONGITUDE ~ . -LATITUDE, 
                                 data = building0.svm.df,
                                 preProcess = c("zv"),
                                 method = "svmRadial",
                                 trControl=trctrl)

plot(svm.b0.longitude)
predict.svm.b0.long <- predict(svm.b0.longitude,test.svm.b0.long)
postResample(test.svm.b0.long$LONGITUDE,predict.svm.b0.long)

#   building1----
#   Create Partition

DummyVar <- dummyVars("~FLOOR", data = building1.svm.df, fullRank=T)
DummyVarDF <- data.frame(predict(DummyVar, newdata = building1.svm.df))
building1.floor.df<-cbind(building1.svm.df, DummyVarDF)
building1.floor.df$FLOOR <- NULL

set.seed(9890)
intrain <- createDataPartition(y = building1.svm.df$LONGITUDE, p= 0.7, list = FALSE)
train.svm.b1.long <- building1.svm.df[intrain,]
test.svm.b1.long <- building1.svm.df[-intrain,]

#   Modelling (Just Run this for predictions)
trctrl <- trainControl(method = "none", verboseIter = TRUE)

set.seed(3195)

svm.b1.longitude <- caret::train(LONGITUDE ~ . -LATITUDE, 
                                 data = building1.floor.df,
                                 preProcess = c("zv"),
                                 method = "svmRadial",
                                 trControl=trctrl,
                                 tuneLength = 10)

svm.b1.longitude <- caret::train(LONGITUDE ~ . -LATITUDE, 
                                 data = building1.svm.df,
                                 preProcess = c("zv"),
                                 method = "svmRadial",
                                 trControl=trctrl)

plot(svm.b1.longitude)
predict.svm.b1.long <- predict(svm.b1.longitude,test.svm.b1.long)
postResample(test.svm.b1.long$LONGITUDE,predict.svm.b1.long)

#   building2----
#   Create Partition

DummyVar <- dummyVars("~FLOOR", data = building2.svm.df, fullRank=T)
DummyVarDF <- data.frame(predict(DummyVar, newdata = building2.svm.df))
building2.floor.df<-cbind(building2.svm.df, DummyVarDF)
building2.floor.df$FLOOR <- NULL

set.seed(9890)
intrain <- createDataPartition(y = building2.svm.df$LONGITUDE, p= 0.7, list = FALSE)
train.svm.b2.long <- building2.svm.df[intrain,]
test.svm.b2.long <- building2.svm.df[-intrain,]

#   Modelling (Just Run this for predictions)
trctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

set.seed(3195)

svm.b2.longitude <- caret::train(LONGITUDE ~ . -LATITUDE, 
                                 data = building2.floor.df,
                                 preProcess = c("zv"),
                                 method = "svmRadial",
                                 trControl=trctrl,
                                 tuneLength = 10)

plot(svm.b2.longitude)
predict.svm.b2.long <- predict(svm.b2.longitude,test.svm.b2.long)
postResample(test.svm.b2.long$LONGITUDE,predict.svm.b2.long)