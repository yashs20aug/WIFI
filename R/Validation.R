# Validation Set Manipulation for testing----

validation.df <-unique(validationData.df)

# Changing values higher than -90 to 100

validation.df[validation.df<(-90)]<- 100
validation.df<-validation.df[,c(column.v)]
validation.df$LONGITUDE <- validationData.df$LONGITUDE # Restoring LONGITUDE

#  Removing Redundant Rows

keep <- apply(validation.df[,1:404], 1, function(x) length(unique(x)) != 1)
validation.df <- validation.df[keep, ]

#  Changing 100 to -105

validation.df[validation.df==100]<- (-105)

#  Reducing attributes by melting----

meltv.df<-melt(validation.df,
                id = c("LONGITUDE" ,"LATITUDE", 
                       "FLOOR", "BUILDINGID",
                       "SPACEID", "RELATIVEPOSITION",
                       "USERID", "PHONEID"))

meltv.df <- meltv.df[ ! meltv.df$value %in% 100, ]

ggplot(meltv.df, aes(x =value)) + #Parabolic at Vertex(-29,0)
  geom_density(aes(y = ..count..), fill = "lightgray") +
  scale_x_continuous(breaks = round(seq(min(meltv.df$value),
                                        max(meltv.df$value), by = 10),1)) +
  labs(y="Count", x = "RSSI values") +
  ggtitle("After removing No Signals(100dBm)and Weak Signals(< -90dBm)")

meltv.df <- meltv.df %>% group_by(.dots=c("variable","BUILDINGID")) %>% 
  summarise(
    value   = mean(value)
  )

#    Dummify----
building0.norm.val.df$FLOOR<- as.factor(building0.norm.val.df$FLOOR)
building1.norm.val.df$FLOOR<- as.factor(building1.norm.val.df$FLOOR)
building2.norm.val.df$FLOOR<- as.factor(building2.norm.val.df$FLOOR)

DummyVar <- dummyVars("~FLOOR", data = building0.norm.val.df, fullRank=T)
DummyVarDF <- data.frame(predict(DummyVar, newdata = building0.norm.val.df))
building0.floor.val.df<-cbind(building0.norm.val.df, DummyVarDF)
building0.floor.val.df$FLOOR <- NULL

DummyVar <- dummyVars("~FLOOR", data = building1.norm.val.df, fullRank=T)
DummyVarDF <- data.frame(predict(DummyVar, newdata = building1.norm.val.df))
building1.floor.val.df<-cbind(building1.norm.val.df, DummyVarDF)
building1.floor.val.df$FLOOR <- NULL

DummyVar <- dummyVars("~FLOOR", data = building2.norm.val.df, fullRank=T)
DummyVarDF <- data.frame(predict(DummyVar, newdata = building2.norm.val.df))
building2.floor.val.df<-cbind(building2.norm.val.df, DummyVarDF)
building2.floor.val.df$FLOOR <- NULL


# BUILDING----
#  SVM
#   melted dataframe

melt.predict.val.svm <- predict(melt.svm.BUILDING ,meltv.df)
confusionMatrix(meltv.df$BUILDINGID,melt.predict.val.svm)

predict.val.svm <- predict(svm.BUILDING,validation.df.normalized)
confusionMatrix(validation.df.normalized$BUILDINGID,predict.val.svm)

#  KNN

validation.df.normalized <- lapply(validation.df[, 1:404], normalize)
validation.df.normalized <- as.data.frame(validation.df.normalized)
validation.df.normalized <- cbind(validation.df.normalized,validation.df[,405:412])

#   Create a new dataframe,selecting only features from the training dataset
val.knn.df <- validation.df.normalized %>% group_by(.dots=c(WAP.predictors,"FLOOR","BUILDINGID")) %>% summarise(
  LATITUDE   = mean(LATITUDE),
  LONGITUDE  = mean(LONGITUDE)
)

predict.val.knn <- predict(knnfit,val.knn.df)
confusionMatrix(val.knn.df$BUILDINGID,predict.val.knn)#98.44%

#  Random Forest

predict.val.rf <- predict(rf,validation.df)
confusionMatrix(validation.df$BUILDINGID,predict.val.rf)

# FLOOR----
#   Pre-processing for SVM 

building0.norm.val.df <- as.data.frame(lapply(building0.val.df[, 4:165], normalize))
building0.norm.val.df <- cbind(building0.norm.val.df,building0.val.df[,c(1,2,3)])

building1.norm.val.df <- as.data.frame(lapply(building1.val.df[, 4:185], normalize))
building1.norm.val.df <- cbind(building1.norm.val.df,building1.val.df[,c(1,2,3)])

building2.norm.val.df <- as.data.frame(lapply(building2.val.df[, 4:179], normalize))
building2.norm.val.df <- cbind(building2.norm.val.df,building2.val.df[,c(1,2,3)])

#   Predictions 
predictions.svm.b0 <- predict(svm.b0.floor,building0.norm.val.df )
confusionMatrix(building0.norm.val.df$FLOOR,predictions.svm.b0)

predictions.svm.b1 <- predict(svm.b1.floor,building1.norm.val.df )
confusionMatrix(building1.norm.val.df$FLOOR,predictions.svm.b1) #77.38

predictions.svm.b2 <- predict(svm.b2.floor,building2.norm.val.df )
confusionMatrix(building2.norm.val.df$FLOOR,predictions.svm.b2)

#  KNN
predictions.knn.b0 <- predict(knn.b0.floor,building0.norm.val.df )
confusionMatrix(building0.norm.val.df$FLOOR,predictions.knn.b0)

predictions.knn.b1 <- predict(knn.b1.floor,building1.norm.val.df )
confusionMatrix(building1.norm.val.df$FLOOR,predictions.knn.b1)

predictions.knn.b2 <- predict(knn.b2.floor,building2.norm.val.df )
confusionMatrix(building2.norm.val.df$FLOOR,predictions.knn.b2)# 0.7016

#RF
predictions.rf.b0 <- predict(rf.b0.floor,building0.norm.val.df )
confusionMatrix(building0.norm.val.df$FLOOR,predictions.rf.b0)

predictions.rf.b1 <- predict(rf.b1.floor,building1.norm.val.df )
confusionMatrix(building1.norm.val.df$FLOOR,predictions.rf.b1)

predictions.rf.b2 <- predict(rf.b2.floor,building2.norm.val.df )
confusionMatrix(building2.norm.val.df$FLOOR,predictions.rf.b2)

# LATITUDE----
#   building0----
#    SVM
predictions.svm.b0.lat <- predict(svm.b0.latitude,building0.floor.val.df)
postResample(building0.floor.val.df$LATITUDE,predictions.svm.b0.lat)
#    KNN
predictions.knn.b0.lat <- predict(knn.b0.latitude,building0.floor.val.df)
postResample(building0.floor.val.df$LATITUDE,predictions.knn.b0.lat)

#   building1----
#    SVM
predictions.svm.b1.lat <- predict(svm.b1.latitude,building1.floor.val.df)
postResample(building1.floor.val.df$LATITUDE,predictions.svm.b1.lat)
#    KNN
predictions.knn.b1.lat <- predict(knn.b1.latitude,building1.floor.val.df)
postResample(building1.floor.val.df$LATITUDE,predictions.knn.b1.lat)



#   building2----
#    SVM
predictions.svm.b2.lat <- predict(svm.b2.latitude,building2.floor.val.df)
postResample(building2.floor.val.df$LATITUDE,predictions.svm.b2.lat)
#    KNN
predictions.knn.b2.lat <- predict(knn.b2.latitude,building2.floor.val.df)
postResample(building2.floor.val.df$LATITUDE,predictions.knn.b2.lat)

# LONGITUDE----
#   building0----
#    SVM
predictions.svm.b0.long <- predict(svm.b0.longitude,building0.floor.val.df)
postResample(building0.floor.val.df$LONGITUDE,predictions.svm.b0.long)

#    KNN
predictions.knn.b0.long <- predict(knn.b0.longitude,building0.floor.val.df)
postResample(building0.floor.val.df$LONGITUDE,predictions.knn.b0.long)

#   building1----
#    SVM
predictions.svm.b1.long <- predict(svm.b1.longitude,building1.floor.val.df)
postResample(building1.floor.val.df$LONGITUDE,predictions.svm.b1.long)

#    KNN
predictions.knn.b1.long <- predict(knn.b1.longitude,building1.floor.val.df)
postResample(building1.floor.val.df$LONGITUDE,predictions.knn.b1.long)

#   building2----
#    SVM
predictions.svm.b2.long <- predict(svm.b2.longitude,building2.floor.val.df)
postResample(building2.floor.val.df$LONGITUDE,predictions.svm.b2.long)

#    KNN
predictions.knn.b2.long <- predict(knn.b2.longitude,building2.floor.val.df)
postResample(building2.floor.val.df$LONGITUDE,predictions.knn.b2.long)
