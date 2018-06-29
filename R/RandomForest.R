# RANDOM FOREST
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
stopCluster(cl)
#  BUILDINGID----

train.df <- cbind(train.df[,1:404],train.df[,c("LATITUDE","LONGITUDE","FLOOR","BUILDINGID")])
train.df <- unique(train.df)

WAPS <- colnames(train.df[,1:404])

train.df <- train.df %>% group_by(.dots=c(WAPS,"FLOOR","BUILDINGID")) %>% 
  summarise(
  LATITUDE   = mean(LATITUDE),
  LONGITUDE  = mean(LONGITUDE)
)
#  BUILDINGID Data Partition

set.seed(9890)
ind<-sample(2,nrow(train.df),replace = TRUE,prob=c(0.7,0.3))
train<-train.df[ind==1,]
test<-train.df[ind==2,]

#  BUILDINGID Random Forest

set.seed(222)

#  Using Wifi signals to determine buildings

rf<-randomForest(f,
                 data=train.df,
                 ntree=400,
                 mtry=20,
                 importance=TRUE,
                 proximity=TRUE)
print(rf)

##  Prediction and Confusion Matrix

BUILDINGID.prf<- predict(rf,test)
confusionMatrix(BUILDINGID.prf,test$BUILDINGID)

#Error Rate and variable importance

plot(rf)
varUsed(rf)
varImpPlot(rf)


#Tuning
t<-tuneRF(train[,-(405:407)],train$BUILDINGID,
          stepFactor = 0.5, 
          plot=TRUE, 
          ntreeTry = 405,
          trace=TRUE,
          improve=0.05)
# FLoor----

rf.b0.floor <- randomForest(FLOOR ~ .-LONGITUDE -LATITUDE,
                                       data=building0.svm.df,
                                       importance=TRUE,
                                       proximity=TRUE)

rf.b1.floor <- randomForest(FLOOR ~ .-LONGITUDE -LATITUDE,
                            data=building1.svm.df,
                            importance=TRUE,
                            proximity=TRUE)

rf.b2.floor <- randomForest(FLOOR ~ .-LONGITUDE -LATITUDE,
                            data=building2.svm.df,
                            importance=TRUE,
                            proximity=TRUE)

# scatterplot3d::scatterplot3d(building0.df$LONGITUDE,building0.df$LATITUDE,
#                              building0.df$FLOOR,color = colors )