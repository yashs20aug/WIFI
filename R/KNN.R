# KNN-Classification
#  Predicting the damn building
#  Rotate and rescale,get new measurements.
#  Making a formula variable for necessary predictors for label "BUILDINGID"----

f <- paste("BUILDINGID ~",paste(WAP.predictors, collapse=" + "))
f <- as.formula(f)


# Normalizing and creating a new DF

normalize <- function(x) {
  y <- (x + 105)/(105)
  y
}
#-90,0 (TRAINING)
#-90,-34 (VALIDATION)

train.knn.df <- lapply(train.df[, 1:404], normalize)
train.knn.df <- as.data.frame(train.knn.df)
train.knn.df <- cbind(train.knn.df,train.df[,c("LATITUDE","LONGITUDE","FLOOR","BUILDINGID")])
train.knn.df <- unique(train.knn.df)

train.knn.df <- train.knn.df %>% group_by(.dots=c(WAP.predictors,"FLOOR","BUILDINGID")) %>% summarise(
       LATITUDE   = mean(LATITUDE),
       LONGITUDE  = mean(LONGITUDE)
   )
# Data Partition

set.seed(123)
ind<-sample(2,nrow(train.knn.df),replace = TRUE,prob=c(0.7,0.3))
train.knn<-train.knn.df[ind==1,]
test.knn<-train.knn.df[ind==2,]

# KNN for BUILDINGID ----
#  Getting necessary predictors

WAP.predictors <- melted.df %>% 
  select(BUILDINGID,variable,value) %>% 
  distinct(BUILDINGID,variable,.keep_all = TRUE) %>% 
  count(variable) %>% filter(n==1)
WAP.predictors <- WAP.predictors$variable
WAP.predictors <- as.character(WAP.predictors)

#  Modelling (Just Run This)
trControl <- trainControl(method  = "cv",
                          number  = 5,
                          verboseIter = TRUE)

knnfit <- caret::train(f,
                method     = "knn",
                trControl  = trControl,
                metric     = "Accuracy",
                data       = train.knn.df,
                preProcess = "zv")

plot(knnfit)
# knnfit <- train(f,
#              method     = "knn",
#              tuneGrid   = expand.grid(k=1:5),
#              trControl  = trControl,
#              metric     = "Accuracy",
#              data       = train,
#              preProcess = c("zv"))
plot(knnfit)
predict.knn <- predict(knnfit,test.knn)
confusionMatrix(test.knn$BUILDINGID,predict.knn)

#  Miss Classifications in BUILDING ID
misclassified.knn <- which(predict.knn != unlist(test.knn[,291])) 
test.knn[c(misclassified.knn),] %>% View()

#   Finding the cause, WAPS in more than 1 building

WAP.duplicates <- melted.df %>% 
     select(BUILDINGID,variable,value) %>% 
     distinct(BUILDINGID,variable,.keep_all = TRUE) %>% 
     count(variable) %>% filter(n>1)

# FLOOR----
#   Building0----
#   Use same Data Set as SVM
#   Modelling (Just Run this for predictions)

set.seed(3195)

trControl <- trainControl(method  = "repeatedcv",
                                       number  = 10,
                                       repeats = 2,
                                       verboseIter = TRUE)

trControl <- trainControl(method  = "none",
                          verboseIter = TRUE)

knn.b0.floor <- caret::train(FLOOR ~ .-LONGITUDE -LATITUDE,
                       method     = "knn",
                       trControl  = trControl,
                       metric     = "Accuracy",
                       data = building0.svm.df)


plot(knn.b0.floor)
predict.knn.b0 <- predict(knn.b0.floor,test.svm.b0) # using SVM data partition
confusionMatrix(test.svm.b0$FLOOR,predict.knn.b0)

#   Building1----
#   Use same Data Set as SVM
#   Modelling (Just Run this for predictions)

set.seed(3195)

trControl <- trainControl(method  = "repeatedcv",
                          number  = 10,
                          repeats = 2,
                          verboseIter = TRUE)

trControl <- trainControl(method  = "none",
                          verboseIter = TRUE)

knn.b1.floor <- caret::train(FLOOR ~ . -BUILDINGID,
                          method     = "knn",
                          tuneGrid   = expand.grid(k=1:20),
                          trControl  = trControl,
                          metric     = "Accuracy",
                          data = train.svm.b1)

knn.b1.floor <- caret::train(FLOOR ~ .-LONGITUDE -LATITUDE,
                             method     = "knn",
                             trControl  = trControl,
                             metric     = "Accuracy",
                             data = building1.svm.df)


plot(knn.b1.floor)
predict.knn.b1 <- predict(knn.b1.floor,test.svm.b1) # using SVM data partition
confusionMatrix(test.svm.b1$FLOOR,predict.knn.b1)

#   Building2----
set.seed(3195)

trControl <- trainControl(method  = "repeatedcv",
                          number  = 10,
                          repeats = 2,
                          verboseIter = TRUE)

trControl <- trainControl(method  = "none",
                          verboseIter = TRUE)

knn.b2.floor <- caret::train(FLOOR ~ .-LONGITUDE -LATITUDE,
                             method     = "knn",
                             trControl  = trControl,
                             metric     = "Accuracy",
                             data = building2.svm.df)

knn.b2.floor <- caret::train(FLOOR ~ . -BUILDINGID,
                             method     = "knn",
                             tuneGrid   = expand.grid(k=1:20),
                             trControl  = trControl,
                             metric     = "Accuracy",
                             data = train.svm.b2)



plot(knn.b2.floor)
predict.knn.b2 <- predict(knn.b2.floor,test.svm.b2) # using SVM data partition
confusionMatrix(test.svm.b2$FLOOR,predict.knn.b2)

#  Latitude----
#   building0----
#   Using building0.floor.df dummified floor variable
trctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

set.seed(3195)

knn.b0.latitude <- caret::train(LATITUDE ~ . -LONGITUDE,
                                method     = "knn",
                                trControl  = trctrl,
                                metric     = "RMSE",
                                data = building0.floor.df)

#   building1----

knn.b1.latitude <- caret::train(LATITUDE ~ . -LONGITUDE,
                                method     = "knn",
                                trControl  = trctrl,
                                metric     = "RMSE",
                                data = building1.floor.df)

#   building2----
knn.b2.latitude <- caret::train(LATITUDE ~ . -LONGITUDE,
                                method     = "knn",
                                trControl  = trctrl,
                                metric     = "RMSE",
                                data = building2.floor.df)
#  Longitude----

#   building0----
#   Create Partition

DummyVar <- dummyVars("~FLOOR", data = building0.svm.df, fullRank=T)
DummyVarDF <- data.frame(predict(DummyVar, newdata = building0.svm.df))
building0.floor.df<-cbind(building0.svm.df, DummyVarDF)
building0.floor.df$FLOOR <- NULL

# set.seed(9890)
# intrain <- createDataPartition(y = building0.svm.df$LONGITUDE, p= 0.7, list = FALSE)
# train.svm.b0.long <- building0.svm.df[intrain,]
# test.svm.b0.long <- building0.svm.df[-intrain,]

#   Modelling (Just Run this for predictions)
trctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

set.seed(3195)

knn.b0.longitude <- caret::train(LONGITUDE ~ . -LATITUDE,
                                 method     = "knn",
                                 trControl  = trctrl,
                                 metric     = "RMSE",
                                 data = building0.floor.df)

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

knn.b1.longitude <- caret::train(LONGITUDE ~ . -LATITUDE,
                                 method     = "knn",
                                 trControl  = trctrl,
                                 metric     = "RMSE",
                                 data = building1.floor.df)
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

knn.b2.longitude <- caret::train(LONGITUDE ~ . -LATITUDE,
                                 method     = "knn",
                                 trControl  = trctrl,
                                 metric     = "RMSE",
                                 data = building2.floor.df)