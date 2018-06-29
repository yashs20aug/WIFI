# Install packages
library(reticulate)
library(tensorflow)
library(keras)
install_tensorflow(method= "system",version = "gpu")

#Preprocess
train.df.nn <- train.df
str(train.df.nn)
train.df.nn <- train.df.nn[,WAP.predictors]
train.df.nn <- cbind(train.df.nn,train.df$BUILDINGID)
# Change to matrix
train.df.nn<- as.matrix(train.df.nn)
dimnames(train.df.nn) <- NULL
class(train.df.nn) <- "numeric"
# Normalize
train.df.nn[,1:289] <- normalize(train.df.nn[,1:289])
summary(train.df.nn)

# Data partition
set.seed(123)
ind <- sample(2, nrow(train.df.nn), replace = T, prob = c(0.7, 0.3))
training.nn <- train.df.nn[ind==1,1:289]
test.nn <- train.df.nn[ind==2,1:289]
trainingtarget <- train.df.nn[ind==1, 290]
testtarget <- train.df.nn[ind==2, 290]

# One Hot Encoding
trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)
print(testLabels)

# There's one additional rule of thumb that helps for supervised learning problems. 
#  The upper bound on the number of hidden neurons that 
#  won't result in over-fitting is:
#   
#   Nh=Ns/(α∗(Ni+No))
#   Ni = number of input neurons.
#   No = number of output neurons.
#   Ns = number of samples in training data set.
#   α = an arbitrary scaling factor usually 2-10.
# 
#   Others recommend setting alpha to a value between 5 and 10, but I find a value of 2 will often work without overfitting.

# Create sequential model
model <- keras_model_sequential()
model %>%
  layer_dense(units=151, activation = "relu", input_shape = c(289)) %>% #relu=rectified linear
  layer_dense(units = 3, activation = "softmax") #Softmax helps to keep between 0 and1
summary(model)

# Compile
model %>%
  compile(loss = "categorical_crossentropy",
          optimizer = "adam",
          metrics = "accuracy")

# Fit model
history <- model %>%
  fit(training.nn,
      trainLabels,
      epoch = 200,
      batch_size = 1024,
      validation_split = 0.2)
plot(history)

# Evaluate model with test data
model3 <- model %>%
  evaluate(test.nn, testLabels)

# Prediction & confusion matrix - test data
prob <- model %>%
  predict_proba(test.nn)

predict.nn <- model %>%
  predict_classes(test.nn)
table3 <- confusionMatrix(predict.nn, testtarget)

cbind(prob, predict.nn, testtarget)

# Fine-tune model
model1
table1
model2
table2
model3
table3