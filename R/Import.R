# IMPORTING DataSET----
# Select and CTRL + Shift + C for instant comment
load.libraries <- c('readr','caret','Hmisc', 'anytime', 'gridExtra', 'corrplot',
                    'reshape2', 'ggplot2','dplyr','ipft',"ggmap", "rgdal", "rgeos",
                    "maptools", "tidyr", "tmap","randomForest","tensorflow","keras")

install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

trainingData.df <- read_csv("Data/UJIndoorLoc/trainingData.csv")
validationData.df <- read_csv("Data/UJIndoorLoc/validationData.csv")



# EXPLORATORY ANALYSIS----

summary(trainingData.df)
summary(trainingData.df[,521:529])

#  Create a copy

trainingData.df<-unique(trainingData.df)
copy.df<-trainingData.df
#  UNIX timestamp to UTC

copy.df$DateTime<- anytime(copy.df$TIMESTAMP)
copy.df$TIMESTAMP<-NULL

#  Distribution and Occurences

occurences<-table(factor(unlist(trainingData.df[,1:520])))#Signal Strengths
barplot(occurences / sum(occurences))

#  Counting columns with all rows as 100

column.v<-vector("numeric")
columnval.v<-vector("numeric")

#  TrainingData

for(i in 1:520){
  if(all(trainingData.df[,i]==100)){
    #print (i)
    #column.v<-c(column.v,i)
    column.v<-c(column.v,i)
  }
}

#  ValidationData

for(i in 1:520){
  if(all(validationData.df[,i]==100)){
    #print (i)
    #column.v<-c(column.v,i)
    columnval.v<-c(columnval.v,i)
  }
}

commonval.v<- c(unique(columnval.v),unique(column.v))
commonval.v <- commonval.v[duplicated(commonval.v)]
commonval.v #No common column with all values as 100

#  Removing Redundant Rows

keep <- apply(copy.df[,3:404], 1, function(x) length(unique(x)) != 1)
copy.df <- copy.df[keep, ]

# Changing values higher than -90 to 100

melted.df[melted.df[,11] < (-90),11] <- 100

copy.df[copy.df<(-90)]<- 100
copy.df<-copy.df[,-c(column.v)]
copy.df$LONGITUDE <- trainingData.df$LONGITUDE # Restoring LONGITUDE

#  Reducing attributes by melting

melted.df<-melt(copy.df,
                id = c("LONGITUDE" ,"LATITUDE", 
                       "FLOOR", "BUILDINGID",
                       "SPACEID", "RELATIVEPOSITION",
                       "USERID", "PHONEID", "DateTime"))
meltval.df<-melt(validation.df,
                 id = c("LATITUDE","FLOOR","BUILDINGID","SPACEID",         
                        "RELATIVEPOSITION", "USERID" , "PHONEID",         
                         "LONGITUDE" ))


melted.df <- melted.df[ ! melted.df$value %in% 100, ]
meltval.df <- meltval.df[ ! meltval.df$value %in% -105, ]


ggplot(melted.df, aes(x =value)) + #Parabolic at Vertex(-29,0)
  geom_density(aes(y = ..count..), fill = "lightgray") +
  scale_x_continuous(breaks = round(seq(min(melted.df$value),
                                        max(melted.df$value), by = 10),1)) +
  labs(y="Count", x = "RSSI values") +
  ggtitle("After removing No Signals(100dBm)and Weak Signals(<-90dBm)")

# PREPROCESSING----

#  Conversion of variables
columns <- c("FLOOR", "BUILDINGID", "SPACEID",
          "RELATIVEPOSITION", "USERID", "PHONEID")
melted.df[columns] <- lapply(melted.df[columns], factor)

copy.df[columns] <- lapply(copy.df[columns], factor)


#  copy.df - Changing 100 to -105
copy.df[copy.df==100]<- (-105)

#  melted.df - Factorising WiFi Strength in a new column

melted.df$Signal_Strength <- cut(melted.df$value, 
                                 breaks = c(-91, -80, -70, -67, -30, 30), 
                                 labels = c( "Bad", "Okay", "Good", "Very Good", "Amazing"), 
                                 right = TRUE)
melted.df$Signal_Strength <- ordered(melted.df$Signal_Strength, 
                                     levels = c("Bad", "Okay", "Good", "Very Good", "Amazing"))
table(melted.df$Signal_Strength)
plot(table(melted.df$Signal_Strength))

melted.df$USERID <- NULL 
melted.df$DateTime <- NULL
melted.df$PHONEID <- NULL

# For Validation,getting same columns
copy.df$DateTime <- NULL
column.v <- colnames(copy.df)

# Lets try some shit out
WAPB0 <- melted.df %>% filter(BUILDINGID==0) %>% select(variable) %>% unique()
WAPBuilding <- c(WAPB0, WAPB1, WAPB2) 
WAPBuilding<-as.data.frame(table(unlist(WAPBuilding)))
plot2 <- ggplot(NULL, aes(LATITUDE, LONGITUDE)) + 
    geom_point(data = copy.df,colour = "red", size = 3) +
    geom_point(data = WAPBuilding,colour = "blue", size = 3)



                   