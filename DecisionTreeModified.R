# install.packages("SparkR")
# install.packages("cluster")
library(SparkR)
library(cluster)
library(factoextra)
library(dplyr)
library(datasets)
library(psych)
library(rpart)
library(rpart.plot)
library(DMwR)

setwd("C:/Users/manis/Desktop/SEM2/BDT/Project/CrimeDataAnalysis-master/CrimeDataAnalysis-master")

set.seed(20)

#Reads the dataset into a dataframe
Dst <- read.csv("new_clean_data_final.csv", header = T)


#Makes sure the predictors and target variable are factors 
Dst$Primary.Type <- as.factor(Dst$Primary.Type)
Dst$Location.Description <- as.factor(Dst$Location.Description)
Dst$District <- as.factor(Dst$District)


#Categories the hours in 5 times of the day 
Dst$TimeOfTheDay <- "Night"

Dst$Hour <- as.numeric(Dst$Hour)

Dst[Dst$Hour >= 1 & Dst$Hour < 4,]$TimeOfTheDay <- 'Early Morning'
Dst[Dst$Hour >= 4 & Dst$Hour < 6,]$TimeOfTheDay <- 'Dawn'
Dst[Dst$Hour >= 6 & Dst$Hour < 12,]$TimeOfTheDay <- 'Morning'
Dst[Dst$Hour >= 12 & Dst$Hour < 16,]$TimeOfTheDay <- 'Afternoon'
Dst[Dst$Hour >= 16 & Dst$Hour < 21,]$TimeOfTheDay <- 'Evening'


Dst$TimeOfTheDay <- as.factor(Dst$TimeOfTheDay)

# #Splits the dataset into train and test datasets in 80-20 proportion 
# library(caTools)
# # set.seed(123)
# split = sample.split(NewDst, SplitRatio = 0.8)
# Train = subset(NewDst, split == TRUE)
# Test = subset(NewDst, split == FALSE)


# new.for.corelation <- data.frame(train.new.primary.type, train.new.location.description, train.new.district, TOTD)
# new.df <- filter(new.for.corelation, train.new.location.description == "STREET" | train.new.location.description == "RESIDENCE" | train.new.location.description == "APARTMENT" | train.new.location.description == "SIDEWALK" | train.new.location.description == "OTHER" | train.new.location.description == "CTA TRAIN" | train.new.location.description == "BAR OR TAVERN" | train.new.location.description == "ALLEY" | train.new.location.description == "DEPARTMENT STORE" | train.new.location.description == "RESTRAURANT")
# new.df <- filter(new.df, train.new.primary.type == "BATTERY" | train.new.primary.type == "NARCOTICS" |  train.new.primary.type == "THEFT")
# 
# new.df$train.new.primary.type <- factor(new.df$train.new.primary.type)
# new.df$train.new.location.description <- factor(new.df$train.new.location.description)
# new.df$train.new.district <- factor(new.df$train.new.district)
# 
# #Smote technique is used to handle class imbalance
# new.df.smote <- SMOTE(train.new.primary.type ~ train.new.location.description + train.new.district + TOTD, new.df, perc.over = 100, perc.under = 400)

NewDst <- data.frame(Dst$Primary.Type, Dst$Location.Description, Dst$District, Dst$TimeOfTheDay, Dst$Season)
NewDst <- setNames(NewDst, c("PrimaryType", "LocationDescription", "District", "TimeOfTheDay", "Season"))



sapply(NewDst, class)
levels(NewDst$Season)
levels(NewDst$PrimaryType)
levels(NewDst$LocationDescription)
levels(NewDst$District)




# new.for.corelation <- data.frame(NewDst$PrimaryType, NewDst$LocationDescription, NewDst$District, NewDst$TimeOfTheDay)
new.df <- filter(NewDst, NewDst$LocationDescription == "STREET" | NewDst$LocationDescription == "RESIDENCE" | NewDst$LocationDescription == "APARTMENT" | NewDst$LocationDescription == "SIDEWALK" | NewDst$LocationDescription == "OTHER" | NewDst$LocationDescription == "CTA TRAIN" | NewDst$LocationDescription == "BAR OR TAVERN" | NewDst$LocationDescription == "ALLEY" | NewDst$LocationDescription == "DEPARTMENT STORE" | NewDst$LocationDescription == "RESTRAURANT")
new.df <- filter(new.df, new.df$PrimaryType == "BATTERY" | new.df$PrimaryType == "NARCOTICS" |  new.df$PrimaryType == "THEFT")

write.csv(new.df,"C:/Users/manis/Desktop/SEM2/BDT/Project/CrimeDataAnalysis-master/CrimeDataAnalysis-master/prepareddata.csv", row.names = FALSE)


new.df$PrimaryType <- as.factor(new.df$PrimaryType)
new.df$LocationDescription <- as.factor(new.df$LocationDescription)
new.df$District <- as.factor(new.df$District)
new.df$TimeOfTheDay <- as.factor(new.df$TimeOfTheDay)
new.df$Season <- as.factor(new.df$Season)

#Splits the dataset into train and test datasets in 80-20 proportion 
library(caTools)
# set.seed(123)
split = sample.split(new.df, SplitRatio = 0.8)
Train = subset(new.df, split == TRUE)
Test = subset(new.df, split == FALSE)


# new.df$train.new.primary.type <- factor(new.df$train.new.primary.type)
# new.df$train.new.location.description <- factor(new.df$train.new.location.description)
# new.df$train.new.district <- factor(new.df$train.new.district)
Train$PrimaryType <- as.factor(Train$PrimaryType)
Train$LocationDescription <- as.factor(Train$LocationDescription)
Train$District <- as.character(Train$District)
Train$District <- as.factor(Train$District)
Train$TimeOfTheDay <- as.factor(Train$TimeOfTheDay)
Train$Season <- as.factor(Train$Season)

#Smote technique is used to handle class imbalance
new.df.smote <- SMOTE(PrimaryType ~ LocationDescription + District + TimeOfTheDay + Season, Train, perc.over = 100, perc.under = 400)

#Train the model
# classifier <- rpart(formula = PrimaryType ~ LocationDescription + District + TimeOfTheDay + Season,
#                data = Train)

classifier <- rpart(formula = PrimaryType ~ LocationDescription + District + TimeOfTheDay + Season,
                    method = "class",
                    data = Train)

pred <- predict(classifier, Test, type="class")

library(caret)
confusionMatrix(pred, Test$PrimaryType)


rpart.plot(classifier, extra=104, fallen.leaves = T, type=4, main="Rpart on New Data (Full Tree)")


# Random Forest

RFDst <- read.csv("prepareddata.csv", header = T, nrows = 200000)

# RFDst <- data.frame(Dst$Primary.Type, Dst$Location.Description, Dst$TimeOfTheDay, Dst$Season)
# RFDst <- setNames(RFDst, c("PrimaryType", "LocationDescription", "TimeOfTheDay", "Season"))

RFDst$PrimaryType <- as.factor(RFDst$PrimaryType)
RFDst$LocationDescription <- as.factor(RFDst$LocationDescription)
RFDst$District <- as.factor(RFDst$District)
RFDst$TimeOfTheDay <- as.factor(RFDst$TimeOfTheDay)
RFDst$Season <- as.factor(RFDst$Season)

# new.for.corelation <- data.frame(NewDst$PrimaryType, NewDst$LocationDescription, NewDst$District, NewDst$TimeOfTheDay)
# RF <- filter(RFDst, RFDst$LocationDescription == "STREET" | RFDst$LocationDescription == "RESIDENCE" | RFDst$LocationDescription == "APARTMENT" | RFDst$LocationDescription == "SIDEWALK" | RFDst$LocationDescription == "OTHER" | RFDst$LocationDescription == "CTA TRAIN" | RFDst$LocationDescription == "BAR OR TAVERN" | RFDst$LocationDescription == "ALLEY" | RFDst$LocationDescription == "DEPARTMENT STORE" | RFDst$LocationDescription == "RESTRAURANT")
# RF <- filter(RF, RF$PrimaryType == "BATTERY" | RF$PrimaryType == "NARCOTICS" |  RF$PrimaryType == "THEFT")
# 
# RF <- subset(RFDst, (RFDst$LocationDescription == "STREET" | RFDst$LocationDescription == "RESIDENCE" | RFDst$LocationDescription == "APARTMENT" | RFDst$LocationDescription == "SIDEWALK" | RFDst$LocationDescription == "OTHER" | RFDst$LocationDescription == "CTA TRAIN" | RFDst$LocationDescription == "BAR OR TAVERN" | RFDst$LocationDescription == "ALLEY" | RFDst$LocationDescription == "DEPARTMENT STORE" | RFDst$LocationDescription == "RESTRAURANT"))
# RF <- subset(RF, (RF$PrimaryType == "BATTERY" | RF$PrimaryType == "NARCOTICS" |  RF$PrimaryType == "THEFT"))

# RF$PrimaryType <- as.factor(RF$PrimaryType)
# RF$LocationDescription <- as.factor(RF$LocationDescription)
# RF$TimeOfTheDay <- as.factor(RF$TimeOfTheDay)
# RF$Season <- as.factor(RF$Season)

#Splits the dataset into train and test datasets in 80-20 proportion 
library(caTools)
# set.seed(123)
split = sample.split(RFDst, SplitRatio = 0.8)
Train = subset(RFDst, split == TRUE)
Test = subset(RFDst, split == FALSE)

v <- data.frame(Train$LocationDescription, Train$District, Train$TimeOfTheDay, Train$Season)
v <- setNames(v, c("LocationDescription", "District", "TimeofTheDay", "Season"))

library(randomForest)
set.seed(123)
classifier2 = randomForest(x = v,
                           y = Train$PrimaryType,
                           ntree = 500)

w <- data.frame(Test$LocationDescription, Test$District, Test$TimeOfTheDay, Test$Season)
w <- setNames(w, c("LocationDescription", "District", "TimeofTheDay", "Season"))

y_pred2 = predict(classifier2, newdata = w)

# Confusion matrix to check the accuray and precision.
cm2 = confusionMatrix(reference = Test$PrimaryType, data = y_pred2)
cm2
