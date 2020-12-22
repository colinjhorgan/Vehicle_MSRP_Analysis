getwd()

library(tidyverse)
library(randomForest)
library(caTools)
library(ICSNP)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(bestNormalize)

#Get path to data and read into dataframe
path <- 'CleanedMSRPData.csv'
import <- read_csv(path)
df <- subset(import, select= -c(X1))

#drop rows where our target MSRP is null (54 rows)
df <- df[!(is.na(df$MSRP)),]

######################## More Data Preprocessing ###############################

#Drop null values from dataframe
df <- drop_na(df)

#Convert data to factors, justified in report
df.fact <- lapply(subset(df, select=-c(MSRP)), factor)
df.fact$MSRP <- df$MSRP

#Appropriately name columns
names(df.fact) <- make.names(names(df.fact), unique=TRUE)
str(df.fact)

#Convert rows with more than 100 factors to numerical
df.fact$Wheelbase..in. <- as.numeric(df.fact$Wheelbase..in.)
df.fact$SAE.Net.Torque...RPM <- as.numeric(df.fact$SAE.Net.Torque...RPM)
df.fact$SAE.Net.Horsepower...RPM <- as.numeric(df.fact$SAE.Net.Horsepower...RPM)
df.fact$Turning.Diameter...Curb.to.Curb..ft. <- as.numeric(df.fact$Turning.Diameter...Curb.to.Curb..ft.)

#Plot distributions of numerical data
qqnorm(df.fact$Wheelbase..in., main='Wheelbase')
qqnorm(df.fact$SAE.Net.Torque...RPM, main='SAE Net Torque')
qqnorm(df.fact$SAE.Net.Horsepower...RPM, main='SAE Net Horsepower')
qqnorm(df.fact$Turning.Diameter...Curb.to.Curb..ft., main='Turning Diameter')
qqnorm(df.fact$MSRP, main='MSRP')

#Conver df.fact to dataframe
df.fact <- as.data.frame(df.fact)

#Set aside data for training and testing
size <- floor(.075 * NROW(df.fact$Engine))
train_index <- sample.split(colnames(df.fact), SplitRatio = .75)
train <- subset(df.fact, train_index == TRUE)
test <- subset(df.fact, train_index == FALSE)

############################# Decision Tree ####################################

#Use training data to create a decision tree
dtr <- rpart(MSRP ~ ., data=train)

#plot decision tree
rpart.plot(dtr, tweak=1.3)
prp(dtr, type=5)

#Make predictions using model and get MSE and MAE
preds <- predict(dtr, test)
sqrt(MSE(preds, test$MSRP)) #Root mean squared error
MAE(preds, test$MSRP) #Mean absolute error

#Plot residuals
plot(test$MSRP, (test$MSRP - preds),
     main='Decision Tree Residuals',
     xlab='MSRP of Test Data',
     ylab='Residual')
abline(0,0, col='red')

#Plot actual vs. predicted with line of best fit
bfl <- lm(test$MSRP ~ preds) #Get line of best fit
summary(bfl)
plot(preds, test$MSRP,
     main='Actual vs. Predicted MSRP',
     xlab='Predicted MSRP',
     ylab='Actual MSRP')
abline(bfl, col='red')


######################## Random Forest Modeling ################################


#Train random forest on training data (Uncomment to run, will take a while)
rf <- randomForest(MSRP ~ ., 
                   data=train, 
                   ntree=350)

#Obtain feature importances
importance(rf, type=2)

#Plot importances
varImpPlot(rf, main='Feature Importances', n.var=15)

#Plot random forest
plot(rf, main='Error (MSE) Reduction Across Iterations')

#Get RMSE of final model
sqrt(rf$mse[350])

#Make predictions using model and get MSE and MAE
preds <- predict(rf, test)
sqrt(MSE(preds, test$MSRP)) #Root mean squared error
MAE(preds, test$MSRP) #Mean absolute error

#Plot residuals
plot(test$MSRP, (test$MSRP - preds),
     main='Random Forest Residuals',
     xlab='MSRP of Test Data',
     ylab='Residual')
abline(0,0, col='red')

#Plot actual vs. predicted with line of best fit
bfl <- lm(test$MSRP ~ preds) #Get line of best fit
summary(bfl)
plot(preds, test$MSRP,
     main='Actual vs. Predicted MSRP',
     xlab='Predicted MSRP',
     ylab='Actual MSRP')
abline(bfl, col='red')

########################## Reduced Decision Tree ###############################

#Decision Tree with reduced data set
dtr <- rpart(MSRP ~ Displacement + Engine + SAE.Net.Torque...RPM + SAE.Net.Horsepower...RPM + Manufacturer,
             data=train)
prp(dtr, type=5) #plot tree

#Make predictions using model and get MSE and MAE
preds <- predict(dtr, test)
sqrt(MSE(preds, test$MSRP)) #Root mean squared error
MAE(preds, test$MSRP) #Mean absolute error

#Plot residuals
plot(test$MSRP, (test$MSRP - preds),
     main='Decision Tree Residuals',
     xlab='MSRP of Test Data',
     ylab='Residual')
abline(0,0, col='red')

#Plot actual vs. predicted with line of best fit
bfl <- lm(test$MSRP ~ preds) #Get line of best fit
summary(bfl)
plot(preds, test$MSRP,
     main='Actual vs. Predicted MSRP',
     xlab='Predicted MSRP',
     ylab='Actual MSRP')
abline(bfl, col='red')


