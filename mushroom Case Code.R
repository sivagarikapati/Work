
library(readxl)
mushrooms <- read_excel("C:/Users/vanshika1229/Desktop/Data Sience/Week2/Mushroom/mushrooms.xlsx")

View(mushrooms)

mushrooms <- as.data.frame(unclass(mushrooms))
str(mushrooms)

mushrooms_dataset<-mushrooms
# Load the carret package which allows us to partition the data
library(caret)
# we use the dataset to create the partition (80% training and 20% Testing)
index<-createDataPartition(mushrooms_dataset$class,p=0.80,list = FALSE)

#select 20% data for testing

testdata<-mushrooms_dataset[-index,]

#select 80% data for Traindata


trainset<-mushrooms_dataset[index,]

# Dimension of the data
dim(trainset)

# Structure of the data

str(trainset)

#summary of the data

summary(trainset)

# level of the prediction colum

levels(trainset$class)

classdim<-as.numeric(trainset$class)

#histogram


hist(classdim)

summary(trainset)


# Initial data visualization
library(ggplot2)
str(trainset)

## a. Univariate categorical data visualization
p<- ggplot(data = trainset)

p+geom_bar(mapping = aes(x = cap.shape, fill=class), position = position_dodge())+
  theme(legend.position = "top")
table(trainset$cap.shape, trainset$class)

## a. Univariate categorical data visualization
p<- ggplot(data = trainset)

p+geom_bar(mapping = aes(x = trainset$cap.surface, fill=trainset$class), position = position_dodge())+
  theme(legend.position = "top")
table(trainset$cap.surface, trainset$class)


## a. Univariate categorical data visualization
p<- ggplot(data = trainset)

p+geom_bar(mapping = aes(x = trainset$cap.color,fill=trainset$class), position = position_dodge())+
  theme(legend.position = "top")
table(trainset$cap.color, trainset$class)

## a. Univariate categorical data visualization
p<- ggplot(data = trainset)

p+geom_bar(mapping = aes(x = trainset$gill.size,fill=trainset$class), position = position_dodge())+
  theme(legend.position = "top")
table(trainset$gill.size, trainset$class)

table(trainset$cap.shape, trainset$class)

table( trainset$class,trainset$gill.color)


library(caret)

set.seed(1000)

str(trainset)

# Fit the model 
model.rpart<-train(x = trainset[,2:23],y = trainset[,1], method = "rpart",metric = "Accuracy")
## Loading required package: rpart 
print(model.rpart)


plot(model.rpart$finalModel)


model.rpart$finalModel



library(rattle)
library(rpart.plot)
# install.packages("rpart.plot") 
fancyRpartPlot(model.rpart$finalModel) 

rpart.plot(model.rpart$finalModel)

## Predictions on train dataset 
pred<-table(predict(object = model.rpart$finalModel,newdata = trainset[,2:23],type="class"))

print(pred)

confusionMatrix(predict(object = model.rpart$finalModel,newdata = trainset[,2:23],type="class"),trainset$class)


## Checking accuracy on the testdata set we created initially 

pred_test1<-predict(object = model.rpart$finalModel, newdata = testdata[,2:23], type="class")

confusionMatrix(pred_test1,testdata$class)

## Random Forest

library(caret)

set.seed(1000)


#str(trainset)

# Fit the model 
model.rf<-train(x = trainset[,2:23],y = trainset[,1], method = "rf",metric = "Accuracy")
## Loading required package: RF 
print(model.rf)


plot(model.rf$finalModel)


model.rf$finalModel







## Predictions on train dataset 
pred<-table(predict(object = model.rf$finalModel,newdata = trainset[,2:23],type="class"))

print(pred)

confusionMatrix(predict(object = model.rf$finalModel,newdata = trainset[,2:23],type="class"),trainset$class)


## Checking accuracy on the testdata set we created initially 

pred_test1<-predict(object = model.rf$finalModel, newdata = testdata[,2:23], type="class")

confusionMatrix(pred_test1,testdata$class)

########################################


library(caret) 
install.packages("gbm") 
library(gbm)

# Fit the model 
model.gbm<-train(x = trainset[,2:23],y = trainset[,1], method = "gbm",metric = "Accuracy",verbose=FALSE)
## Loading required package: plyr 
# Print the model
print(model.gbm)

##summary(model.gbm)

pred<-predict(object = model.gbm,newdata = trainset[,2:23]) 

confusionMatrix(pred,trainset$class)

## Performance on the test set
pred_test<-predict(object = model.gbm,newdata = testdata[,2:23]) 
confusionMatrix(pred_test,testdata$class)

#############################################33333
library(caret)

# summarize accuracy of models
results <- resamples(list(TREE=model.rpart, RandomForest=model.rf, GBM=model.gbm)) 
summary(results)


dotplot(results)

