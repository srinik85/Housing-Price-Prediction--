## Project 1 Real Estate

getwd()
setwd("D:/Practice R/Project working directory")

data=read.csv('housing_train.csv')
colnames(data)

# Data cleaning
summary(data)

#replacing blank with NA
data[data==""]<-NA

#checking with missing values
sapply(data, function(x) sum(is.na(x))/nrow(data))


# dropping identification variables
data_1<-data[c(-2,-7,-9)]

#checking with missing values
sapply(data_1, function(x) sum(is.na(x))/nrow(data_1))

data_2<-data_1[!is.na(data_1$CouncilArea),]

sapply(data_2, function(x) sum(is.na(x))/nrow(data_2))


data_2[is.na(data_2$BuildingArea),'BuildingArea']=median(data_2$BuildingArea,na.rm=T)
data_2[is.na(data_2$YearBuilt),'YearBuilt']=median(data_2$YearBuilt,na.rm=T)

#Verifying whether mising values left  
sapply(data_2, function(x) sum(is.na(x))/nrow(data_2))

data_2["Age"]=2021-data_2['YearBuilt']

#dropping YearBuilt
data_2<-data_2[c(-12)]

summary(data_2)
plot(density(data_2$Price))
plot(density(data_2$Landsize))

sum(abs(data_1$Price-median(data_1$Price))>3*sd(data_1$Price))

# we have 178 outlier values
out_values<-abs(data_1$Price-median(data_1$Price))>3*sd(data_1$Price)
data_1$Price[out_values]<-median(data_1$Price)

plot(density(data_1$Price))

cor(data_2$Price,data_2$Landsize)

data_2<-data_2[c(-1)]
sample_index=sample(2,nrow(data_2),replace = T,prob = c(0.7,0.3))
train=data_2[sample_index==1,]
test=data_2[sample_index==2,]

dim(train)
colnames(train)

lm.model<-lm(Price~.,data=data_2)
summary(lm.out)

pred.lm.model=predict(lm.model,newdata = test)
error=test$Price-pred.lm.model
mse=sum((error-mean(error))**2)/nrow(test)
TSS=sum((test$Price-mean(test$Price))**2)/nrow(test)
r_squared=1-(mse/TSS)
r_squared

library(randomForest)

rf.model=randomForest(Price~.,data=train,ntree = 1000,importance = T)

rf.model

plot(rf.model)

varImpPlot(rf.model)

pred.rf.model=predict(rf.model,newdata = test)
error=test$Price-pred.rf.model
mse=sum((error-mean(error))**2)/nrow(test)
TSS=sum((test$Price-mean(test$Price))**2)/nrow(test)
r_squared=1-(mse/TSS)
r_squared

test_data=read.csv('housing_test.csv')
str(test_data)

#Data Cleaning for test data
#replacing blank with NA
test_data[test_data==""]<-NA
sapply(test_data,function(x) sum(is.na(x))/nrow(test_data))

colnames(data)
colnames(test_data)
dim(data);dim(test_data)

#replacing CouncilArea with mode value
test_data[is.na(test_data$CouncilArea),"CouncilArea"]="Boroondara"  

#replacing NAs with median
test_data[is.na(test_data$Bathroom),'Bathroom']=median(test_data$Bathroom,na.rm=T)
test_data[is.na(test_data$Car),'Car']=median(test_data$Car,na.rm=T)
test_data[is.na(test_data$Landsize),'Landsize']=median(test_data$Landsize,na.rm=T)
test_data[is.na(test_data$Bedroom2),'Bedroom2']=median(test_data$Bedroom2,na.rm=T)
test_data[is.na(test_data$BuildingArea),'BuildingArea']=median(test_data$BuildingArea,na.rm=T)
test_data[is.na(test_data$YearBuilt),'YearBuilt']=median(test_data$YearBuilt,na.rm=T)

#Verifying whether NAs are left
sapply(test_data,function(x) sum(is.na(x))/nrow(test_data))

#Dropping Identification column
test_data1=test_data[c(-2,-6,-8)] 

test_data1["Age"]=2021-test_data1['YearBuilt']

colnames(test_data1)#dropping column YearBuilt
test_data1=test_data1[-11]
colnames(test_data1)

summary(test_data1)

pred.rf.model=predict(rf.model,newdata = test_data1)

pred=data.frame(pred.rf.model)

nrow(pred)

sapply(pred,function(x) sum(is.na(x))/nrow(pred))

pred_price=cbind(test_data,pred)

write.csv(pred_price,"housing_test_with_price_pred.csv")
