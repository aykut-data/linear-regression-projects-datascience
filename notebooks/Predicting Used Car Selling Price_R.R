library(dplyr)
library(reshape2)
library(caret)
library(car)
library(nortest)

data<-read.csv("Vehicle Data.csv",header = T)
head(data)

summary(data)
colSums(is.na(data))

#Drop NAs
data<-na.omit(data)

CM_data<- data %>% select(selling_price,year,km_driven,mileage,engine,max_power,seats)
correlation_matrix<-round(cor(CM_data),2)
melted_CM<-melt(correlation_matrix)

ggplot(melted_CM, aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  geom_text(aes(Var1, Var2, label=value),color="black",size=4)+
  scale_fill_gradient2(low="red", mid="white", high="blue")+labs(title = "",x="",y="")


set.seed(123)
# Excluding name and year from analysis
#Split data into training and test sets (e.g., 80% training, 20% test)
index<-createDataPartition(data$selling_price, p=0.8, list=FALSE)

traindata<-data[index,]
testdata<-data[-index,]

#Define your formula
data_model<- lm(selling_price~km_driven+seller_type+transmission+owner+mileage+max_power+
                  engine+seats,data=traindata)
data_model

#Print the model summary
summary(data_model)
vif(data_model)

#Revised Formula -- Excluding max power
data_model1<- lm(selling_price~km_driven+seller_type+transmission+owner+mileage+engine+max_power, data=traindata)
data_model1

#Print the model summary
summary(data_model1)

#Calculate and store the residuals
traindata$resi<-residuals(data_model1)
#Assign the fitted values
traindata$fit<-fitted(data_model1)
#Create a scatter plot of residuals vs. predicted values
plot(traindata$fit,traindata$resi)

qqnorm(traindata$resi)

#n<-length(traindata$resi)
#Randomly sample 5000 observations from data
subset_data<-sample(traindata$resi, 5000)
shapiro.test(subset_data)
lillie.test(traindata$resi)

traindata$resi<-residuals(data_model1)
RMSE<-sqrt(mean(traindata$resi**2))
RMSE

testdata$pred<-predict(data_model1,testdata)
testdata$resi<-(testdata$selling_price-testdata$pred)
RMSEtest<-sqrt(mean(testdata$resi**2))
RMSEtest
