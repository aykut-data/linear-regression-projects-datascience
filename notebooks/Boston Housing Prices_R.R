hp<-read.csv("Housing Prices.csv",header=T)
head(hp)

install.packages("ggplot2")
install.packages("reshape2")

library(ggplot2)
library(reshape2)
cormat <- round(cor(hp),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  scale_fill_gradient2(low="red",mid="white",high="blue")

install.packages("caret")
library(caret)
index<-createDataPartition(hp$MEDV,p=0.8,list=FALSE)
traindata<-hp[index,]
testdata<-hp[-index,]

hp_model<-lm(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+LSTAT,data=traindata)
hp_model

summary(hp_model)


install.packages("car")
library(car)
vif(hp_model)

hp_model1<-lm(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+PTRATIO+LSTAT,data=traindata)

library(car)
vif(hp_model1)

traindata$fit<-fitted(hp_model1)
traindata$resi<-residuals(hp_model1)
plot(traindata$fit,traindata$resi)

qqnorm(traindata$resi)
shapiro.test(traindata$resi)

library(nortest)
lillie.test(traindata$resi)

##Rerun the model after removing the insignificant variables
hp_model2<-lm(MEDV~CRIM+ZN+CHAS+NOX+RM+DIS+RAD+PTRATIO+LSTAT,data=traindata)
summary(hp_model2)

traindata$resi<-residuals(hp_model2)
RMSE<-sqrt(mean(traindata$resi**2))
RMSE

testdata$pred<-predict(hp_model2,testdata)
testdata$res<-(testdata$MEDV-testdata$pred)
RMSEtest<-sqrt(mean(testdata$res**2))
RMSEtest

library(caret)
kfolds<-trainControl(method="cv",number=4)
kmodel<- train(MEDV~CRIM+ZN+CHAS+NOX+RM+DIS+RAD+PTRATIO+LSTAT,data=hp,method="lm",
               trControl=kfolds)
kmodel
