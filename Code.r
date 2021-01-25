data <- read.csv("data.csv") #import dataset
library(dplyr) #import all libraries
library(pROC)
library(dplyr)
library(caTools)
library(Metrics)

#data summary task
nrow(data) 
summary(data)


#correlation
cor(data$area, data$presence)
cor(data$dry, data$presence)
cor(data$water, data$presence)
cor(data$shade, data$presence)
cor(data$bird, data$presence)
cor(data$fish, data$presence)
cor(data$other.ponds, data$presence)
cor(data$land, data$presence)
cor(data$macro, data$presence)
cor(data$HSI, data$presence)

#Train test split
set.seed(2)
split<-sample.split(data, SplitRatio=0.7)
split
train<-subset(data, split="TRUE")
test<-subset(data, split="FALSE")


#Linear regression on the whole dataset
Model <-lm(formula = train$presence ~.,data=train)
#print(summary(Model))

pred<-predict(Model,test)
#pred
#accuracy
#coefficent of determination
Y_test<- test$presence
error <- Y_test - pred
R2=1-sum(error^2)/sum((Y_test- mean(Y_test))^2)
print(paste("R2",R2))

mean_squared_error <- mse(test$presence,pred)
print(paste("mean_squared_error",mean_squared_error))
root_mean_squared_error <- rmse(test$presence,pred)
print(paste("mean_squared_error",root_mean_squared_error))
#rmse

attribs<-c("area", "dry", "water","shade", "bird", "fish", "other.ponds", "land", "macro","HSI")
counter=2
for (val in attribs) {
  #filterdData=data[val]
print(val)  

#Linear regression
Model <-lm(formula = train$presence ~train[,counter],data=train)
#print(summary(Model))

pred<-predict(Model,test)
#pred
#accuracy
#coefficent of determination
Y_test<- test$presence
error <- Y_test - pred
R2=1-sum(error^2)/sum((Y_test- mean(Y_test))^2)
print(paste("R2",R2))

mean_squared_error <- mse(test$presence,pred)
print(paste("mean_squared_error",mean_squared_error))
root_mean_squared_error <- rmse(test$presence,pred)
print(paste("mean_squared_error",root_mean_squared_error))
#rmse
counter<-counter+1
}


#ROC ANALYSIS
rocobj1 <- plot.roc(data$presence, data$area,
                    main="Statistical comparison",
                    percent=TRUE,
                    col="red")
rocobj2 <- lines.roc(data$presence, data$dry, 
                     percent=TRUE, 
                     col="green")
rocobj3 <- lines.roc(data$presence, data$water, 
                     percent=TRUE, 
                     col="blue")
rocobj4 <- lines.roc(data$presence, data$shade, 
                     percent=TRUE, 
                     col="orange")
rocobj5 <- lines.roc(data$presence, data$bird, 
                     percent=TRUE, 
                     col="yellow")
rocobj6 <- lines.roc(data$presence, data$fish, 
                     percent=TRUE, 
                     col="black")
rocobj7 <- lines.roc(data$presence, data$other.ponds, 
                     percent=TRUE, 
                     col="purple")
rocobj8 <- lines.roc(data$presence, data$land, 
                     percent=TRUE, 
                     col="pink")
rocobj9 <- lines.roc(data$presence, data$macro, 
                     percent=TRUE, 
                     col="brown")
rocobj10 <- lines.roc(data$presence, data$HSI, 
                     percent=TRUE, 
                     col="grey")


testobj <- roc.test(rocobj10, rocobj9,rocobj4)
text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
legend("bottomright", legend=c("area", "dry","water","shade","bird","fish","other.ponds","land","macro","HSI"), col=c("red", "green","blue","orange","yellow","black","purple","pink","brown","grey"), lwd=2)


