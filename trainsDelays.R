#*************************#
#### SETTING DIRECTORY ####
#*************************#

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#****************************#
#### INSTALLING LIBRARIES ####
#****************************#

library(stringr)
library(readxl)
library(splitstackshape)
library(RJSONIO)
library(jsonlite)
library(rvest)
library(dplyr)
library(xml2)
library(ggplot2)
library(tidyverse)
library(caret)
library(leaps)
library(tree)
library(party)
library(caret)
library(caTools)
library(randomForest)
library(psych)
library(gbm)
library(imbalance)
library(e1071)




#********************#
#### LOADING DATA ####
#********************#
set.seed(1)

datiTreni=read.csv("datiTreni.csv", sep=",")
datiTreni=subset(datiTreni, select=-c(X))

#### deleting the observation with delay < 0 ####
datiDelays=datiTreni[-which(datiTreni$ritArr<0), ]
#### deleting not used columns ####
datiDelays=subset(datiDelays, select=-c(Data, Giorno, stazPart, ritPart, stazArr, arrProg))



#******************#
##### MODELING #####
#******************#
n=NROW(datiDelays)

#### testset ####
idTest=sample(n, floor(n/3))
testSet=datiDelays[idTest,]

#### train set ####
trainSet=datiDelays[-idTest,]

#### RANDOM FOREST ####
rf01 = randomForest(ritArr ~ ., data = trainSet)
rf01
names(rf01)
plot(rf01, col = "#A20045", main = "Random forest")
MSErf01=mean((predict(rf01, testSet)-testSet$ritArr)^2)
MSErf01
varImpPlot(rf01, main="Variable importance", pch = 19, color="#A20045")

#### THIS IS THE ONE OF THE TWO BEST MSE ####
#### using only ritArr, partProg, visibAssisi, visibPeru, visibMontev ####
#### test set ####
testSet1=subset(testSet, select=c(ritArr, partProg, visibAssisi, visibPeru, visibMontev))
#### train set ####
trainSet1=subset(trainSet, select=c(ritArr, partProg, visibAssisi, visibPeru, visibMontev))
rf02=randomForest(ritArr ~ ., data = trainSet1)
rf02
names(rf02)
plot(rf02, col = "#A20045", main = "Random forest")
MSErf02=mean((predict(rf02, testSet1)-testSet1$ritArr)^2)
MSErf02

#### using only partProg ####

#### test set ####
testSet1=subset(testSet, select=c(ritArr, partProg))
#### train set ####
trainSet1=subset(trainSet, select=c(ritArr, partProg))
rf02=randomForest(ritArr ~ ., data = trainSet1)
rf02
names(rf02)
plot(rf02, col = "#A20045", main = "Random forest")
MSErf02=mean((predict(rf02, testSet1)-testSet1$ritArr)^2)
MSErf02


#### using partProg and all the visibilities ####

visib = datiDelays[, grepl("visib", colnames(datiDelays))]
datiDelays2=cbind(datiDelays$ritArr, datiDelays$partProg, visib)
colnames(datiDelays2)[which(names(datiDelays2) == "datiDelays$ritArr")] <- "ritArr"
colnames(datiDelays2)[which(names(datiDelays2) == "datiDelays$partProg")] <- "partProg"
#### test set ####
testSet2=datiDelays2[idTest,]
#### train set ####
trainSet2=datiDelays2[-idTest,]
rf03=randomForest(ritArr ~ ., data = trainSet2)
rf03
names(rf03)
plot(rf03, col = "#A20045", main = "Random forest")
MSErf03=mean((predict(rf03, testSet2)-testSet2$ritArr)^2)
MSErf03


#### using partProg and mean of visibilities ####

visib = datiDelays[, grepl("visib", colnames(datiDelays))]
meanVisib=rowMeans(visib)
colnames(datiDelays2)[which(names(datiDelays2) == "datiDelays$ritArr")] <- "ritArr"
colnames(datiDelays2)[which(names(datiDelays2) == "datiDelays$partProg")] <- "partProg"
datiDelays3=data.frame(ritArr=datiDelays$ritArr, partProg = datiDelays$partProg, meanVisib)
#### test set ####
testSet3=datiDelays3[idTest,]
#### train set ####
trainSet3=datiDelays3[-idTest,]
rf03=randomForest(ritArr ~ ., data = trainSet3)
rf03
names(rf03)
plot(rf03, col = "#A20045", main = "Random forest")
MSErf03=mean((predict(rf03, testSet3)-testSet3$ritArr)^2)
MSErf03

#### TRYING GBM PACKAGE FOR IMPORTANT VARIABLES ####
boost.all <- gbm(ritArr ~ ., data = trainSet, distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage=0.1)
boost.all
summary(boost.all)
plot.gbm(boost.all, i.var = "partProg")
plot.gbm(boost.all, i.var = "visibAssisi")
plot.gbm(boost.all, i.var = "visibFiglVal")
plot.gbm(boost.all, i.var = "fenFiglVal")
plot.gbm(boost.all, i.var = "fenAssisi")


mean((predict(boost.all, newdata = testSet, n.trees = 5000) - testSet$ritArr)^2) 


#### THIS IS THE ONE OF THE TWO BEST MSE ####
#### using only ritArr, partProg, visibAssisi, visibFiglVal, fenFiglVal, fenAssisi ####
#### test set ####
testSet1=subset(testSet, select=c(ritArr, partProg, visibAssisi, visibFiglVal, fenAssisi))
#### train set ####
trainSet1=subset(trainSet, select=c(ritArr, partProg, visibAssisi, visibFiglVal, fenAssisi))
rf02=randomForest(ritArr ~ ., data = trainSet1)
rf02
names(rf02)
plot(rf02, col = "#A20045", main = "Random forest")
MSErf02=mean((predict(rf02, testSet1)-testSet1$ritArr)^2)
MSErf02

#### TRYING REGRESSION ONLY ON TRAIN DELAYS TIME GREATER THAN 10 ####

datiDelays10=datiDelays[which(datiDelays$ritArr>10), ]
n=NROW(datiDelays10)
#### testset ####
idTest=sample(n, floor(n/3))
testSet=datiDelays10[idTest,]
#### train set ####
trainSet=datiDelays10[-idTest,]
testSet1=subset(testSet1, select=c(ritArr, partProg, visibAssisi, visibFiglVal, fenAssisi))
#### train set ####
trainSet1=subset(trainSet1, select=c(ritArr, partProg, visibAssisi, visibFiglVal, fenAssisi))
rf02=randomForest(ritArr ~ ., data = trainSet1)
rf02
names(rf02)
plot(rf02, col = "#A20045", main = "Random forest")
MSErf02=mean((predict(rf02, testSet1)-testSet1$ritArr)^2)
MSErf02

#### nothing to do, it sucks lol ####

delaysTree1=tree(ritArr~., trainSet1)
summary(delaysTree1)
plot(delaysTree1)
text(delaysTree1, pretty=0)
treePred1=predict(delaysTree1, testSet1)
mean((treePred1-testSet1$ritArr)^2)

#### REGRESSION SUCKS, TRY CLASSIFICATION ####

#### CLASSIFICATION ####

#create a variable that assume 0 for 0 < delays time < 10 and 1 for 10 < delays time 
n=NROW(classDati)
classDati=datiDelays
classDati$dummy=classDati$ritArr > 10
classDati$dummy[which(classDati$ritArr>10)]="10+ delay"
classDati$dummy[which(classDati$ritArr<=10)]="10- delay"
classDati$dummy=as.factor(classDati$dummy)
classDati4=subset(classDati, select=-c(ritArr))
table(classDati$dummy)

#### trying with all the variables ####
classIdTest=sample(n, floor(n/3))
classTrainSet=classDati4[-classIdTest,]
classTestSet=classDati4[classIdTest,]
classRf=randomForest(dummy ~ ., data = classTrainSet)
classRf
names(classRf)
plot(classRf, col = "#A20045", main = "Random forest")
pred=predict(classRf, classTestSet)
varImpPlot(classRf)
table(pred, classTestSet$dummy)
confusionMatrix(table(pred, classTestSet$dummy))
#accuracy
confusionMatrix(table(pred, classTestSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(pred, classTestSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(pred, classTestSet$dummy))$byClass[2]




#### trying with the most important variables ####
classDati2=subset(classDati, select=c(dummy, partProg, visibFire, visibFiglVal, visibSGioVal, visibMontev))
table(classDati2$dummy)
classIdTest=sample(n, floor(n/3))
classTrainSet=classDati2[-classIdTest,]
classTestSet=classDati2[classIdTest,]
classRf=randomForest(dummy ~ ., data = classTrainSet)
classRf
names(classRf)
plot(classRf, col = "#A20045", main = "Random forest")
pred=predict(classRf, classTestSet)
varImpPlot(classRf)
table(pred, classTestSet$dummy)
confusionMatrix(table(pred, classTestSet$dummy))
#accuracy
confusionMatrix(table(pred, classTestSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(pred, classTestSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(pred, classTestSet$dummy))$byClass[2]



#### TRYING BALANCING DATA ####
classDati3=classDati
classDati3$dummy[which(classDati3$dummy==TRUE)]=1
classDati3$dummy[which(classDati3$dummy==FALSE)]=0
classDati3=subset(classDati3, select=c(dummy, visibFire, visibFiglVal, visibSGioVal, visibMontev))

newDummy = rwo(classDati3, numInstances = 600, classAttr = "dummy")
table(classDati3$dummy)
classDati3=rbind(classDati3, newDummy)
table(classDati3$dummy) 

n=NROW(classDati3)
classIdTest=sample(n, floor(n/3))
classTrainSet=classDati3[-classIdTest,]
classTestSet=classDati3[classIdTest,]
classRf=randomForest(as.factor(dummy) ~ ., data = classTrainSet)
classRf
names(classRf)
plot(classRf, col = "#A20045", main = "Random forest")
pred=predict(classRf, classTestSet)
varImpPlot(classRf)
table(pred, classTestSet$dummy)
#accuracy
confusionMatrix(table(pred, classTestSet$dummy))



#### TRYING SUPPORT VECTOR MACHINE ####
svmfit = svm(as.factor(dummy) ~ ., data = classDati, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

