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
library(DMwR)


#********************#
#### LOADING DATA ####
#********************#
set.seed(1)

datiTreni=read.csv("datiTreni.csv", sep=",")
datiTreni=subset(datiTreni, select=-c(X))

#### deleting the observation with delays time < 0 ####
datiDelays=datiTreni[-which(datiTreni$ritArr<0), ]
#### deleting not used columns ####
datiDelays=subset(datiDelays, select=-c(Data, Giorno, stazPart, ritPart, stazArr, arrProg))

#********************#
###### MODELING ######
#********************#

#### Creating a dummy variable ####
classDati=datiDelays
n=NROW(classDati)
classDati$dummy=classDati$ritArr > 10
classDati$dummy[which(classDati$ritArr>10)]="10+ delay"
classDati$dummy[which(classDati$ritArr<=10)]="10- delay"
classDati$dummy=as.factor(classDati$dummy) 

NROW(classDati)
NROW(classDati[which(classDati$ritArr>10),])
NROW(classDati[which(classDati$ritArr<=10),])


#### Creating training and testing set ####
idTest=sample(n, floor(n/3))
testSet=classDati[idTest,]
trainingSet=classDati[-idTest,]

#### Classification tree ####
delaysTree=tree(dummy~.-ritArr, trainingSet)
summary(delaysTree)
plot(delaysTree)
text(delaysTree)
delaysTree

treePred=predict(delaysTree, testSet, type="class")
table(treePred, testSet$dummy)
confusionMatrix(treePred, testSet$dummy)
x=confusionMatrix(treePred, testSet$dummy)
#accuracy
confusionMatrix(table(treePred, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(treePred, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(treePred, testSet$dummy))$byClass[2]
#kappa 
confusionMatrix(table(treePred, testSet$dummy))$overall[2]

kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDati[idTest,]
  trainingSet=classDati[-idTest,]
  delaysTree=tree(dummy~partProg+fenFiglVal+fenAssisi-ritArr, trainingSet)
  treePred=predict(delaysTree, testSet, type="class")
  kappa[i]=confusionMatrix(table(treePred, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
max(kappa)
mean(kappa)
table(testSet$dummy)

#### PER BEAMER CONFRONTARE QUESTI DUE ####
#### DUE ####
#### Bagging ####
#setting m=p we are using a bagging approach
p=3 #without considering ritArr e dummy
delaysBagg=randomForest(dummy~partProg+fenFiglVal+fenAssisi-ritArr, trainingSet, importance=TRUE)
delaysBagg
baggPred=predict(delaysBagg, testSet)
table(baggPred, testSet$dummy)
confusionMatrix(baggPred, testSet$dummy)
#il bagging ci fa ottenere un po' di specificità che nel singolo albero era zero
#probabilmente è dovuto alla troppo grande importanza di partProg
importance(delaysBagg)
varImpPlot(delaysBagg)
confusionMatrix(table(baggPred, testSet$dummy))
#accuracy
confusionMatrix(table(baggPred, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(baggPred, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(baggPred, testSet$dummy))$byClass[2]
#kappa
confusionMatrix(table(baggPred, testSet$dummy))$overall[2]

p=NCOL(classDati)-2
kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDati[idTest,]
  trainingSet=classDati[-idTest,]
  delaysBagg=randomForest(dummy~partProg+fenAssisi+fenFiglVal-ritArr, trainingSet, importance=TRUE)
  baggPred=predict(delaysBagg, testSet)
  kappa[i]=confusionMatrix(table(baggPred, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
max(kappa)
mean(kappa)


#### Random forest ####
#not setting mtry for classification the default number of variables randomply sampled as candidates at each split is sqrt(p)
delaysRF=randomForest(dummy~.-ritArr, trainingSet, importance=TRUE)
delaysRF
mean(delaysRF$err.rate)
rfPred=predict(delaysRF, testSet)
table(rfPred, testSet$dummy)
confusionMatrix(rfPred, testSet$dummy)
importance(delaysRF)
varImpPlot(delaysRF)
confusionMatrix(table(rfPred, testSet$dummy))
#accuracy
confusionMatrix(table(rfPred, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(rfPred, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(rfPred, testSet$dummy))$byClass[2]
#kappa
confusionMatrix(table(rfPred, testSet$dummy))$overall[2]
#variable importance according this model
varImpPlot(delaysRF) #it looks like partProg has a really huge importance in classifying the data

kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDati[idTest,]
  trainingSet=classDati[-idTest,]
  delaysRF=randomForest(dummy~.-ritArr, trainingSet, importance=TRUE)
  rfPred=predict(delaysRF, testSet)
  kappa[i]=confusionMatrix(table(rfPred, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
max(kappa)
mean(kappa)



#random forest doesn't appear as a huge improvement for performance but try seeing different values for mtry
#for mtry 
p=NCOL(classDati)-2
oobErrRate=c()
testErrRate=c()
kappa=c()
B=500

for(mtry in 1:p){
  rf=randomForest(dummy~.-ritArr, trainingSet, mtry=mtry, ntree=B)
  oobErrRate[mtry]=rf$err.rate[500] #oob error rate 
  pred=predict(rf, testSet)
  #to compute test error let's do 1-accuracy
  testErrRate[mtry]=1-confusionMatrix(pred, testSet$dummy)$overall[1]
  #we compute kappa because Cohen's kappa is more informative than accuracy when working with unbalanced data
  kappa[mtry]=confusionMatrix(pred, testSet$dummy)$overall[2]
}
#test error
matplot(1:mtry, cbind(oobErrRate,testErrRate), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Error rate", xlab="mtry")
legend("bottomright", legend=c("Out of Bag Error", "Test Error"), pch=19, col=c("#A20045", "#00484D"))
#kappa
matplot(1:mtry, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry") #kappa always sucks



#### MODEL IS OVERFITTING :'( ####

#### building a dataset that contains just all the visibilities and partProg as predictor variables ####
visibCol=classDati[, grepl("visib", colnames(classDati))]
classDatiVisib=cbind(classDati$dummy, classDati$ritArr, classDati$partProg, visibCol)
colnames(classDatiVisib)[which(names(classDatiVisib) == "classDati$ritArr")] <- "ritArr"
colnames(classDatiVisib)[which(names(classDatiVisib) == "classDati$dummy")] <- "dummy"
colnames(classDatiVisib)[which(names(classDatiVisib) == "classDati$partProg")] <- "partProg"

#### defining test and training set for dataset that contains just visib* and partProg as predictor variables #### 
testSet=classDatiVisib[idTest,]
trainingSet=classDatiVisib[-idTest,]

#### trying using only visibilities and partProg ####
p=NCOL(classDatiVisib)-2
oobErrRate=c()
testErrRate=c()
kappa=c()
B=500

for(mtry in 1:p){
  rf=randomForest(dummy~.-ritArr, trainingSet, mtry=mtry, ntree=B)
  oobErrRate[mtry]=rf$err.rate[500] #oob error rate 
  pred=predict(rf, testSet)
  #to compute test error let's do 1-accuracy
  testErrRate[mtry]=1-confusionMatrix(pred, testSet$dummy)$overall[1]
  #we compute kappa because Cohen's kappa is more informative than accuracy when working with unbalanced data
  kappa[mtry]=confusionMatrix(pred, testSet$dummy)$overall[2]
}

#test error
matplot(1:mtry, cbind(oobErrRate,testErrRate), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Error rate", xlab="mtry")
legend("topleft", legend=c("Out of Bag Error", "Test Error"), pch=19, col=c("#A20045", "#00484D"))
#kappa
matplot(1:mtry, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")



#### building a dataset that contains only partProg and mean of visibilities as predictor variables ####
classDatiVisibMean=data.frame(classDati$dummy, classDati$ritArr, classDati$partProg, rowMeans(visibCol))

colnames(classDatiVisibMean)[which(names(classDatiVisibMean) == "classDati.ritArr")] <- "ritArr"
colnames(classDatiVisibMean)[which(names(classDatiVisibMean) == "classDati.dummy")] <- "dummy"
colnames(classDatiVisibMean)[which(names(classDatiVisibMean) == "classDati.partProg")] <- "partProg"
colnames(classDatiVisibMean)[which(names(classDatiVisibMean) == "rowMeans.visibCol.")] <- "visibMean"



#### defining test and training set for dataset that contains mean ####
testSet=classDatiVisibMean[idTest,]
trainingSet=classDatiVisibMean[-idTest,]


#### trying using only the mean of visibilities and the partProg ####
p=NCOL(classDatiVisibMean)-2
oobErrRate=c()
testErrRate=c()
kappa=c()
B=500

for(mtry in 1:p){
  rf=randomForest(dummy~.-ritArr, trainingSet, mtry=mtry, ntree=B)
  oobErrRate[mtry]=rf$err.rate[500] #oob error rate 
  pred=predict(rf, testSet)
  #to compute test error let's do 1-accuracy
  testErrRate[mtry]=1-confusionMatrix(pred, testSet$dummy)$overall[1]
  #we compute kappa because Cohen's kappa is more informative than accuracy when working with unbalanced data
  kappa[mtry]=confusionMatrix(pred, testSet$dummy)$overall[2]
}

#test error
matplot(1:mtry, cbind(oobErrRate,testErrRate), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Error rate", xlab="mtry")
legend("topleft", legend=c("Out of Bag Error", "Test Error"), pch=19, col=c("#A20045", "#00484D"))
#kappa
matplot(1:mtry, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")


#### using only visibilities mean ####
p=NCOL(classDatiVisibMean)-3
oobErrRate=c()
testErrRate=c()
kappa=c()
B=500

#we are just using a predictor variable so we are indeed doing a bagging because mtry=p
rf=randomForest(dummy~.-ritArr-partProg, trainingSet, mtry=p, ntree=B)
oobErrRate=rf$err.rate[500] #oob error rate 
pred=predict(rf, testSet)
#accuracy
confusionMatrix(table(pred, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(pred, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(pred, testSet$dummy))$byClass[2]
#kappa 
confusionMatrix(table(pred, testSet$dummy))$overall[2]

p=NCOL(classDatiVisibMean)-3
kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDatiVisibMean[idTest,]
  trainingSet=classDatiVisibMean[-idTest,]
  rf=randomForest(dummy~.-ritArr-partProg, trainingSet, mtry=p, ntree=B)
  pred=predict(rf, testSet)
  kappa[i]=confusionMatrix(table(pred, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
max(kappa)
mean(kappa)


#### using only the programmed partence time (partProg) ####
p=NCOL(classDatiVisibMean)-3
oobErrRate=c()
testErrRate=c()
kappa=c()
B=500

#we are just using a predictor variable so we are indeed doing a bagging because mtry=p
rf=randomForest(dummy~.-ritArr-visibMean, trainingSet, mtry=p, ntree=B)
pred=predict(rf, testSet)
#accuracy
confusionMatrix(table(pred, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(pred, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(pred, testSet$dummy))$byClass[2]
#kappa 
confusionMatrix(table(pred, testSet$dummy))$overall[2]

p=NCOL(classDatiVisibMean)-3
kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDatiVisibMean[idTest,]
  trainingSet=classDatiVisibMean[-idTest,]
  rf=randomForest(dummy~.-ritArr-visibMean, trainingSet, mtry=p, ntree=B)
  pred=predict(rf, testSet)
  kappa[i]=confusionMatrix(table(pred, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
max(kappa)
mean(kappa)




#### idk what I should do lol ####



#***************************************************************#
#### TRYING BALANCING CLASSES DOWNSAMPLING THE BIGGEST CLASS ####
#***************************************************************#

#"downSample" function recognize the biggest class and rebalance the frequency sampling from it
classDatiBal=downSample(subset(classDati, select=-c(dummy)), classDati$dummy , list=FALSE, yname = "dummy")
table(classDatiBal$dummy)

#extracting columns that have "visib" in the title name
visibBal=classDatiBal[, grepl("visib", colnames(classDatiBal))]

#a cute graph, not so significative I think
ggplot(data=classDatiBal, aes(x=rowMeans(visibBal), y = ritArr)) +
  geom_point(aes(color=dummy, shape=dummy)) +
  xlab("Visibility means") +  ylab("Delay's time") +
  ggtitle("")

#### defining the test and training set ####
n=NROW(classDatiBal)
idTest=sample(n, floor(n/3))
testSet=classDatiBal[idTest,]
trainingSet=classDatiBal[-idTest,]


#### Classification tree ####
delaysTreeBal=tree(dummy~.-ritArr, trainingSet)
summary(delaysTreeBal)
plot(delaysTreeBal)
text(delaysTreeBal)
varImp(delaysTreeBal)
treePredBal=predict(delaysTreeBal, testSet, type="class")
table(treePredBal, testSet$dummy)
confusionMatrix(treePredBal, testSet$dummy)
#accuracy
confusionMatrix(table(treePredBal, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(treePredBal, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(treePredBal, testSet$dummy))$byClass[2]
#kappa 
confusionMatrix(table(treePredBal, testSet$dummy))$overall[2]

kappa=c()
for (i in 1:100)
  { 
    idTest=sample(n, floor(n/3))
    testSet=classDatiBal[idTest,]
    trainingSet=classDatiBal[-idTest,]
    delaysTreeBal=tree(dummy~.-ritArr, trainingSet)
    treePredBal=predict(delaysTreeBal, testSet, type="class")
    kappa[i]=confusionMatrix(table(treePredBal, testSet$dummy))$overall[2]
  }

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="N. iterations")
max(kappa)
mean(kappa)


#### Balanced bagging ####
p=NCOL(classDatiBal)-2 #tolgo ritArr e dummy
delaysBaggBal=randomForest(dummy~.-ritArr, trainingSet, mtry=p, importance=TRUE, ntree=500)
delaysBaggBal
baggPredBal=predict(delaysBaggBal, testSet)
#accuracy
confusionMatrix(table(baggPredBal, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(baggPredBal, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(baggPredBal, testSet$dummy))$byClass[2]
#kappa 
confusionMatrix(table(baggPredBal, testSet$dummy))$overall[2]
#variables importance
varImpPlot(delaysBaggBal)


p=NCOL(classDatiBal)-2 #tolgo ritArr e dummy
kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDatiBal[idTest,]
  trainingSet=classDatiBal[-idTest,]
  delaysBaggBal=randomForest(dummy~.-ritArr, trainingSet, mtry=p, importance=TRUE, ntree=500)
  baggPredBal=predict(delaysBaggBal, testSet)
  kappa[i]=confusionMatrix(table(baggPredBal, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
max(kappa)
mean(kappa)


#### Balanced random forest ####
idTest=sample(n, floor(n/3))
testSet=classDatiBal[idTest,]
trainingSet=classDatiBal[-idTest,]

p=3
oobErrRateBal=c()
testErrRateBal=c()
B=500
spec=c()
kappa=c()


for(mtry in 1:p){
  rfBal=randomForest(dummy~partProg+fenFiglVal+fenAssisi-ritArr, trainingSet, mtry=mtry, ntree=B)
  oobErrRateBal[mtry]=rfBal$err.rate[500] #oob error rate 
  predBal=predict(rfBal, testSet)
  #to compute test error let's do 1-accuracy
  testErrRateBal[mtry]=1-confusionMatrix(predBal, testSet$dummy)$overall[1]
  #specificity
  spec[mtry]=specificity(table(predBal, testSet$dummy))
  #kappa
  kappa[mtry]=confusionMatrix(predBal, testSet$dummy)$overall[2]
}

#test error
matplot(1:mtry, cbind(oobErrRateBal,testErrRateBal), pch=19 , col=c("#A20045", "#00484D"), type="b", ylab="Error rate", xlab="mtry")
legend("toprigh", legend=c("Out of Bag Error", "Test Error"), pch=19, col=c("#A20045", "#00484D"))

#kappa
matplot(1:mtry, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")

#this graph is only about the last rf runned
varImpPlot(rfBal) 





#### trying using only the visibility mean and partProg ####
#computing and aggregating mean of visibilities
classDatiBalMean=data.frame(classDatiBal, rowMeans(visibBal))
#renaming it
colnames(classDatiBalMean)[which(names(classDatiBalMean) == "classDatiBal.dummy")] <- "dummy"
colnames(classDatiBalMean)[which(names(classDatiBalMean) == "classDatiBal.partProg")] <- "partProg"
colnames(classDatiBalMean)[which(names(classDatiBalMean) == "classDatiBal.ritArr")] <- "ritArr"
colnames(classDatiBalMean)[which(names(classDatiBalMean) == "rowMeans.visibBal.")] <- "visibMean"

#using the previous idTest but on the dataset where we have the visibilities mean
testSet=classDatiBalMean[idTest,]
trainingSet=classDatiBalMean[-idTest,]

#model
p=3
oobErrRateBalMean=c()
testErrRateBalMean=c()
spec=c()
kappa=c()
B=500
meanKappa=c()

for(mtry in 1:p){
  rfBalMean=randomForest(dummy~partProg+fenFiglVal+fenAssisi-ritArr, trainingSet, mtry=mtry, ntree=B)
  oobErrRateBalMean[mtry]=rfBal$err.rate[500] #oob error rate 
  predBalMean=predict(rfBalMean, testSet)
  #to compute test error let's do 1-accuracy
  testErrRateBalMean[mtry]=1-confusionMatrix(predBalMean, testSet$dummy)$overall[1]
  #specificity
  spec[mtry]=specificity(table(predBalMean, testSet$dummy))
  #kappa
  kappa[mtry]=confusionMatrix(predBalMean, testSet$dummy)$overall[2]
}

#test error
matplot(1:mtry, cbind(oobErrRateBalMean,testErrRateBalMean), pch=19 , col=c("#A20045", "#00484D"), type="b", ylab="Error rate", xlab="mtry")
legend("toprigh", legend=c("Out of Bag Error", "Test Error"), pch=19, col=c("#A20045", "#00484D"))

#kappa
matplot(1:mtry, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")

#checking bagging for partProg and mean of visibilities
#### THIS IS ONE OF THE BEST MODEL ####
p=3
oobErrRateBalMean=c()
testErrRateBalMean=c()
kappa=c()
B=500

rfBalMean=randomForest(dummy~partProg+fenFiglVal+fenAssisi-ritArr, trainingSet, ntree=B)
oobErrRateBalMean=rfBal$err.rate[500] #oob error rate 
predBalMean=predict(rfBalMean, testSet)
#to compute test error let's do 1-accuracy
testErrRateBalMean=1-confusionMatrix(predBalMean, testSet$dummy)$overall[1]
#accuracy
confusionMatrix(table(predBalMean, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(predBalMean, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(predBalMean, testSet$dummy))$byClass[2]
#kappa
confusionMatrix(table(predBalMean, testSet$dummy))$overall[2]

B=500
kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDatiBalMean[idTest,]
  trainingSet=classDatiBalMean[-idTest,]
  rfBalMean=randomForest(dummy~partProg+fenFiglVal+fenAssisi-ritArr, trainingSet, mtry=mtry, ntree=B)
  predBalMean=predict(rfBalMean, testSet)
  kappa[i]=confusionMatrix(table(predBalMean, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
max(kappa)
mean(kappa)




#### trying using only the visibility mean ####
#### this is bagging ####
oobErrRateBalMean=c()
testErrRateBalMean=c()
kappa=c()
B=500

rfBalMean=randomForest(dummy~.-partProg-ritArr, trainingSet, ntree=B)
oobErrRateBalMean=rfBal$err.rate[500] #oob error rate 
predBalMean=predict(rfBalMean, testSet)
#to compute test error let's do 1-accuracy
testErrRateBalMean=1-confusionMatrix(predBalMean, testSet$dummy)$overall[1]
#accuracy
confusionMatrix(table(predBalMean, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(predBalMean, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(predBalMean, testSet$dummy))$byClass[2]
#kappa
confusionMatrix(table(predBalMean, testSet$dummy))$overall[2]

kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDatiBalMean[idTest,]
  trainingSet=classDatiBalMean[-idTest,]
  rfBalMean=randomForest(dummy~.-partProg-ritArr, trainingSet, ntree=B)
  predBalMean=predict(rfBalMean, testSet)
  kappa[i]=confusionMatrix(table(predBalMean, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
max(kappa)
mean(kappa)




#### trying using only the partProg ####
#### I THINK THIS IS THE BEST MODEL :') (even if k=0.29 it's not that cool) ####
## this is bagging ##
## this seems confirms the huge importance of partProg seen above ###
oobErrRateBalMean=c()
testErrRateBalMean=c()
kappa=c()
B=500

rfBalMean=randomForest(dummy~partProg-visibMean-ritArr, trainingSet, ntree=B)
oobErrRateBalMean=rfBal$err.rate[500] #oob error rate 
predBalMean=predict(rfBalMean, testSet)
#to compute test error let's do 1-accuracy
testErrRateBalMean=1-confusionMatrix(predBalMean, testSet$dummy)$overall[1]
#accuracy
confusionMatrix(table(predBalMean, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(predBalMean, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(predBalMean, testSet$dummy))$byClass[2]
#kappa
confusionMatrix(table(predBalMean, testSet$dummy))$overall[2]

kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDatiBalMean[idTest,]
  trainingSet=classDatiBalMean[-idTest,]
  rfBalMean=randomForest(dummy~partProg-visibMean-ritArr, trainingSet, ntree=B)
  predBalMean=predict(rfBalMean, testSet)
  kappa[i]=confusionMatrix(table(predBalMean, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
max(kappa)
mean(kappa)


#### SEEING THIS RESULTS IT SEEMS partProg IS REALLY USEFUL TO CLASSIFY DATA ####

#### Classification tree just using partProg ####
delaysTreeBal=tree(dummy~partProg-ritArr, trainingSet)
summary(delaysTreeBal)
plot(delaysTreeBal)
text(delaysTreeBal, pretty=0)
treePredBal=predict(delaysTreeBal, testSet, type="class")
table(treePredBal, testSet$dummy)
confusionMatrix(treePredBal, testSet$dummy)
#accuracy
confusionMatrix(table(treePredBal, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(treePredBal, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(treePredBal, testSet$dummy))$byClass[2]
#kappa 
confusionMatrix(table(treePredBal, testSet$dummy))$overall[2]

kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDatiBal[idTest,]
  trainingSet=classDatiBal[-idTest,]
  delaysTreeBal=tree(dummy~partProg-ritArr, trainingSet)
  treePredBal=predict(delaysTreeBal, testSet, type="class")
  kappa[i]=confusionMatrix(table(treePredBal, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
mean(kappa)


#### PER BEAMER CONFRONTARE QUESTI DUE ####
#### DUE ####

oobErrRateBal=c()
testErrRateBal=c()
kappa=c()
B=500

rfBal=randomForest(dummy~partProg+fenFiglVal+fenAssisi-ritArr, trainingSet, ntree=B)
varImpPlot(rfBal)
oobErrRateBal=rfBal$err.rate[500] #oob error rate 
predBal=predict(rfBal, testSet)
#to compute test error let's do 1-accuracy
testErrRateBalMean=1-confusionMatrix(predBalMean, testSet$dummy)$overall[1]
#accuracy
confusionMatrix(table(predBal, testSet$dummy))$overall[1]
#sensitivity
confusionMatrix(table(predBal, testSet$dummy))$byClass[1]
#specificity
confusionMatrix(table(predBal, testSet$dummy))$byClass[2]
#kappa
confusionMatrix(table(predBal, testSet$dummy))$overall[2]

B=500
kappa=c()
for (i in 1:100)
{ 
  idTest=sample(n, floor(n/3))
  testSet=classDatiBal[idTest,]
  trainingSet=classDatiBal[-idTest,]
  rfBal=randomForest(dummy~partProg+fenFiglVal+fenAssisi-ritArr, trainingSet, ntree=B)
  predBal=predict(rfBal, testSet)
  kappa[i]=confusionMatrix(table(predBal, testSet$dummy))$overall[2]
}

matplot(1:100, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")
max(kappa)
mean(kappa)








#### BALANCED RANDOM FOREST ####
idTest=sample(n, floor(n/3))
testSet=classDatiBal[idTest,]
trainingSet=classDatiBal[-idTest,]

p=3
oobErrRateBalMean=c()
testErrRateBalMean=c()
spec=c()
kappa=c()
B=500
meanKappa=c()

for(mtry in 1:p){
  rfBalMean=randomForest(dummy~partProg+fenFiglVal+fenAssisi-ritArr, trainingSet, mtry=mtry, ntree=B)
  oobErrRateBalMean[mtry]=rfBal$err.rate[500] #oob error rate 
  predBalMean=predict(rfBalMean, testSet)
  #to compute test error let's do 1-accuracy
  testErrRateBalMean[mtry]=1-confusionMatrix(predBalMean, testSet$dummy)$overall[1]
  #specificity
  spec[mtry]=specificity(table(predBalMean, testSet$dummy))
  #kappa
  kappa[mtry]=confusionMatrix(predBalMean, testSet$dummy)$overall[2]
}

#test error
matplot(1:mtry, cbind(oobErrRateBalMean,testErrRateBalMean), pch=19 , col=c("#A20045", "#00484D"), type="b", ylab="Error rate", xlab="mtry")
legend("toprigh", legend=c("Out of Bag Error", "Test Error"), pch=19, col=c("#A20045", "#00484D"))

#kappa
matplot(1:mtry, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")


#### UNBALANCED ####
idTest=sample(n, floor(n/3))
testSet=classDati[idTest,]
trainingSet=classDati[-idTest,]


p=3
oobErrRateBalMean=c()
testErrRateBalMean=c()
spec=c()
kappa=c()
B=500
meanKappa=c()

for(mtry in 1:p){
  rfBalMean=randomForest(dummy~partProg+fenFiglVal+fenAssisi-ritArr, trainingSet, mtry=mtry, ntree=B)
  oobErrRateBalMean[mtry]=rfBal$err.rate[500] #oob error rate 
  predBalMean=predict(rfBalMean, testSet)
  #to compute test error let's do 1-accuracy
  testErrRateBalMean[mtry]=1-confusionMatrix(predBalMean, testSet$dummy)$overall[1]
  #specificity
  spec[mtry]=specificity(table(predBalMean, testSet$dummy))
  #kappa
  kappa[mtry]=confusionMatrix(predBalMean, testSet$dummy)$overall[2]
}

#test error
matplot(1:mtry, cbind(oobErrRateBalMean,testErrRateBalMean), pch=19 , col=c("#A20045", "#00484D"), type="b", ylab="Error rate", xlab="mtry")
legend("topright", legend=c("Out of Bag Error", "Test Error"), pch=19, col=c("#A20045", "#00484D"))

#kappa
matplot(1:mtry, cbind(kappa), pch=19 , col=c("#A20045", "#00484D"),type="b", ylab="Kappa value", xlab="mtry")



#### GRAFICI ####
#extracting columns of visibilites
visib=datiDelays[, grepl("visib", colnames(datiDelays))]
visibBal=classDatiBal[, grepl("visib", colnames(classDatiBal))]

fen=datiDelays[, grepl("fen", colnames(datiDelays))]

#a not really cool graph
ggplot(data=classDati, aes(x = rowMeans(visib), y = ritArr)) +
  geom_point(aes(color=dummy, shape=dummy)) +
  xlab("Visibility means") +  ylab("Delay's time") +
  ggtitle("")

#unbalanced
ggplot(classDati, aes(rowMeans(visib), fill=dummy)) +
  geom_histogram(bins=30)

#balanced
ggplot(classDatiBal, aes(ritArr, fill=dummy)) +
  geom_histogram(bins=60) +
  xlab("Minutes of delay") +
  ylab("Frequency") +
  scale_fill_manual(name="Type of delay", values=c("red","blue"), labels=c("Less than 10 minutes","Greater than 10 minutes"))

#unbalanced
ggplot(classDati, aes(ritArr, fill=dummy)) +
  geom_histogram(bins=60) +
  xlab("Minutes of delay") +
  ylab("Frequency") +
  scale_fill_manual(name="Type of delay", values=c("red","blue"), labels=c("Less than 10 minutes","Greater than 10 minutes"))


as.numeric(classDati$dummy)
hist(as.numeric(classDati$dummy))
hist(as.numeric(classDatiBal$dummy))

ggplot(classDati, aes(ritArr, colour=dummy)) +
  geom_freqpoly(binwidth = 5)+
  coord_cartesian(xlim=c(0,100), ylim=c(0,400))

ggplot(classDatiBal, aes(ritArr, colour=dummy)) +
  geom_freqpoly(binwidth = 10) 


  
NROW(classDati)
NROW(classDati[which(classDati$ritArr>10),])
NROW(classDati[which(classDati$ritArr<=10),])
