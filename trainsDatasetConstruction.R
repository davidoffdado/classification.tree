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

#********************#
#### LOADING DATA ####
#********************#

#### treno 4084 ####

#url="https://trainstats.altervista.org/cercatreno.php?ref=cr&treno=4084&stazpart=FOLIGNO&stazarr=FIRENZE%20S.M.N."
#x=url %>%
#  session() %>%
#  html_nodes(xpath='/html/body/div[2]/table/tbody') %>%
#  html_table()
treno4084=read.csv("treno4084.csv", sep=",")
treno4084=subset(treno4084, select=-c(X))
#write.csv(treno4084, "C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/CONTEST Train/treno4084.csv")
colnames(treno4084)=c("Giorno", "Data", "stazPart", "partProg", "ritPart", "stazArr", "arrProg", "ritArr")

#### treno 4070 ####

#url="https://trainstats.altervista.org/cercatreno.php?ref=cr&treno=4070&stazpart=FOLIGNO&stazarr=FIRENZE%20S.M.N."
#x=url %>%
#  session() %>%
#  html_nodes(xpath='/html/body/div[2]/table/tbody') %>%
#  html_table()

treno4070=read.csv("treno4070.csv", sep=",")
treno4070=subset(treno4070, select=-c(X))
#write.csv(treno4070, "C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/CONTEST Train/treno4070.csv")
colnames(treno4070)=c("Giorno", "Data", "stazPart", "partProg", "ritPart", "stazArr", "arrProg", "ritArr")

#### treno 4072 ####

#url="https://trainstats.altervista.org/cercatreno.php?ref=cr&treno=4072&stazpart=FOLIGNO&stazarr=FIRENZE%20S.M.N."
#x=url %>%
#  session() %>%
#  html_nodes(xpath='/html/body/div[2]/table/tbody') %>%
#  html_table()

treno4072=read.csv("treno4072.csv", sep=",")
treno4072=subset(treno4072, select=-c(X))
#write.csv(treno4072, "C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/CONTEST Train/treno4072.csv")
colnames(treno4072)=c("Giorno", "Data", "stazPart", "partProg", "ritPart", "stazArr", "arrProg", "ritArr")


#### treno 4074 ####

#url="https://trainstats.altervista.org/cercatreno.php?ref=cr&treno=4074&stazpart=FOLIGNO&stazarr=FIRENZE%20S.M.N."
#x=url %>%
#  session() %>%
#  html_nodes(xpath='/html/body/div[2]/table/tbody') %>%
#  html_table()

treno4074=read.csv("treno4074.csv", sep=",")
treno4074=subset(treno4074, select=-c(X))
#write.csv(treno4074, "C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/CONTEST Train/treno4074.csv")
colnames(treno4074)=c("Giorno", "Data", "stazPart", "partProg", "ritPart", "stazArr", "arrProg", "ritArr")

#### treno 4076 ####

#url="https://trainstats.altervista.org/cercatreno.php?ref=cr&treno=4076&stazpart=FOLIGNO&stazarr=FIRENZE%20S.M.N."
#x=url %>%
#  session() %>%
#  html_nodes(xpath='/html/body/div[2]/table/tbody') %>%
#  html_table()

treno4076=read.csv("treno4076.csv", sep=",")
treno4076=subset(treno4076, select=-c(X))
#write.csv(treno4076, "C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/CONTEST Train/treno4076.csv")
colnames(treno4076)=c("Giorno", "Data", "stazPart", "partProg", "ritPart", "stazArr", "arrProg", "ritArr")

#### treno 4080 ####

#url="https://trainstats.altervista.org/cercatreno.php?ref=cr&treno=4080&stazpart=FOLIGNO&stazarr=FIRENZE%20S.M.N."
#x=url %>%
#  session() %>%
#  html_nodes(xpath='/html/body/div[2]/table/tbody') %>%
#  html_table()

treno4080=read.csv("treno4080.csv", sep=",")
treno4080=subset(treno4080, select=-c(X))
#write.csv(treno4080, "C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/CONTEST Train/treno4080.csv")
colnames(treno4080)=c("Giorno", "Data", "stazPart", "partProg", "ritPart", "stazArr", "arrProg", "ritArr")


#### treno 4082 ####

#url="https://trainstats.altervista.org/cercatreno.php?ref=cr&treno=4082&stazpart=FOLIGNO&stazarr=FIRENZE%20S.M.N."
#x=url %>%
#  session() %>%
#  html_nodes(xpath='/html/body/div[2]/table/tbody') %>%
#  html_table()

treno4082=read.csv("treno4082.csv", sep=",")
treno4082=subset(treno4082, select=-c(X))
#write.csv(treno4082, "C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/CONTEST Train/treno4082.csv")
colnames(treno4082)=c("Giorno", "Data", "stazPart", "partProg", "ritPart", "stazArr", "arrProg", "ritArr")

#### aggregating data train ####

treniAll=rbind(treno4070, treno4072, treno4076, treno4080, treno4082, treno4084)
View(treniAll)
treniAll$Data=as.Date(treniAll$Data, "%d/%m/%Y")
treniAll=treniAll[order(treniAll$Data),]
View(treniAll)
#### adding weather data ####
#deleting the dates of treniAll when we don't have weather data
treniAll=treniAll[-which(treniAll$Data=="2022-12-05" | treniAll$Data=="2022-12-06"), ]
#loading weather file
write.csv(treniAll, "C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/CONTEST Train/treniAll.csv")
weatherAll=read.csv("weatherAll.csv", sep=",")
weatherAll=subset(weatherAll, select=-c(X))


#********************************#
#### JOINING THE TWO DATASETS ####
#********************************#

treni=cbind(treniAll, weatherAll)

write.csv(treni, "C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/CONTEST Train/datiTreni.csv")


#********************************#
#### UN PO' DI GRAFICI CARINI ####
#********************************#

table(treniAll$ritArr)

ggplot(treniAll, aes(ritArr))+
  geom_freqpoly()

ggplot(treniAll, aes(ritArr, after_stat(density), fill=fenFire))+
  geom_histogram()+
  coord_cartesian(xlim=c(5,100), ylim=c(0, 1))
