getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(stringr)
library(readxl)
library(splitstackshape)
library(ggplot2)
library(tidyverse)
library(fitdistrplus)

#********************#
#### LOADING DATA ####
#********************#

datiTreni=read.csv("datiTreni.csv", sep=",")
datiTreni=subset(datiTreni, select=-c(X))

#### deleting the observation with delay < 0 ####
datiDelays=datiTreni[-which(datiTreni$ritArr<0), ]
#### deleting not used columns ####
datiDelays=subset(datiDelays, select=-c(Giorno, stazPart, ritPart, stazArr, arrProg))




##################################
#### UN PO' DI GRAFICI CARINI ####
##################################

#sembrerebbe che con una maggiore visibilità si faccia più ritardo, ma in realtà questo è dovuto al fatto che ci sono più giorni con buona visibilità
#che giorni con scarsa visibilità, quindi la probabilità di treni in ritardo aumenta
#notiamo che è la pioggia a ridurre maggiormente la visibilità e non la nebbia
ggplot(datiDelays, aes(partProg, ritArr, colour=fenFoligno)) +
  geom_point()

ggplot(datiDelays, aes(ritArr))+
  geom_freqpoly()

#
NROW(table(datiDelays$ritArr))

ggplot(datiDelays, aes(ritArr)) +
  geom_histogram()

#
NROW(table(datiTreni$ritArr))


ggplot(datiTreni, aes(ritArr)) +
  geom_histogram(bins=100) +
  geom_vline(data=datiTreni, aes(xintercept=1, color="red"), linetype="dashed", size=0.5) +
  geom_vline(data=datiTreni, aes(xintercept=2, color="red"), linetype="dashed", size=0.5) +
  geom_vline(data=datiTreni, aes(xintercept=-1, color="red"), linetype="dashed", size=0.5) +
  geom_vline(data=datiTreni, aes(xintercept=5, color="blue"), linetype="dashed", size=0.5) 

ggplot(datiDelays, aes(ritArr)) +
  geom_histogram(bins=100) +
  geom_vline(data=datiDelays, aes(xintercept=1, color="red"), linetype="dashed", size=0.5) +
  geom_vline(data=datiDelays, aes(xintercept=2, color="red"), linetype="dashed", size=0.5) +
  geom_vline(data=datiDelays, aes(xintercept=5, color="blue"), linetype="dashed", size=0.5) 



hist(datiDelays$ritArr, freq=FALSE)
lines(density(datiDelays$ritArr))
  

descdist(datiDelays$ritArr, discrete=FALSE)


plot(fitdist(datiDelays$ritArr, "norm"))
plot(fitdist(datiDelays$ritArr, "exp"))
