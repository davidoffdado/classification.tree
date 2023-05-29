#*************************#
#### SETTING DIRECTORY ####
#*************************#

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#****************************#
#### INSTALLING LIBRARIES ####
#****************************#

library(readxl)
library(splitstackshape)

#********************#
#### LOADING DATA ####
#********************#

#### firenze ####
febFi=read.csv("Firenze-2022-Febbraio.csv", sep=";")
marFi=read.csv("Firenze-2022-Marzo.csv", sep=";")
aprFi=read.csv("Firenze-2022-Aprile.csv", sep=";")
magFi=read.csv("Firenze-2022-Maggio.csv", sep=";")
giuFi=read.csv("Firenze-2022-Giugno.csv", sep=";")
lugFi=read.csv("Firenze-2022-Luglio.csv", sep=";")
agoFi=read.csv("Firenze-2022-Agosto.csv", sep=";")
setFi=read.csv("Firenze-2022-Settembre.csv", sep=";")
ottFi=read.csv("Firenze-2022-Ottobre.csv", sep=";")
novFi=read.csv("Firenze-2022-Novembre.csv", sep=";")
dicFi=read.csv("Firenze-2022-Dicembre.csv", sep=";")
fireWeat=rbind(febFi, marFi, aprFi, magFi, giuFi, lugFi, agoFi, setFi, ottFi, novFi, dicFi)

#definisco il fenomeno "normale" invece di avere ""
levels(fireWeat$FENOMENI)[levels(fireWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
fireWeat=subset(fireWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(fireWeat)[which(names(fireWeat)=="FENOMENI")]="fenFire"
colnames(fireWeat)[which(names(fireWeat)=="VISIBILITA.km")]="visibFire"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
fireWeat=fireWeat[-which(fireWeat$DATA=="1/2/2022" | fireWeat$DATA=="2/2/2022" | fireWeat$DATA=="3/2/2022" | fireWeat$DATA=="4/2/2022" | fireWeat$DATA=="5/2/2022" | fireWeat$DATA=="6/2/2022" | fireWeat$DATA=="7/2/2022" | fireWeat$DATA=="8/2/2022" |fireWeat$DATA=="9/2/2022" | fireWeat$DATA=="10/2/2022" | fireWeat$DATA=="11/2/2022" | fireWeat$DATA=="12/2/2022" | fireWeat$DATA=="13/2/2022" | fireWeat$DATA=="14/2/2022"),]


#### Foligno ####
febFoli=read.csv("Foligno-2022-Febbraio.csv", sep=";")
marFoli=read.csv("Foligno-2022-Marzo.csv", sep=";")
aprFoli=read.csv("Foligno-2022-Aprile.csv", sep=";")
magFoli=read.csv("Foligno-2022-Maggio.csv", sep=";")
giuFoli=read.csv("Foligno-2022-Giugno.csv", sep=";")
lugFoli=read.csv("Foligno-2022-Luglio.csv", sep=";")
agoFoli=read.csv("Foligno-2022-Agosto.csv", sep=";")
setFoli=read.csv("Foligno-2022-Settembre.csv", sep=";")
ottFoli=read.csv("Foligno-2022-Ottobre.csv", sep=";")
novFoli=read.csv("Foligno-2022-Novembre.csv", sep=";")
dicFoli=read.csv("Foligno-2022-Dicembre.csv", sep=";")
dicFoli=dicFoli[-which(dicFoli$DATA=="5/12/2022"),]
FolignoWeat=rbind(febFoli, marFoli, aprFoli, magFoli, giuFoli, lugFoli, agoFoli, setFoli, ottFoli, novFoli, dicFoli)


#removing the first 14 days of february because data starts from 15/02
FolignoWeat=FolignoWeat[-which(FolignoWeat$DATA=="1/2/2022" | FolignoWeat$DATA=="2/2/2022" | FolignoWeat$DATA=="3/2/2022" | FolignoWeat$DATA=="4/2/2022" | FolignoWeat$DATA=="5/2/2022" | FolignoWeat$DATA=="6/2/2022" | FolignoWeat$DATA=="7/2/2022" | FolignoWeat$DATA=="8/2/2022" |FolignoWeat$DATA=="9/2/2022" | FolignoWeat$DATA=="10/2/2022" | FolignoWeat$DATA=="11/2/2022" | FolignoWeat$DATA=="12/2/2022" | FolignoWeat$DATA=="13/2/2022" | FolignoWeat$DATA=="14/2/2022"),]
#definisco il fenomeno "normale" invece di avere ""
levels(FolignoWeat$FENOMENI)[levels(FolignoWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
FolignoWeat=subset(FolignoWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(FolignoWeat)[which(names(FolignoWeat)=="FENOMENI")]="fenFoligno"
colnames(FolignoWeat)[which(names(FolignoWeat)=="VISIBILITA.km")]="visibFoligno"

#### Spello ####
febSpe=read.csv("Spello-2022-Febbraio.csv", sep=";")
marSpe=read.csv("Spello-2022-Marzo.csv", sep=";")
aprSpe=read.csv("Spello-2022-Aprile.csv", sep=";")
magSpe=read.csv("Spello-2022-Maggio.csv", sep=";")
giuSpe=read.csv("Spello-2022-Giugno.csv", sep=";")
lugSpe=read.csv("Spello-2022-Luglio.csv", sep=";")
agoSpe=read.csv("Spello-2022-Agosto.csv", sep=";")
setSpe=read.csv("Spello-2022-Settembre.csv", sep=";")
ottSpe=read.csv("Spello-2022-Ottobre.csv", sep=";")
novSpe=read.csv("Spello-2022-Novembre.csv", sep=";")
dicSpe=read.csv("Spello-2022-Dicembre.csv", sep=";")
dicSpe=dicSpe[-which(dicSpe$DATA=="5/12/2022"),]
spelloWeat=rbind(febSpe, marSpe, aprSpe, magSpe, giuSpe, lugSpe, agoSpe, setSpe, ottSpe, novSpe, dicSpe)


#definisco il fenomeno "normale" invece di avere ""
levels(spelloWeat$FENOMENI)[levels(spelloWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
spelloWeat=subset(spelloWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(spelloWeat)[which(names(spelloWeat)=="FENOMENI")]="fenSpello"
colnames(spelloWeat)[which(names(spelloWeat)=="VISIBILITA.km")]="visibSpello"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
spelloWeat=spelloWeat[-which(spelloWeat$DATA=="1/2/2022" | spelloWeat$DATA=="2/2/2022" | spelloWeat$DATA=="3/2/2022" | spelloWeat$DATA=="4/2/2022" | spelloWeat$DATA=="5/2/2022" | spelloWeat$DATA=="6/2/2022" | spelloWeat$DATA=="7/2/2022" | spelloWeat$DATA=="8/2/2022" |spelloWeat$DATA=="9/2/2022" | spelloWeat$DATA=="10/2/2022" | spelloWeat$DATA=="11/2/2022" | spelloWeat$DATA=="12/2/2022" | spelloWeat$DATA=="13/2/2022" | spelloWeat$DATA=="14/2/2022"),]


#### assisi ####
febAss=read.csv("Assisi-2022-Febbraio.csv", sep=";")
marAss=read.csv("Assisi-2022-Marzo.csv", sep=";")
aprAss=read.csv("Assisi-2022-Aprile.csv", sep=";")
magAss=read.csv("Assisi-2022-Maggio.csv", sep=";")
giuAss=read.csv("Assisi-2022-Giugno.csv", sep=";")
lugAss=read.csv("Assisi-2022-Luglio.csv", sep=";")
agoAss=read.csv("Assisi-2022-Agosto.csv", sep=";")
setAss=read.csv("Assisi-2022-Settembre.csv", sep=";")
ottAss=read.csv("Assisi-2022-Ottobre.csv", sep=";")
novAss=read.csv("Assisi-2022-Novembre.csv", sep=";")
dicAss=read.csv("Assisi-2022-Dicembre.csv", sep=";")
dicAss=dicAss[-which(dicAss$DATA=="5/12/2022"),]
assisiWeat=rbind(febAss, marAss, aprAss, magAss, giuAss, lugAss, agoAss, setAss, ottAss, novAss, dicAss)


#definisco il fenomeno "normale" invece di avere ""
levels(assisiWeat$FENOMENI)[levels(assisiWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
assisiWeat=subset(assisiWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(assisiWeat)[which(names(assisiWeat)=="FENOMENI")]="fenAssisi"
colnames(assisiWeat)[which(names(assisiWeat)=="VISIBILITA.km")]="visibAssisi"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
assisiWeat=assisiWeat[-which(assisiWeat$DATA=="1/2/2022" | assisiWeat$DATA=="2/2/2022" | assisiWeat$DATA=="3/2/2022" | assisiWeat$DATA=="4/2/2022" | assisiWeat$DATA=="5/2/2022" | assisiWeat$DATA=="6/2/2022" | assisiWeat$DATA=="7/2/2022" | assisiWeat$DATA=="8/2/2022" |assisiWeat$DATA=="9/2/2022" | assisiWeat$DATA=="10/2/2022" | assisiWeat$DATA=="11/2/2022" | assisiWeat$DATA=="12/2/2022" | assisiWeat$DATA=="13/2/2022" | assisiWeat$DATA=="14/2/2022"),]

#### bastia ####
febBas=read.csv("Bastia Umbra-2022-Febbraio.csv", sep=";")
marBas=read.csv("Bastia Umbra-2022-Marzo.csv", sep=";")
aprBas=read.csv("Bastia Umbra-2022-Aprile.csv", sep=";")
magBas=read.csv("Bastia Umbra-2022-Maggio.csv", sep=";")
giuBas=read.csv("Bastia Umbra-2022-Giugno.csv", sep=";")
lugBas=read.csv("Bastia Umbra-2022-Luglio.csv", sep=";")
agoBas=read.csv("Bastia Umbra-2022-Agosto.csv", sep=";")
setBas=read.csv("Bastia Umbra-2022-Settembre.csv", sep=";")
ottBas=read.csv("Bastia Umbra-2022-Ottobre.csv", sep=";")
novBas=read.csv("Bastia Umbra-2022-Novembre.csv", sep=";")
dicBas=read.csv("Bastia Umbra-2022-Dicembre.csv", sep=";")
dicBas=dicBas[-which(dicBas$DATA=="5/12/2022"),]
bastiaWeat=rbind(febbas, marbas, aprbas, magbas, giubas, lugbas, agobas, setbas, ottbas, novbas, dicbas)


#definisco il fenomeno "normale" invece di avere ""
levels(bastiaWeat$FENOMENI)[levels(bastiaWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
bastiaWeat=subset(bastiaWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(bastiaWeat)[which(names(bastiaWeat)=="FENOMENI")]="fenBastia"
colnames(bastiaWeat)[which(names(bastiaWeat)=="VISIBILITA.km")]="visibBastia"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
bastiaWeat=bastiaWeat[-which(bastiaWeat$DATA=="1/2/2022" | bastiaWeat$DATA=="2/2/2022" | bastiaWeat$DATA=="3/2/2022" | bastiaWeat$DATA=="4/2/2022" | bastiaWeat$DATA=="5/2/2022" | bastiaWeat$DATA=="6/2/2022" | bastiaWeat$DATA=="7/2/2022" | bastiaWeat$DATA=="8/2/2022" |bastiaWeat$DATA=="9/2/2022" | bastiaWeat$DATA=="10/2/2022" | bastiaWeat$DATA=="11/2/2022" | bastiaWeat$DATA=="12/2/2022" | bastiaWeat$DATA=="13/2/2022" | bastiaWeat$DATA=="14/2/2022"),]

#### Perugia ####
febPer=read.csv("Perugia-2022-Febbraio.csv", sep=";")
marPer=read.csv("Perugia-2022-Marzo.csv", sep=";")
aprPer=read.csv("Perugia-2022-Aprile.csv", sep=";")
magPer=read.csv("Perugia-2022-Maggio.csv", sep=";")
giuPer=read.csv("Perugia-2022-Giugno.csv", sep=";")
lugPer=read.csv("Perugia-2022-Luglio.csv", sep=";")
agoPer=read.csv("Perugia-2022-Agosto.csv", sep=";")
setPer=read.csv("Perugia-2022-Settembre.csv", sep=";")
ottPer=read.csv("Perugia-2022-Ottobre.csv", sep=";")
novPer=read.csv("Perugia-2022-Novembre.csv", sep=";")
dicPer=read.csv("Perugia-2022-Dicembre.csv", sep=";")
dicPer=dicPer[-which(dicPer$DATA=="5/12/2022"),]
peruWeat=rbind(febPer, marPer, aprPer, magPer, giuPer, lugPer, agoPer, setPer, ottPer, novPer, dicPer)


#definisco il fenomeno "normale" invece di avere ""
levels(peruWeat$FENOMENI)[levels(peruWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
peruWeat=subset(peruWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(peruWeat)[which(names(peruWeat)=="FENOMENI")]="fenPeru"
colnames(peruWeat)[which(names(peruWeat)=="VISIBILITA.km")]="visibPeru"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
peruWeat=peruWeat[-which(peruWeat$DATA=="1/2/2022" | peruWeat$DATA=="2/2/2022" | peruWeat$DATA=="3/2/2022" | peruWeat$DATA=="4/2/2022" | peruWeat$DATA=="5/2/2022" | peruWeat$DATA=="6/2/2022" | peruWeat$DATA=="7/2/2022" | peruWeat$DATA=="8/2/2022" |peruWeat$DATA=="9/2/2022" | peruWeat$DATA=="10/2/2022" | peruWeat$DATA=="11/2/2022" | peruWeat$DATA=="12/2/2022" | peruWeat$DATA=="13/2/2022" | peruWeat$DATA=="14/2/2022"),]

#### Corciano ####
febCor=read.csv("Corciano-2022-Febbraio.csv", sep=";")
marCor=read.csv("Corciano-2022-Marzo.csv", sep=";")
aprCor=read.csv("Corciano-2022-Aprile.csv", sep=";")
magCor=read.csv("Corciano-2022-Maggio.csv", sep=";")
giuCor=read.csv("Corciano-2022-Giugno.csv", sep=";")
lugCor=read.csv("Corciano-2022-Luglio.csv", sep=";")
agoCor=read.csv("Corciano-2022-Agosto.csv", sep=";")
setCor=read.csv("Corciano-2022-Settembre.csv", sep=";")
ottCor=read.csv("Corciano-2022-Ottobre.csv", sep=";")
novCor=read.csv("Corciano-2022-Novembre.csv", sep=";")
dicCor=read.csv("Corciano-2022-Dicembre.csv", sep=";")
dicCor=dicCor[-which(dicCor$DATA=="5/12/2022"),]
corcianoWeat=rbind(febCor, marCor, aprCor, magCor, giuCor, lugCor, agoCor, setCor, ottCor, novCor, dicCor)



#definisco il fenomeno "normale" invece di avere ""
levels(corcianoWeat$FENOMENI)[levels(corcianoWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
corcianoWeat=subset(corcianoWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(corcianoWeat)[which(names(corcianoWeat)=="FENOMENI")]="fenCorciano"
colnames(corcianoWeat)[which(names(corcianoWeat)=="VISIBILITA.km")]="visibCorciano"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
corcianoWeat=corcianoWeat[-which(corcianoWeat$DATA=="1/2/2022" | corcianoWeat$DATA=="2/2/2022" | corcianoWeat$DATA=="3/2/2022" | corcianoWeat$DATA=="4/2/2022" | corcianoWeat$DATA=="5/2/2022" | corcianoWeat$DATA=="6/2/2022" | corcianoWeat$DATA=="7/2/2022" | corcianoWeat$DATA=="8/2/2022" |corcianoWeat$DATA=="9/2/2022" | corcianoWeat$DATA=="10/2/2022" | corcianoWeat$DATA=="11/2/2022" | corcianoWeat$DATA=="12/2/2022" | corcianoWeat$DATA=="13/2/2022" | corcianoWeat$DATA=="14/2/2022"),]

#### Magione ####
febMag=read.csv("Magione-2022-Febbraio.csv", sep=";")
marMag=read.csv("Magione-2022-Marzo.csv", sep=";")
aprMag=read.csv("Magione-2022-Aprile.csv", sep=";")
magMag=read.csv("Magione-2022-Maggio.csv", sep=";")
giuMag=read.csv("Magione-2022-Giugno.csv", sep=";")
lugMag=read.csv("Magione-2022-Luglio.csv", sep=";")
agoMag=read.csv("Magione-2022-Agosto.csv", sep=";")
setMag=read.csv("Magione-2022-Settembre.csv", sep=";")
ottMag=read.csv("Magione-2022-Ottobre.csv", sep=";")
novMag=read.csv("Magione-2022-Novembre.csv", sep=";")
dicMag=read.csv("Magione-2022-Dicembre.csv", sep=";")
dicMag=dicMag[-which(dicMag$DATA=="5/12/2022"),]
magWeat=rbind(febMag, marMag, aprMag, magMag, giuMag, lugMag, agoMag, setMag, ottMag, novMag, dicMag)


#definisco il fenomeno "normale" invece di avere ""
levels(magWeat$FENOMENI)[levels(magWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
magWeat=subset(magWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(magWeat)[which(names(magWeat)=="FENOMENI")]="fenMag"
colnames(magWeat)[which(names(magWeat)=="VISIBILITA.km")]="visibMag"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
magWeat=magWeat[-which(magWeat$DATA=="1/2/2022" | magWeat$DATA=="2/2/2022" | magWeat$DATA=="3/2/2022" | magWeat$DATA=="4/2/2022" | magWeat$DATA=="5/2/2022" | magWeat$DATA=="6/2/2022" | magWeat$DATA=="7/2/2022" | magWeat$DATA=="8/2/2022" |magWeat$DATA=="9/2/2022" | magWeat$DATA=="10/2/2022" | magWeat$DATA=="11/2/2022" | magWeat$DATA=="12/2/2022" | magWeat$DATA=="13/2/2022" | magWeat$DATA=="14/2/2022"),]




#### Passignano sul Trasimeno ####
febPass=read.csv("Passignano sul Trasimeno-2022-Febbraio.csv", sep=";")
marPass=read.csv("Passignano sul Trasimeno-2022-Marzo.csv", sep=";")
aprPass=read.csv("Passignano sul Trasimeno-2022-Aprile.csv", sep=";")
magPass=read.csv("Passignano sul Trasimeno-2022-Maggio.csv", sep=";")
giuPass=read.csv("Passignano sul Trasimeno-2022-Giugno.csv", sep=";")
lugPass=read.csv("Passignano sul Trasimeno-2022-Luglio.csv", sep=";")
agoPass=read.csv("Passignano sul Trasimeno-2022-Agosto.csv", sep=";")
setPass=read.csv("Passignano sul Trasimeno-2022-Settembre.csv", sep=";")
ottPass=read.csv("Passignano sul Trasimeno-2022-Ottobre.csv", sep=";")
novPass=read.csv("Passignano sul Trasimeno-2022-Novembre.csv", sep=";")
dicPass=read.csv("Passignano sul Trasimeno-2022-Dicembre.csv", sep=";")
dicPass=dicPass[-which(dicPass$DATA=="5/12/2022"), ]
PassWeat=rbind(febPass, marPass, aprPass, magPass, giuPass, lugPass, agoPass, setPass, ottPass, novPass, dicPass)


#definisco il fenomeno "normale" invece di avere ""
levels(PassWeat$FENOMENI)[levels(PassWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
PassWeat=subset(PassWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(PassWeat)[which(names(PassWeat)=="FENOMENI")]="fenPass"
colnames(PassWeat)[which(names(PassWeat)=="VISIBILITA.km")]="visibPass"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
PassWeat=PassWeat[-which(PassWeat$DATA=="1/2/2022" | PassWeat$DATA=="2/2/2022" | PassWeat$DATA=="3/2/2022" | PassWeat$DATA=="4/2/2022" | PassWeat$DATA=="5/2/2022" | PassWeat$DATA=="6/2/2022" | PassWeat$DATA=="7/2/2022" | PassWeat$DATA=="8/2/2022" |PassWeat$DATA=="9/2/2022" | PassWeat$DATA=="10/2/2022" | PassWeat$DATA=="11/2/2022" | PassWeat$DATA=="12/2/2022" | PassWeat$DATA=="13/2/2022" | PassWeat$DATA=="14/2/2022"),]


#### Cortona ####
febCort=read.csv("Cortona-2022-Febbraio.csv", sep=";")
marCort=read.csv("Cortona-2022-Marzo.csv", sep=";")
aprCort=read.csv("Cortona-2022-Aprile.csv", sep=";")
magCort=read.csv("Cortona-2022-Maggio.csv", sep=";")
giuCort=read.csv("Cortona-2022-Giugno.csv", sep=";")
lugCort=read.csv("Cortona-2022-Luglio.csv", sep=";")
agoCort=read.csv("Cortona-2022-Agosto.csv", sep=";")
setCort=read.csv("Cortona-2022-Settembre.csv", sep=";")
ottCort=read.csv("Cortona-2022-Ottobre.csv", sep=";")
novCort=read.csv("Cortona-2022-Novembre.csv", sep=";")
dicCort=read.csv("Cortona-2022-Dicembre.csv", sep=";")
dicCort=dicCort[-which(dicCort$DATA=="5/12/2022"),]
CortWeat=rbind(febCort, marCort, aprCort, magCort, giuCort, lugCort, agoCort, setCort, ottCort, novCort, dicCort)




#definisco il fenomeno "normale" invece di avere ""
levels(CortWeat$FENOMENI)[levels(CortWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
CortWeat=subset(CortWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(CortWeat)[which(names(CortWeat)=="FENOMENI")]="fenCort"
colnames(CortWeat)[which(names(CortWeat)=="VISIBILITA.km")]="visibCort"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
CortWeat=CortWeat[-which(CortWeat$DATA=="1/2/2022" | CortWeat$DATA=="2/2/2022" | CortWeat$DATA=="3/2/2022" | CortWeat$DATA=="4/2/2022" | CortWeat$DATA=="5/2/2022" | CortWeat$DATA=="6/2/2022" | CortWeat$DATA=="7/2/2022" | CortWeat$DATA=="8/2/2022" |CortWeat$DATA=="9/2/2022" | CortWeat$DATA=="10/2/2022" | CortWeat$DATA=="11/2/2022" | CortWeat$DATA=="12/2/2022" | CortWeat$DATA=="13/2/2022" | CortWeat$DATA=="14/2/2022"),]



#### Montevarchi ####
febMontev=read.csv("Montevarchi-2022-Febbraio.csv", sep=";")
marMontev=read.csv("Montevarchi-2022-Marzo.csv", sep=";")
aprMontev=read.csv("Montevarchi-2022-Aprile.csv", sep=";")
magMontev=read.csv("Montevarchi-2022-Maggio.csv", sep=";")
giuMontev=read.csv("Montevarchi-2022-Giugno.csv", sep=";")
lugMontev=read.csv("Montevarchi-2022-Luglio.csv", sep=";")
agoMontev=read.csv("Montevarchi-2022-Agosto.csv", sep=";")
setMontev=read.csv("Montevarchi-2022-Settembre.csv", sep=";")
ottMontev=read.csv("Montevarchi-2022-Ottobre.csv", sep=";")
novMontev=read.csv("Montevarchi-2022-Novembre.csv", sep=";")
dicMontev=read.csv("Montevarchi-2022-Dicembre.csv", sep=";")
dicMontev=dicMontev[-which(dicMontev$DATA=="5/12/2022"),]
MontevWeat=rbind(febMontev, marMontev, aprMontev, magMontev, giuMontev, lugMontev, agoMontev, setMontev, ottMontev, novMontev, dicMontev)


#definisco il fenomeno "normale" invece di avere ""
levels(MontevWeat$FENOMENI)[levels(MontevWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
MontevWeat=subset(MontevWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(MontevWeat)[which(names(MontevWeat)=="FENOMENI")]="fenMontev"
colnames(MontevWeat)[which(names(MontevWeat)=="VISIBILITA.km")]="visibMontev"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
MontevWeat=MontevWeat[-which(MontevWeat$DATA=="1/2/2022" | MontevWeat$DATA=="2/2/2022" | MontevWeat$DATA=="3/2/2022" | MontevWeat$DATA=="4/2/2022" | MontevWeat$DATA=="5/2/2022" | MontevWeat$DATA=="6/2/2022" | MontevWeat$DATA=="7/2/2022" | MontevWeat$DATA=="8/2/2022" |MontevWeat$DATA=="9/2/2022" | MontevWeat$DATA=="10/2/2022" | MontevWeat$DATA=="11/2/2022" | MontevWeat$DATA=="12/2/2022" | MontevWeat$DATA=="13/2/2022" | MontevWeat$DATA=="14/2/2022"),]





#### San Giovanni Valdarno ####
febSGioVal=read.csv("San Giovanni Valdarno-2022-Febbraio.csv", sep=";")
marSGioVal=read.csv("San Giovanni Valdarno-2022-Marzo.csv", sep=";")
aprSGioVal=read.csv("San Giovanni Valdarno-2022-Aprile.csv", sep=";")
magSGioVal=read.csv("San Giovanni Valdarno-2022-Maggio.csv", sep=";")
giuSGioVal=read.csv("San Giovanni Valdarno-2022-Giugno.csv", sep=";")
lugSGioVal=read.csv("San Giovanni Valdarno-2022-Luglio.csv", sep=";")
agoSGioVal=read.csv("San Giovanni Valdarno-2022-Agosto.csv", sep=";")
setSGioVal=read.csv("San Giovanni Valdarno-2022-Settembre.csv", sep=";")
ottSGioVal=read.csv("San Giovanni Valdarno-2022-Ottobre.csv", sep=";")
novSGioVal=read.csv("San Giovanni Valdarno-2022-Novembre.csv", sep=";")
dicSGioVal=read.csv("San Giovanni Valdarno-2022-Dicembre.csv", sep=";")
dicSGioVal=dicSGioVal[-which(dicSGioVal$DATA=="5/12/2022"),]
SGioValWeat=rbind(febSGioVal, marSGioVal, aprSGioVal, magSGioVal, giuSGioVal, lugSGioVal, agoSGioVal, setSGioVal, ottSGioVal, novSGioVal, dicSGioVal)



#definisco il fenomeno "normale" invece di avere ""
levels(SGioValWeat$FENOMENI)[levels(SGioValWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
SGioValWeat=subset(SGioValWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(SGioValWeat)[which(names(SGioValWeat)=="FENOMENI")]="fenSGioVal"
colnames(SGioValWeat)[which(names(SGioValWeat)=="VISIBILITA.km")]="visibSGioVal"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
SGioValWeat=SGioValWeat[-which(SGioValWeat$DATA=="1/2/2022" | SGioValWeat$DATA=="2/2/2022" | SGioValWeat$DATA=="3/2/2022" | SGioValWeat$DATA=="4/2/2022" | SGioValWeat$DATA=="5/2/2022" | SGioValWeat$DATA=="6/2/2022" | SGioValWeat$DATA=="7/2/2022" | SGioValWeat$DATA=="8/2/2022" |SGioValWeat$DATA=="9/2/2022" | SGioValWeat$DATA=="10/2/2022" | SGioValWeat$DATA=="11/2/2022" | SGioValWeat$DATA=="12/2/2022" | SGioValWeat$DATA=="13/2/2022" | SGioValWeat$DATA=="14/2/2022"),]


#### Figline Valdarno ####
febFiglVal=read.csv("Figline Valdarno-2022-Febbraio.csv", sep=";")
marFiglVal=read.csv("Figline Valdarno-2022-Marzo.csv", sep=";")
aprFiglVal=read.csv("Figline Valdarno-2022-Aprile.csv", sep=";")
magFiglVal=read.csv("Figline Valdarno-2022-Maggio.csv", sep=";")
giuFiglVal=read.csv("Figline Valdarno-2022-Giugno.csv", sep=";")
lugFiglVal=read.csv("Figline Valdarno-2022-Luglio.csv", sep=";")
agoFiglVal=read.csv("Figline Valdarno-2022-Agosto.csv", sep=";")
setFiglVal=read.csv("Figline Valdarno-2022-Settembre.csv", sep=";")
ottFiglVal=read.csv("Figline Valdarno-2022-Ottobre.csv", sep=";")
novFiglVal=read.csv("Figline Valdarno-2022-Novembre.csv", sep=";")
dicFiglVal=read.csv("Figline Valdarno-2022-Dicembre.csv", sep=";")
dicFiglVal=dicFiglVal[-which(dicFiglVal$DATA=="5/12/2022"),]
FiglValWeat=rbind(febFiglVal, marFiglVal, aprFiglVal, magFiglVal, giuFiglVal, lugFiglVal, agoFiglVal, setFiglVal, ottFiglVal, novFiglVal, dicFiglVal)


#definisco il fenomeno "normale" invece di avere ""
levels(FiglValWeat$FENOMENI)[levels(FiglValWeat$FENOMENI)==""]="normale"
#tolgo le variabili che non voglio usare
FiglValWeat=subset(FiglValWeat, select=c(LOCALITA, DATA, FENOMENI, VISIBILITA.km))
#rinomino le colonne, prendo fenomeni e visibilità per aggiungerli al dataset treniAll
colnames(FiglValWeat)[which(names(FiglValWeat)=="FENOMENI")]="fenFiglVal"
colnames(FiglValWeat)[which(names(FiglValWeat)=="VISIBILITA.km")]="visibFiglVal"

#### counting frequency of each days ####
#removing the first 14 days of february because data starts from 15/02
FiglValWeat=FiglValWeat[-which(FiglValWeat$DATA=="1/2/2022" | FiglValWeat$DATA=="2/2/2022" | FiglValWeat$DATA=="3/2/2022" | FiglValWeat$DATA=="4/2/2022" | FiglValWeat$DATA=="5/2/2022" | FiglValWeat$DATA=="6/2/2022" | FiglValWeat$DATA=="7/2/2022" | FiglValWeat$DATA=="8/2/2022" |FiglValWeat$DATA=="9/2/2022" | FiglValWeat$DATA=="10/2/2022" | FiglValWeat$DATA=="11/2/2022" | FiglValWeat$DATA=="12/2/2022" | FiglValWeat$DATA=="13/2/2022" | FiglValWeat$DATA=="14/2/2022"),]


#### joining all the stations ####
weatherAll=cbind(assisiWeat, corcianoWeat, CortWeat, FiglValWeat, fireWeat, FolignoWeat, magWeat, MontevWeat, PassWeat, peruWeat, SGioValWeat, spelloWeat)
View(weatherAll)
#check which days are missing and deleting it
NROW(freq)
NROW(weatherAll)
date_range <- seq(min(treniAll$Data), max(treniAll$Data), by = 1)
date_range[!date_range %in% treniAll$Data] 
weatherAll=weatherAll[-which(weatherAll$DATA=="6/3/2022"), ]
weatherAll=weatherAll[-which(weatherAll$DATA=="13/4/2022"), ]
NROW(freq)
NROW(weatherAll)
#elimino tutte le colonne LOCALITA e DATA
weatherAll=subset(weatherAll, select=-LOCALITA) %>%
            select_at(vars(-starts_with("LOCALITA")))
weatherAll=subset(weatherAll, select=-DATA) %>%
  select_at(vars(-starts_with("DATA")))

#expand days
freq=as.data.frame(table(treniAll$Data))$Freq
weatherAll["freq"]=freq
weatherAll=expandRows(weatherAll, "freq")
NROW(weatherAll)
NROW(treniAll)

#### ORA POSSO USARE weatherAll UNENDOLO A treniALL ####
#creo un file csv da usare nell'altro script
write.csv(weatherAll, "C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/CONTEST Train/weatherAll.csv")
