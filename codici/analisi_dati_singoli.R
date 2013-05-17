#cartella e libreria zoo per dataframe con serie storiche
setwd("C:\\R\\r-output")
library(zoo)


#definizione periodo in cui si eseguono le ottimizzazioni,il nr di rendimenti utilizzati per la media storica
inizio<-'2001 Q3'
fine<-'2012 Q4'




##mediastorica
#caricamento file

load('ottimizzazionedatistoriciVARtarget.RData')

#arrotondamento riordino e annualizzazione(dimenticato di fare durante l'ottimizzazione)
mediastorica<-(1+mediastorica)^4-1

mediastorica<-100*round(mediastorica, digits=4)
mediastorica<-mediastorica[,-14]
mediastorica<-mediastorica[,-14]
mediastorica<-mediastorica[,-14]
mstor<-cbind(mediastorica[,1:8],mediastorica[,14],mediastorica[,9:13])

v.nomiseriestorichepergrafici<-read.csv('nomiseriestorichepergrafici.csv',heade=F)
colnames(mstor)<-t(v.nomiseriestorichepergrafici)
#Istogrammi
par(mfrow=c(2,2))

for (i in 1:4) {
	hist(mstor[,i],col='grey',main=colnames(mstor)[i],xlab='Media Storica')
}

##ytm
#caricamento file
load('ottimizzazioneytmVARtarget.RData')


#arrotondamento riordino ytmtrimestrali e aggiustamento periodo
ytmtrimestrali<-ytmtrimestrali[,-15]
ytmtrimestrali<-ytmtrimestrali[,-15]
ytmtrimestrali<-ytmtrimestrali[,-15]
ytm<-100*round(ytmtrimestrali, digits=4)
index(ytm)<-index(ytm)+0.25
names(ytm)<-names(mstor)

##rendimenti realizzati
#caricamento file
load('risultatiytmVARtarget.RData')

#arrotondamento e ottenimento dei rendimenti trimestrali del periodo utilizzato
rreal<-100*round(rendimentirealizzati,digits=4)

rreal<-window(rreal, start = start(mstor), end=end(mstor))
names(rreal)<-names(mstor)

#Istogrammi
par(mfrow=c(2,2))

for (i in 9:12) {
	hist(rreal[,i],col='grey',main=colnames(rreal)[i],xlab='Rendimenti realizzati',breaks=10,freq=FALSE)
}

#correlazione
correlazione<-52*round(cor(rendimentisettimanali),digits=4)
colnames(correlazione)<-names(mstor)
rownames(correlazione)<-names(mstor)
write.xls(correlazione.xls)



