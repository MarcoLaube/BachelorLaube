


#cartella e libreria zoo per dataframe con serie storiche
setwd("C:\\R\\r-output")
library(zoo)
#load('risultatiytm.RData')
#load('risultatistorici.RData')

confronto<-list()
sharpe<-merge(risultatiytm$sharpe[,2],risultatistorici$sharpe[,2])
names(sharpe)<-c('ytm','media storica')
rendimento<-merge(risultatiytm$rporto[,2],risultatistorici$rporto[,2])
names(rendimento)<-c('ytm','media storica')
stdev<-merge(risultatiytm$stdev[,2],risultatistorici$stdev[,2])
names(stdev)<-c('ytm','media storica')

confronto$sharpe<-sharpe
confronto$rendimento<-rendimento
confronto$stdev<-stdev

#plot
plot(risultatistorici$msquare,col="blue", xlabel='trimestri',ylabel='Rendimenti standardizzati')


lines(risultatiytm$msquare,col="red")


#vola

#dataframe stdev effettiva


dummyvola<-matrix(,ncol=1,nrow=nrperiodi)
dummyvola<-zooreg(dummyvola,start=as.yearqtr(inizio,format ="%Y Q%q"),end=as.yearqtr(fine,format ="%Y Q%q"),frequency=4)
names(dummyvola)<-'dummyVola'

rendimentigeometrici<-function(x) {diff(log(x))}
qtrtoyear<-function(x) (1+x)^4-1
datigiornalierivola<-read.zoo("vola.txt", format ="%d.%m.%Y") 
datitrimestralivola<-aggregate(datigiornalierivola,as.yearqtr,tail,1)
rvolatrimestrali<-rendimentigeometrici(datitrimestralivola)
rvolaannualizzati<-qtrtoyear(rvolatrimestrali)

for (i in index(rvolaannualizzati)){
	dummyvola[as.yearqtr(i,format="%Y Q%q"),]<-as.numeric(rvolaannualizzati[as.yearqtr(i,format="%Y Q%q"),]>0.02)
}

for (i in index(dummyvola)) { if (is.na(dummyvola[as.yearqtr(i,format="%Y Q%q"),])) dummyvola[as.yearqtr(i,format="%Y Q%q"),]<-1
	
	
	
}



summary(lm(risultatiytm$msquare~risultatistorici$msquare+dummyvola*risultatistorici$msquare+dummyvola))
