#cartella e libreria 
setwd("C:\\R\\r-output")
library(zoo)
library(distr)
library(fBasics)
library(Hmisc) #per Ecdf
library(urca) #per dickker
library(stats) #per shapiro
#definizione periodo in cui si eseguono le ottimizzazioni,il nr di rendimenti utilizzati per la media storica
inizio<-'2001 Q3'
fine<-'2012 Q4'


##funzioni
media<-function(n){
	inizio<-'2001 Q3'
	fine<-'2012 Q4'
	nrperiodi<-length(seq(as.yearqtr(inizio,format ="%Y Q%q"),as.yearqtr(fine,format ="%Y Q%q"),by=0.25))
	#preparazione dataframe media storica
	mediastorica<-matrix(,ncol=17,nrow=nrperiodi)
	mediastorica<-zooreg(mediastorica,start=as.yearqtr(inizio,format ="%Y Q%q"),end=as.yearqtr(fine,format ="%Y Q%q"),frequency=4)
	for (i in index(mediastorica)){
		#preparazione rendimenti (trimestrali e annualizzati)
		rendimentiaritmetici<-function(x) {((lag(x)/x))-1}
		qtrtoyear<-function(x) (1+x)^4-1
		datigiornalieri<-read.zoo("dati.txt", format ="%d.%m.%Y") 
		
		datitrimestrali<-aggregate(datigiornalieri,as.yearqtr,tail,1)
		rendimentitrimestrali<-rendimentiaritmetici(datitrimestrali)
		
		primoytrmedia<-as.yearqtr(i,format ="%Y Q%q" )-n/4-0.25 #definisce trimestre del primo elemento per la media storica
		campionerendimenti<-window(rendimentitrimestrali,start=as.yearqtr(primoytrmedia,format ="%Y Q%q"),end=as.yearqtr(i,format ="%Y Q%q")-0.25) #contiene solo i rendimenti per la media storica
		mediastorica[as.yearqtr(i,format="%Y Q%q"),]<-apply(campionerendimenti,2,FUN=mean)
	}
#arrotondamento riordino e annualizzazione(dimenticato di fare durante l'ottimizzazione)
	mediastorica<-(1+mediastorica)^4-1
	
	mediastorica<-100*round(mediastorica, digits=4)
	mediastorica<-mediastorica[,-14]
	mediastorica<-mediastorica[,-14]
	mediastorica<-mediastorica[,-14]
	mstor<-cbind(mediastorica[,1:8],mediastorica[,14],mediastorica[,9:13])
	
	v.nomiseriestorichepergrafici<-read.csv('nomiseriestorichepergrafici.csv',header=F)
	colnames(mstor)<-t(v.nomiseriestorichepergrafici)
	return(mstor)
}
#Suddivisione dei dati in asset class, x la matrice dei dati

shortterm<-function(x){
	st<-x[,1:3]
	return(st)
}
bondgov<-function(x){
	st<-x[,4:5]
	return(st)
}
bondcorp<-function(x){
	st<-x[,6:7]
	return(st)
}
HY<-function(x){
	st<-x[,8:10]
	return(st)
}
embi<-function(x){
	st<-x[,10]
	return(st)
}
conv<-function(x){
	st<-x[,11:12]
	return(st)
}
azioni<-function(x){
	st<-x[,13:14]
	return(st)
}
bond<-function(x){
	st<-x[,4:10]
	return(st)
}
govandcorp<-function(x){
	st<-x[,4:7]
	return(st)
}

#creazione istogrammi, x la matrice dei dati, s il tipo di rendimenti (storici, a scadenza,realizzati)
isto<-function(x,s){
	x<-as.matrix(x)
	z<-ceiling(ncol(x)/2)
	

	k<-matrix(,nrow=1,ncol=ncol(x))
	for(i in 1:ncol(x)){
		
		k[,i]<-max(density(x[,i])$y, na.rm=TRUE)
	}
	
	f<-as.numeric(max(k,na.rm=TRUE))+0.05
	par(mfrow=c(z,2))
	for (i in 1:ncol(x)) {
		hist(x[,i],col='grey',main=colnames(x)[i],xlab=s,ylab='Probabilita', breaks=10, freq=FALSE,xlim=c(-50,50),ylim=c(0,1))
	}
	lines(density(x[,i]), col='red')
		#lines(Ecdf(x[,i])$y,col='darkblue',type='l')
	}
	
	


#plot rendimento in funzione del rischio (la prima aggiusta il grafico rispetto ai dati)

rvsvar<-function(rreal){
	par(mfrow=c(1,1))
	plot(stats(shortterm(rreal))[,2],stats(shortterm(rreal))[,1],xlim=c(min(stats(rreal)[,2]),max(stats(rreal)[,2])),ylim=c(min(stats(rreal)[,1]),max(stats(rreal)[,1])), xlab=c('Deviazione Standard(%)'),ylab=c('Rendimento medio(%)'),main=c('Rendimento in relazione al rischio'),col='black',bg='black',pch=21)
	points(stats(bondgov(rreal))[,2],stats(bondgov(rreal))[,1],col='darkred',bg='darkred',pch=21)
	points(stats(bondcorp(rreal))[,2],stats(bondcorp(rreal))[,1],col='purple',bg='purple',pch=21)
	points(stats(HY(rreal))[,2],stats(HY(rreal))[,1],col='red',bg='red',pch=21)
	points(stats(conv(rreal))[,2],stats(conv(rreal))[,1],col='orange',bg='orange',pch=21)
	points(stats(azioni(rreal))[,2],stats(azioni(rreal))[,1],col='yellow',bg='yellow',pch=21)
	reg<-lm(stats(rreal)[,1]~stats(rreal)[,2])
	abline(a=as.numeric(reg$coefficients[1]),b=as.numeric(reg$coefficients[2]), col='blue')
	legend("bottomright",c('Short Term','Obbligazioni Governative','Obbligazioni Corporate','High Yield e Mercati Emergenti','Obbligazioni Convertibili','Azioni'),pch=19,col=c('black','darkred','purple','red','orange','yellow'),cex=1)
}
rvsvar2<-function(rreal){
	par(mfrow=c(1,1))
	plot(stats(shortterm(rreal))[,2],stats(shortterm(rreal))[,1],xlim=c(0,45),ylim=c(0,20), xlab=c('Deviazione Standard(%)'),ylab=c('Rendimento medio(%)'),main=c('Rendimento in relazione al rischio'),col='black',bg='black',pch=21)
	points(stats(bondgov(rreal))[,2],stats(bondgov(rreal))[,1],col='darkred',bg='darkred',pch=21)
	points(stats(bondcorp(rreal))[,2],stats(bondcorp(rreal))[,1],col='purple',bg='purple',pch=21)
	points(stats(HY(rreal))[,2],stats(HY(rreal))[,1],col='red',bg='red',pch=21)
	points(stats(conv(rreal))[,2],stats(conv(rreal))[,1],col='orange',bg='orange',pch=21)
	points(stats(azioni(rreal))[,2],stats(azioni(rreal))[,1],col='yellow',bg='yellow',pch=21)
	reg<-lm(stats(rreal)[,1]~stats(rreal)[,2])
	abline(a=as.numeric(reg$coefficients[1]),b=as.numeric(reg$coefficients[2]), col='blue')
	legend("bottomright",c('Short Term','Obbligazioni Governative','Obbligazioni Corporate','High Yield e Mercati Emergenti','Obbligazioni Convertibili','Azioni'),pch=19,col=c('black','darkred','purple','red','orange','yellow'),cex=0.7)
}


#plot densità
dens<-function(rreal){
	par(mfrow=c(1,1))
	plot(density(shortterm(rreal)[,1]),xlim=c(min((rreal)),max((rreal))),ylim=c(0,0.5), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni empiriche'),col='black',bg='black',pch=21)
	lines(density(shortterm(rreal)[,2]),col='black',bg='black',pch=21)
	lines(density(shortterm(rreal)[,3]),col='black',bg='black',pch=21)
	lines(density(bondgov(rreal)[,1]),col='darkred',bg='darkred',pch=21)
	lines(density(bondgov(rreal)[,2]),col='darkred',bg='darkred',pch=21)
	lines(density(bondcorp(rreal)[,1]),col='purple',bg='purple',pch=21)
	lines(density(bondcorp(rreal)[,2]),col='purple',bg='purple',pch=21)
	
	lines(density(HY(rreal)[,1]),col='red',bg='red',pch=21)
	lines(density(HY(rreal)[,2]),col='red',bg='red',pch=21)
	lines(density(HY(rreal)[,3]),col='red',bg='red',pch=21)
	
	lines(density(conv(rreal)[,1]),col='orange',bg='orange',pch=21)
	lines(density(conv(rreal)[,2]),col='orange',bg='orange',pch=21)
	lines(density(azioni(rreal)[,1]),col='yellow',bg='yellow',pch=21)
	lines(density(azioni(rreal)[,2]),col='yellow',bg='yellow',pch=21)
	
	
	legend("bottomright",c('Short Term','Obbligazioni Governative','Obbligazioni Corporate','High Yield e Mercati Emergenti','Obbligazioni Convertibili','Azioni'),pch=19,col=c('black','darkred','purple','red','orange','yellow'),cex=0.7)
}
confrplot1<-function(rreal,mstor){
	par(mfrow=c(4,2))
	
	for(i in 1:8){
		plot(rreal[,i],ylab=names(rreal)[,i],xlab='Periodo')
		lines(mstor[,i],col='red')
		abline(a=mean(rreal[,i]),b=0,col='green')
		
	}
	
}
confrplot2<-function(rreal,mstor){
	par(mfrow=c(3,2))
	
	for(i in 9:14){
		plot(rreal[,i],ylab=names(rreal)[,i],xlab='Periodo')
		lines(mstor[,i],col='red')
		abline(a=mean(rreal[,i]),b=0,col='green')
		
	}
	
	legend("bottomright",c('Rendimenti realizzati','Media Storica','Media rendimenti realizzati'),pch=19,col=c('blue','red','green'),cex=0.7)
}
confrplot3<-function(rreal,mstor){
	par(mfrow=c(4,2))
	
	for(i in 1:8){
		plot(rreal[,i],ylab=names(rreal)[,i],xlab='Periodo')
		lines(mstor[,i],col='red')
	
		
	}
	
}
confrplot4<-function(rreal,mstor){
	par(mfrow=c(3,2))
	
	for(i in 9:14){
		plot(rreal[,i],ylab=names(rreal)[,i],xlab='Periodo')
		lines(mstor[,i],col='red')
		
		
	}
	
	legend("bottomright",c('Rendimenti realizzati','Rendimenti a scadenza'),pch=19,col=c('blue','red'),cex=0.7)
}
confrplot5<-function(rreal,mstor){
	par(mfrow=c(3,2))
	
	for(i in 4:8){
		plot(mstor[,i],ylab=names(rreal)[,i],xlab='Periodo',ylim=c(0,max(mstor[,i])+5))
		
		abline(a=mean(rreal[,i]),b=0,col='red')
		
	}
	legend("bottomright",c('Rendimenti a scadenza','Media rendimenti realizzati'),pch=19,col=c('blue','red'),cex=0.7)
	
}
confrplot6<-function(rreal,mstor){
	par(mfrow=c(3,2))
	
	for(i in 9:14){
		plot(mstor[,i],ylab=names(rreal)[,i],xlab='Periodo',ylim=c(0,max(mstor[,i])+5))
		
		abline(a=mean(rreal[,i]),b=0,col='red')
		
	}
	
	legend("bottomright",c('Rendimenti a scadenza','Media rendimenti realizzati'),pch=19,col=c('blue','red'),cex=0.7)
}
cdplot1<-function(rreal,mstor){
	par(mfrow=c(4,2))
	
	for(i in 1:8){
		cdplot(mstor[,i],factor(rreal[,i]),xlab='Rendimenti a scadenza',main=names(rreal)[,i],ylab='Media Storica')
		
		
		
	}
}
cdplot2<-function(rreal,mstor){
	par(mfrow=c(3,2))
	
	for(i in 9:14){
		cdplot(mstor[,i],factor(rreal[,i]),xlab='Rendimenti a scadenza',main=names(rreal)[,i],ylab='Media Storica')
		
		
		
	}
}
denssep<-function(rreal){
	
		par(mfrow=c(2,2))
		plot(density(shortterm(rreal)[,1]),xlim=c(-20,20),ylim=c(0,0.5), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Short term'),col='red',bg='red',pch=21)
		lines(density(shortterm(rreal)[,2]),col='blue',bg='blue',pch=21)
		lines(density(shortterm(rreal)[,3]),col='darkgreen',bg='darkgreen',pch=21)
		legend("topleft",as.vector(names(shortterm(rreal))),pch=19,col=c('red','blue','darkgreen'),cex=1)
		
		plot(density(bondgov(rreal)[,1]),xlim=c(-20,20),ylim=c(0,0.1), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Obbligazioni'),col='black',bg='black',pch=21)
		lines(density(bondgov(rreal)[,2]),col='red',bg='red',pch=21)
		lines(density(bondcorp(rreal)[,1]),col='blue',bg='blue',pch=21)
		lines(density(bondcorp(rreal)[,2]),col='darkgreen',bg='darkgreen',pch=21)
		legend("topleft",c(as.vector(names(bondgov(rreal))),as.vector(names(bondcorp(rreal)))),pch=19,col=c('black','red','blue','darkgreen'),cex=1)
		
		plot(density(HY(rreal)[,1]),xlim=c(-50,50),ylim=c(0,0.05), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni High Yield e Mercati emergenti'),col='red',bg='red',pch=21)
		lines(density(HY(rreal)[,2]),col='blue',bg='blue',pch=21)
		lines(density(HY(rreal)[,3]),col='darkgreen',bg='darkgreen',pch=21)
		legend("topleft",as.vector(names(HY(rreal))),pch=19,col=c('red','blue','darkgreen'),cex=1)
		
		plot(density(conv(rreal)[,1]),xlim=c(-50,50),ylim=c(0,0.05), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Convertibili e Azioni'),col='black',bg='black',pch=21)
		lines(density(conv(rreal)[,2]),col='red',bg='red',pch=21)
		lines(density(azioni(rreal)[,1]),col='blue',bg='blue',pch=21)
		lines(density(azioni(rreal)[,2]),col='darkgreen',bg='darkgreen',pch=21)
		legend("topleft",c(as.vector(names(conv(rreal))),as.vector(names(azioni(rreal)))),pch=19,col=c('black','red','blue','darkgreen'),cex=1)
		
	
}
denssep2<-function(rreal){
	
	par(mfrow=c(2,2))
	plot(density(shortterm(rreal)[,1]),xlim=c(-20,20),ylim=c(0,0.4), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Short term'),col='red',bg='red',pch=21)
	lines(density(shortterm(rreal)[,2]),col='blue',bg='blue',pch=21)
	lines(density(shortterm(rreal)[,3]),col='darkgreen',bg='darkgreen',pch=21)
	lines(density(rnorm(1000,mean=0,sd=10)),type='l',col='black',bg='black',pch=21)
	legend("topleft",c(as.vector(names(shortterm(rreal))),'N(0,10)'),pch=19,col=c('red','blue','darkgreen','black'),cex=1)
	
	plot(density(bondgov(rreal)[,1]),xlim=c(-20,20),ylim=c(0,0.20), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Obbligazioni'),col='orange',bg='orange',pch=21)
	lines(density(bondgov(rreal)[,2]),col='red',bg='red',pch=21)
	lines(density(bondcorp(rreal)[,1]),col='blue',bg='blue',pch=21)
	lines(density(bondcorp(rreal)[,2]),col='darkgreen',bg='darkgreen',pch=21)
	lines(density(rnorm(1000,mean=0,sd=10)),type='l',col='black',bg='black',pch=21)
	legend("topleft",c(as.vector(names(bondgov(rreal))),as.vector(names(bondcorp(rreal))),'N(0,10)'),pch=19,col=c('orange','red','blue','darkgreen','black'),cex=1)
	
	plot(density(HY(rreal)[,1]),xlim=c(-50,50),ylim=c(0,0.10), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni High Yield e Mercati emergenti'),col='red',bg='red',pch=21)
	lines(density(HY(rreal)[,2]),col='blue',bg='blue',pch=21)
	lines(density(HY(rreal)[,3]),col='darkgreen',bg='darkgreen',pch=21)

	legend("topleft",c(as.vector(names(HY(rreal)))),pch=19,col=c('red','blue','darkgreen','black'),cex=1)
	
	plot(density(conv(rreal)[,1]),xlim=c(-50,50),ylim=c(0,0.10), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Convertibili e Azioni'),col='black',bg='black',pch=21)
	lines(density(conv(rreal)[,2]),col='red',bg='red',pch=21)
	lines(density(azioni(rreal)[,1]),col='blue',bg='blue',pch=21)
	lines(density(azioni(rreal)[,2]),col='darkgreen',bg='darkgreen',pch=21)
	
	legend("topleft",c(as.vector(names(conv(rreal))),as.vector(names(azioni(rreal)))),pch=19,col=c('black','red','blue','darkgreen','black'),cex=1)
	
	
}
denssep3<-function(rreal){
	
	par(mfrow=c(2,2))
	plot(density(shortterm(rreal)[,1]),xlim=c(-2,10),ylim=c(0,0.6), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Short term'),col='red',bg='red',pch=21)
	lines(density(shortterm(rreal)[,2]),col='blue',bg='blue',pch=21)
	lines(density(shortterm(rreal)[,3]),col='darkgreen',bg='darkgreen',pch=21)
	lines(density(rnorm(1000000,mean=5,sd=1)),type='l',col='black',bg='black',pch=21)
	legend("topright",c(as.vector(names(shortterm(rreal))),'N(5,1)'),pch=19,col=c('red','blue','darkgreen','black'),cex=1)
	
	plot(density(bondgov(rreal)[,1]),xlim=c(0,10),ylim=c(0,0.6), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Obbligazioni'),col='orange',bg='orange',pch=21)
	lines(density(bondgov(rreal)[,2]),col='red',bg='red',pch=21)
	lines(density(bondcorp(rreal)[,1]),col='blue',bg='blue',pch=21)
	lines(density(bondcorp(rreal)[,2]),col='darkgreen',bg='darkgreen',pch=21)
	lines(density(rnorm(1000000,mean=1,sd=1)),type='l',col='black',bg='black',pch=21)
	legend("topright",c(as.vector(names(bondgov(rreal))),as.vector(names(bondcorp(rreal))),'N(1,1)'),pch=19,col=c('orange','red','blue','darkgreen','black'),cex=1)
	
	plot(density(HY(rreal)[,1]),xlim=c(0,20),ylim=c(0,0.5), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni High Yield e Mercati emergenti'),col='red',bg='red',pch=21)
	lines(density(HY(rreal)[,2]),col='blue',bg='blue',pch=21)
	lines(density(HY(rreal)[,3]),col='darkgreen',bg='darkgreen',pch=21)
	
	legend("topright",c(as.vector(names(HY(rreal)))),pch=19,col=c('red','blue','darkgreen','black'),cex=1)
	
	plot(density(conv(rreal)[,1]),xlim=c(0,20),ylim=c(0,0.5), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Convertibili e Azioni'),col='black',bg='black',pch=21)
	lines(density(conv(rreal)[,2]),col='red',bg='red',pch=21)
	lines(density(azioni(rreal)[,1]),col='blue',bg='blue',pch=21)
	lines(density(azioni(rreal)[,2]),col='darkgreen',bg='darkgreen',pch=21)
	
	legend("topright",c(as.vector(names(conv(rreal))),as.vector(names(azioni(rreal)))),pch=19,col=c('black','red','blue','darkgreen','black'),cex=1)
	
	
}

ecdfsep<-function(rreal){
	
	par(mfrow=c(2,2))
	plot(Ecdf(shortterm(rreal)[,1],pl=FALSE),type='l',xlim=c(-2,7),ylim=c(0,1), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Short term'),col='red',bg='red',pch=21)
	lines(Ecdf(shortterm(rreal)[,2],pl=FALSE),type='l',col='blue',bg='blue',pch=21)
	lines(Ecdf(shortterm(rreal)[,3],pl=FALSE),type='l',col='darkgreen',bg='darkgreen',pch=21)
	lines(Ecdf(rnorm(1000,mean=0,sd=1),pl=FALSE),type='l',col='black',bg='black',pch=21)
	legend("topleft",c(as.vector(names(shortterm(rreal))),'N(0,1)'),pch=19,col=c('red','blue','darkgreen','black'),cex=1)
	
	plot(Ecdf(bondgov(rreal)[,1],pl=FALSE),type='l',xlim=c(-5,15),ylim=c(0,1), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Obbligazioni'),col='grey',bg='grey',pch=21)
	lines(Ecdf(bondgov(rreal)[,2],pl=FALSE),type='l',col='red',bg='red',pch=21)
	lines(Ecdf(bondcorp(rreal)[,1],pl=FALSE),type='l',col='blue',bg='blue',pch=21)
	lines(Ecdf(bondcorp(rreal)[,2],pl=FALSE),type='l',col='darkgreen',bg='darkgreen',pch=21)
	lines(Ecdf(rnorm(1000,mean=0,sd=1),pl=FALSE),type='l',col='black',bg='black',pch=21)
	legend("topleft",c(as.vector(names(bondgov(rreal))),as.vector(names(bondcorp(rreal))),'N(0,1)'),pch=19,col=c('grey','red','blue','darkgreen','black'),cex=1)
	
	plot(Ecdf(HY(rreal)[,1],pl=FALSE),type='l',xlim=c(-25,25),ylim=c(0,1), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni High Yield e Mercati emergenti'),col='red',bg='red',pch=21)
	lines(Ecdf(HY(rreal)[,2],pl=FALSE),type='l',col='blue',bg='blue',pch=21)
	lines(Ecdf(HY(rreal)[,3],pl=FALSE),type='l',col='darkgreen',bg='darkgreen',pch=21)
	lines(Ecdf(rnorm(1000,mean=0,sd=1),pl=FALSE),type='l',col='black',bg='black',pch=21)
	legend("topleft",c(as.vector(names(HY(rreal))),'N(0,1)'),pch=19,col=c('red','blue','darkgreen','black'),cex=1)
	
	plot(Ecdf(conv(rreal)[,1],pl=FALSE),type='l',xlim=c(-25,25),ylim=c(0,1), xlab=c('Rendimento(%)'),ylab=c('Probabilita'),main=c('Distribuzioni Convertibili e Azioni'),col='grey',bg='grey',pch=21)
	lines(Ecdf(conv(rreal)[,2],pl=FALSE),type='l',col='red',bg='red',pch=21)
	lines(Ecdf(azioni(rreal)[,1],pl=FALSE),type='l',col='blue',bg='blue',pch=21)
	lines(Ecdf(azioni(rreal)[,2],pl=FALSE),type='l',col='darkgreen',bg='darkgreen',pch=21)
	lines(Ecdf(rnorm(1000,mean=0,sd=1),pl=FALSE),type='l',col='black',bg='black',pch=21)
	legend("topleft",c(as.vector(names(conv(rreal))),as.vector(names(azioni(rreal))),'N(0,1)'),pch=19,col=c('grey','red','blue','darkgreen','black'),cex=1)
	
	
}

#funzione calcolo parametri distribuzione
stats<-function(x){
	s<-as.matrix(t(x))
	
	s<-s[,1:8]
	colnames(s)<-c('Media','Deviazione Standard','Asimmetria','Curtosi','Statistica Dick-Fuller','Valori Critici Dick-Fuller (5%)','Valori critici Dick-Fuller (1%)','P-value Shapiro Test')
	s[,1]<-apply(x,2,FUN=mean)
	s[,2]<-apply(x,2,FUN=sd)
	s[,3]<-apply(x,2,FUN=skewness)
	s[,4]<-apply(x,2,FUN=kurtosis,method=c("moment"))

	return(s)
}
stats2<-function(x){
	s<-as.matrix(t(x))
	
	s<-s[,1:8]
	colnames(s)<-c('Media','Deviazione Standard','Asimmetria','Curtosi','Statistica Dick-Fuller','Valori Critici Dick-Fuller (1%)','Valori critici Dick-Fuller (5%)','P-value Shapiro Test')
	s[,1]<-apply(x,2,FUN=mean)
	s[,2]<-apply(x,2,FUN=sd)
	s[,3]<-apply(x,2,FUN=skewness)
	s[,4]<-apply(x,2,FUN=kurtosis,method=c("moment"))
	for(i in 1:14){
		df<-ur.df(x[,i],type="drift")
		s[i,5]<-as.numeric(df@teststat[1,1])
		s[i,6]<-as.numeric(df@cval[1,1])
		s[i,7]<-as.numeric(df@cval[1,2])
		
	}
	for (i in 1:14){
		s[i,8]<-shapiro.test(as.vector(x[,i]))$p.value
	}
	return(s)
}

##mediastorica
#calcolo



mstor<-media(10)
#mstor5<-media(5)

# tabella statistica destrittiva
tabstat<-write.table(round(stats(mstor)[,6:7],digits=2),sep="\t",col.names=F)

#ecdf e densità

postscript("ecdfmstor.eps")
ecdfsep(mstor)
dev.off()


postscript("densmstor.eps")
denssep2(mstor)
dev.off()


jpeg("rendimentovsrischiomstor.jpeg")
rvsvar2(mstor)
dev.off()

#andamento mstor

postscript("andamentomstor.eps")
plot(mstor,xlab='Periodo',main='Andamento delle medie storiche')
dev.off()

##ytm
#caricamento file
load('ottimizzazioneytmVARtarget.RData')


#arrotondamento riordino ytmtrimestrali e aggiustamento periodo
ytmtrimestrali<-ytmtrimestrali[,-15]
ytmtrimestrali<-ytmtrimestrali[,-15]
ytmtrimestrali<-ytmtrimestrali[,-15]
ytm<-100*round(ytmtrimestrali, digits=4)

index(ytm)<-index(ytm)+0.25
ytm<-ytm[-48,]
ytms<-ytm[-47,]
names(ytms)<-names(mstor)


#densita

postscript("densytm.eps")
denssep3(ytms)
dev.off()

#andamento
postscript("andamentoytms.eps")
plot(ytms,xlab='Periodo',main='Andamento dei rendimenti a scadenza')
dev.off()


#tabella statistica descrittiva

write.table(round(stats2(ytms),digits=2)[,5],sep="\t",col.names=F,row.names=F)

##rendimenti realizzati
#caricamento file
load('risultatiytmVARtarget.RData')

#arrotondamento e ottenimento dei rendimenti trimestrali del periodo utilizzato
rreal<-100*round(rendimentirealizzati,digits=4)

rreal<-window(rreal, start = start(mstor), end=end(mstor))
names(rreal)<-names(mstor)




#densita
postscript("densitarreal.eps")
denssep(rreal)
dev.off()

#andamento
postscript("andamentorreal.eps")
plot(rreal,main='Andamento dei rendimenti',xlab=c('Periodo'))
dev.off()
#tabella statistica descrittiva

write.table(cbind(round(stats2(mstor),digits=2)[,2],round(stats2(rreal)/46,digits=2)[,2],round(stats2(mstor),digits=4)[,2]-round(stats2(rreal)/46,digits=2)[,2]),sep="\t",col.names=F,row.names=F)

#Rendimento vs Rischio

postscript("RendimentovsRischio.eps")

rvsvar(rreal)

dev.off()


##Confronto
##rendimenti realizzati ytms
#errore 
postscript("confrontoandamentorrealmstor.eps")
confrplot1(rreal,mstor)
dev.off()

postscript("confrontoandamentorrealmstor2.eps")
confrplot2(rreal,mstor)
dev.off()

#grafico esempio
postscript("confrontoandamentorrealmstorUSgov.eps")
par(mfrow=c(1,1))
plot(rreal[,4],ylab=names(rreal)[,4],xlab='Periodo',main='Rendimenti realizzati e media storica per i governativi europei')
lines(mstor[,4],col='red')
abline(a=mean(rreal[,4]),b=0,col='green')
legend("bottomright",c('Rendimenti realizzati','Media Storica','Media rendimenti realizzati'),pch=19,col=c('blue','red','green'),cex=1)

dev.off()

errorms<-rreal-mstor
postscript("densitaerrore.eps")
denssep(errorms)
dev.off()

postscript("errormsvsrreal")
plot(rreal[,14],main='Errore e rendimenti realizzati',xlab='Periodo',ylab=names(rreal)[,14])
lines(errorms[,14],col='red')
legend("bottomright",c('Rendimenti realizzati','Errore'),pch=19,col=c('blue','red'),cex=1)
dev.off()

##rendimenti realizzati e ytms
#errore 
postscript("confrontoandamentorrealytms1.eps")
confrplot3(rreal,ytms)
dev.off()

postscript("confrontoandamentorrealytms2.eps")
confrplot4(rreal,ytms)
dev.off()

postscript("confrontomediaytms1.eps")
confrplot5(rreal,ytms)
dev.off()

postscript("confrontomediaytms2.eps")
confrplot6(rreal,ytms)
dev.off()

#grafico esempio
postscript("confrontoandamentorrealytms.eps")

par(mfrow=c(1,2))

plot(rreal[,4],ylab=names(rreal)[,4],xlab='Periodo')

lines(ytms[,4],col='red')
abline(a=mean(rreal[,4]),b=0,col='green')
legend("bottomleft",c('Rendimenti realizzati','Rendimenti a scadenza','Media rendimenti realizzati'),pch=19,col=c('blue','red','green'),cex=1)

plot(rreal[,3],ylab=names(rreal)[,3],xlab='Periodo')
lines(ytms[,3],col='red')
abline(a=mean(rreal[,3]),b=0,col='green')
legend("bottomleft",c('Rendimenti realizzati','Rendimenti a scadenza','Media rendimenti realizzati'),pch=19,col=c('blue','red','green'),cex=1)

mtext("Rendimenti realizzati e a scadenza per i governativi europei e USCash", outer = TRUE, cex = 1.5)
dev.off()

errorytms<-rreal-ytms
postscript("densitaerroreytm.eps")
denssep(errorytms)
dev.off()

postscript("errorytmsvsrreal.eps")
plot(rreal[,14],main='Errore e rendimenti realizzati',xlab='Periodo',ylab=names(rreal)[,14])
lines(errorytms[,14],col='red')
legend("bottomright",c('Rendimenti realizzati','Errore'),pch=19,col=c('blue','red'),cex=0.7)
dev.off()

##confronto media storica ytms

postscript("mstorytms1.eps")
confrplot3(mstor,ytms)
dev.off()

postscript("mstorvsytms2.eps")
confrplot4(mstor,ytms)
dev.off()

#grafico esempio
postscript("confrontoandamentoytmsmstorEugov.eps")
par(mfrow=c(1,1))
plot(ytms[,4],ylim=c(-5,5),ylab=names(ytms)[,4],xlab='Periodo',main='Rendimenti a scadenza e media storica per i governativi europei')
lines(mstor[,4],col='red')
legend("bottomright",c('Rendimenti a scadenza','Media Storica'),pch=19,col=c('blue','red'),cex=1)

dev.off()