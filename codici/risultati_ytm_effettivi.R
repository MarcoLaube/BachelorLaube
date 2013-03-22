
#cartella e libreria zoo per dataframe con serie storiche
setwd("C:\\R\\r-output")
library(zoo)
load('ottimizzazioneytm.RData')

#dataframe rendimenti effettivi
reffettivi<-matrix(,ncol=1,nrow=nrperiodi)
reffettivi<-zooreg(reffettivi,start=as.yearqtr(inizio,format ="%Y Q%q"),end=as.yearqtr(fine,format ="%Y Q%q"),frequency=4)
names(reffettivi)<-"Rendimenti portofolio effettivi"

#dataframe stdev effettiva
stdeveffettiva<-matrix(,ncol=1,nrow=nrperiodi)
stdeveffettiva<-zooreg(stdeveffettiva,start=as.yearqtr(inizio,format ="%Y Q%q"),end=as.yearqtr(fine,format ="%Y Q%q"),frequency=4)
names(stdeveffettiva)<-'Rischio effettivo'

#dataframe sharpe effettivo
sharpeeffettivo<-matrix(,ncol=1,nrow=nrperiodi)
sharpeeffettivo<-zooreg(sharpeeffettivo,start=as.yearqtr(inizio,format ="%Y Q%q"),end=as.yearqtr(fine,format ="%Y Q%q"),frequency=4)
names(sharpeeffettivo)<-"Sharpe Ratio Effettivo"


#riordino dati giornalieri, tolgo le valute e rimetto a posto il EuYH (che mi dava problemi quando esportavo la serie storica da excel in txt, le tabulazioni non combaciavano, falsandomi tutti i dati seguenti, cosi l'ho messa alla fine
datigiornalieri<-read.zoo("dati.txt", format ="%d.%m.%Y") 
datigiornalieri2<-datigiornalieri[,-14]
datigiornalieri2<-datigiornalieri2[,-14]
datigiornalieri2<-datigiornalieri2[,-14]
datigiornalieri2<-cbind(datigiornalieri2[,1:8],datigiornalieri2[,14],datigiornalieri2[,9:13])

#preparazione rendimenti (trimestrali e annualizzati)
rendimentigeometrici<-function(x) {diff(log(x))}
qtrtoyear<-function(x) (1+x)^4-1
datitrimestrali<-aggregate(datigiornalieri2,as.yearqtr,tail,1)
rendimentitrimestrali<-rendimentigeometrici(datitrimestrali)
rendimentiannualizzati<-qtrtoyear(rendimentitrimestrali)

system.time(for(i in index(pesi)) {
#i<-'2008 Q2'
			
#calcolo rendimenti effettivi	
reffettivi[as.yearqtr(i,format="%Y Q%q"),]<-pesi[as.yearqtr(i,format="%Y Q%q"),]%*%t(rendimentiannualizzati[as.yearqtr(i,format="%Y Q%q"),])

#calcolo sharpe effettivo

#calcolo matrice covarianza con i rendimenti settimanali calcolati il mercoledi, con i dati dall'inizio fino al periodo in considerazione
nextwed<-function(x) 7 * ceiling(as.numeric(x-3+4) / 7) + as.Date(3-4) #trova il valore del mercoledi
weektoyear<-function(x) (1+x)^54-1
datisettimanali<-aggregate(datigiornalieri2,nextwed,tail,1)
rendimentisettimanali<-rendimentigeometrici(datisettimanali)
rsettannual<-100*weektoyear(rendimentisettimanali)


rendimentixcov<-window(rsettannual, start = start(rendimentisettimanali), end=as.Date(as.yearqtr(i))) #estrae i dati dall'inizio fino al periodo corrente
covarianze<-cov(rendimentixcov)


stdeveffettiva[as.yearqtr(i,format="%Y Q%q"),]<-(pesi[as.yearqtr(i,format="%Y Q%q"),]%*%covarianze%*%t(pesi[as.yearqtr(i,format="%Y Q%q"),]))^0.5

sharpeeffettivo[as.yearqtr(i,format="%Y Q%q"),]<-(reffettivi[as.yearqtr(i,format="%Y Q%q"),]-rendimentiannualizzati[as.yearqtr(i,format="%Y Q%q"),1])/stdeveffettiva[as.yearqtr(i,format="%Y Q%q"),]
}
)

#rendimento (annualizzato), stdev sharpe totale)
rendimentototale<-((prod((1+reffettivi))/nrperiodi)+1)^(nrperiodi^(-1))

#arrotondamento
pesi<-100*round(pesi, digits=4)
sharpe<-round(sharpe, digits=2)
rporto<-100*round(rporto, digits=4)
stdev<-100*round(stdev, digits=4)
stdeveffettiva<-round(stdeveffettiva, digits=2)
sharpeeffettivo<-100*round(sharpeeffettivo,digits=4)
reffettivi<-100*round(reffettivi, digits=4)

#merge
sharpe<-merge(sharpe,sharpeeffettivo)
rporto<-merge(rporto,reffettivi)
stdev<-merge(stdev,stdeveffettiva)

#creazione lista risultatytm
risultatiytm<-list()
risultatiytm$pesi<-pesi
risultatiytm$sharpe<-sharpe
risultatiytm$rporto<-rporto
risultatiytm$stdev<-stdev



save.image('risultatiytm.RData')







#media storica per periodo
mediastorica<-matrix(,ncol=14,nrow=19)
mediastorica<-zooreg(mediastorica,start=as.yearqtr(inizio,format ="%Y Q%q"),end=as.yearqtr(fine,format ="%Y Q%q"),frequency=4)
for(j in index(mediastorica)){
	primoytrmedia<-as.yearqtr(j,format ="%Y Q%q" )-nrrendimentixmedia*4/12
	campionerendimenti<-window(rendimentiannualizzati,start=as.yearqtr(primoytrmedia,format ="%Y Q%q"),end=as.yearqtr(j,format ="%Y Q%q"))
	mediastorica[as.yearqtr(j,format="%Y Q%q"),]<-apply(campionerendimenti,2,FUN=mean)
}










