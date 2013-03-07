#riordino rendimenti annualizzati, tolgo le valute e rimetto a posto il EuYH (che mi dava problemi quando esportavo la serie storica da excel in txt, le tabulazioni non combaciavano, falsandomi tutti i dati seguenti, cosi l'ho messa alla fine
rendimentiannualizzati<-rendimentiannualizzati[,-14]
rendimentiannualizzati<-rendimentiannualizzati[,-14]
rendimentiannualizzati<-rendimentiannualizzati[,-14]
rendimentiannualizzati<-cbind(rendimentiannualizzati[,1:8],rendimentiannualizzati[,14],rendimentiannualizzati[,9:13])

#media storica per periodo
mediastorica<-matrix(,ncol=14,nrow=39)
mediastorica<-zooreg(mediastorica,start=as.yearqtr(inizio,format ="%Y Q%q"),end=as.yearqtr(fine,format ="%Y Q%q"),frequency=4)
for(j in index(mediastorica)){
	primoytrmedia<-as.yearqtr(j,format ="%Y Q%q" )-nrrendimentixmedia*4/12
	campionerendimenti<-window(rendimentiannualizzati,start=as.yearqtr(primoytrmedia,format ="%Y Q%q"),end=as.yearqtr(j,format ="%Y Q%q"))
	mediastorica[as.yearqtr(j,format="%Y Q%q"),]<-apply(campionerendimenti,2,FUN=mean)
}

#calcolo rendimenti effettivi
rendimentieffettivi<-window(rendimentiannualizzati,start=as.yearqtr(inizio,format ="%Y Q%q")+3/12,end=as.yearqtr(fine,format ="%Y Q%q")+3/12)

#calcolo rendimento effettivo portofoglio
rendimentiportofolio<-as.vector(diag(pesi%*%t(rendimentieffettivi)))
rendimentototale<-prod(rendimentiportofolio+1)^(1/41)-1
premiorischioportofolio<-as.matrix(rendimentiportofolio-rendimentieffettivi[,1])
#calcolo sharperatio effettivo portofoglio
sigma<-(0.1-rendimentieffettivi[,1])*(sharpe[,1]^(-1))
sharpeeffettivo<-(rendimentiportofolio-rendimentieffettivi[,1])*(sigma[,1]^(-1))
