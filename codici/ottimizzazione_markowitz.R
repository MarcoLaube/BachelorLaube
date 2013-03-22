# input
 


#input semplici
v.nomiSerieStoriche<-read.csv("nomiseriestoriche.csv", header = F)
valutaRiferimento<-"EUR"

reportingHorizon<-1
orizzonteExpectedReturns<-1
rendimentoMinimo<-0.09
rendimentoMassimo<-0.11
nrOttimizzazioni<-3
relativeOptimisation<-1
idRiskFree<-"Euro Cash 3M"
conVincoloBenchmark<-1
df_vincoliMultipliNomi<-read.csv2("vincolimultiplinomi.csv",header=T,stringsAsFactors = F) #il df non e' come nell'esempio, se c'e' un problema controllare
df_vincoliSemplici<-read.csv2("vincolisemplici.csv",header=T)
df_vincoliMultipliDati<-read.csv2("vincolimultiplidati.csv",header=T)[-1]
names(df_vincoliMultipliDati)<-c("alMax","alMax","alMax","alMax","alMax","alMax","alMax","alMax","alMax")

#preparazione rendimenti
rendimentigeometrici<-function(x) {diff(log(x))}
qtrtoyear<-function(x) (1+x)^4-1
datigiornalieri<-read.zoo("dati.txt", format ="%d.%m.%Y") 
datitrimestrali<-aggregate(datigiornalieri,as.yearqtr,tail,1)
rendimentitrimestrali<-rendimentigeometrici(datitrimestrali)
rendimentiannualizzati<-qtrtoyear(rendimentitrimestrali)


#preparazione aspettative

primoytrmedia<-as.yearqtr(i,format ="%Y Q%q" )-nrrendimentixmedia*4/12
campionerendimenti<-window(rendimentiannualizzati,start=as.yearqtr(primoytrmedia,format ="%Y Q%q"),end=as.yearqtr(i,format ="%Y Q%q"))
aspettative<-apply(campionerendimenti,2,FUN=mean)

aspettative<-cbind(v.nomiSerieStoriche,aspettative)
names(aspettative)<-c("Name","E.r.")

#matrice covarianza
covarianze<-cov(rendimentiannualizzati) #solo dei titoli del portafolio
v.nomiSerieStoriche<-t(v.nomiSerieStoriche)
colnames(covarianze)<-v.nomiSerieStoriche




#preparazione dataframe aspettative degli asset
matriceaspettative<-cbind(df_vincoliMultipliNomi[1],0,df_vincoliMultipliNomi[2],0)
names(matriceaspettative)<-c("Name","E.r.","Currency","Currency.Risk")
df_aspettative<-merge(aspettative,matriceaspettative,by="Name",all.y=TRUE, sort=FALSE)
df_aspettative[,3]<-NULL
names(df_aspettative)<-c("Name","E.r.","Currency","Currency.Risk")
df_aspettative<-merge(df_vincoliSemplici,df_aspettative,by="Name",sort=FALSE)#i due df devono essere ordinati uguali perchè il programma di ottimizzazione lo richiede
df_aspettative$equals<-NULL
df_aspettative$alMin<-NULL
df_aspettative$alMax<-NULL
df_aspettative$Currency.x<-NULL
names(df_aspettative)<-c("Name","E.r.","Currency","Currency.Risk")

#preparazione dataframe aspettative Fx
currencyandcost<-data.frame(c( "USD", "CHF", "EUR" ),0)
names(currencyandcost)<-c("Name","Hedge Cost")
df_aspettativeFx<-merge(aspettative,currencyandcost,by="Name")
names(df_aspettativeFx)<-c("Currency","E.r.","Hedge.costs")
df_aspettativeFx$E.r.<-0

## uncomment to debug 

setwd("C:\\R\\r-output")
rSourceDirectory = "C:\\R\\r-source"
rOutputdirectory = "C:\\R\\r-output"

## 2008-10-14: corretta bug nel calcolo dello Sharpe Ratio quando non esiste
## il risk free

## start procedure
library(quadprog)
library(tcltk)
options(stringsAsFactors = FALSE)
exitWithError = 0
markowitzVersion = " 1-1"


vech <- function(M)
{
	x <- M[,1]
	k <- nrow(M)
	if (k>1)
	{
		for (i in 2:nrow(M))
		{
			x <- c(x,M[i:k,i])
		}
	}
	return(x)
}

trim <- function(s)
{
	# this function remove leading and trailing spaces
	# s: a scalar or vector of strings
	
	s <- sub(pattern="^ +", replacement="", x=s)
	s <- sub(pattern=" +$", replacement="", x=s)
	return(s)  
}


aggiungiClassi <- function(classiConNomi,x,ancheColonne=FALSE)
{
	if (is.vector(x))
	{ 
		y <- vector(mode = mode(x), length = length(classiConNomi))
		names(y) <- classiConNomi
		y[names(x)] <- x
		esisteNome <- is.element(classiConNomi,names(x))
		y[!esisteNome] <- NA
		return(y)
	}
	
	if (is.data.frame(x))
	{
		nomiRighe = rownames(x)
		y <- vector(mode = mode(x[[1]]), length = length(classiConNomi))
		names(y) <- classiConNomi
		y[nomiRighe] <- x[[1]]
		z <- data.frame(y,stringsAsFactors = FALSE)
		if (ncol(x)>1)
		{
			for (i in 2:ncol(x))
			{
				y <- vector(mode = mode(x[[i]]), length = length(classiConNomi))
				names(y) <- classiConNomi
				y[nomiRighe] <- x[[i]]
				z <- data.frame(z,y,stringsAsFactors = FALSE)             
			}
		}
		rownames(z) <- classiConNomi
		colnames(z) <- colnames(x)
		esisteNome <- is.element(classiConNomi,nomiRighe)
		y[!esisteNome,] <- NA
		return(z)
	}
	
	if (is.matrix(x))
	{
		numeroRighe = length(classiConNomi)
		if (ancheColonne) numeroColonne = numeroRighe else numeroColonne = ncol(x) 
		y <- matrix(nrow=numeroRighe,ncol=numeroColonne)
		if (ancheColonne) nomiColonna = classiConNomi else nomiColonna <- colnames(x)
		rownames(y) <- classiConNomi
		colnames(y) <- nomiColonna
		y[rownames(x),colnames(x)] <- x
		esisteNome <- is.element(classiConNomi,rownames(x))
		y[!esisteNome,] <- NA
		if (ancheColonne) y[,!esisteNome] <- NA 
		return(y)
	}  
}

rimuoviAlgencanOutput <- function()
{
	a = list.files() 
	if (length(a)>0)
	{
		daRimuovere <- substr(a,1,6) == "myOut_"
		for (nomeFile in a[daRimuovere])
		{
			file.remove(nomeFile)
		}
	}
}


verificaDoppioni <- function(v.nomi, messaggio="")
{
	if (length(v.nomi) == length(unique(v.nomi))) return()
	a.tmp <- tapply(X=rep(1,length(v.nomi)), INDEX=as.factor(v.nomi), FUN=sum)
	is.notUnique <- a.tmp != 1
	if (any(is.notUnique))
	{
		stringa1 = paste(dimnames(a.tmp)[[1]][is.notUnique],sep="",collapse=", ")
		stringa2 = paste(messaggio,":\n",stringa1,sep="")
		tkmessageBox(message=stringa2,icon="error")
		exitWithError = 1
		save.image("error.RData")
		stop()      
	}
}

is.Class <- function(v.nomi)
{
	return(substr(v.nomi,1,5)=="CLASS")  
}

rimuoviRiskFreeClass <- function(x)
{
	if (is.vector(x))
	{
		toRemove <- (names(x) == "CLASS RISK FREE") | (names(x) == l.datiRiskFree$idRiskFree)
		return(x[!toRemove])
	}
	
	if (is.matrix(x))
	{
		colNames <- colnames(x)
		rowNames <- rownames(x)
		colToRemove <- (colNames == "CLASS RISK FREE") | (colNames == l.datiRiskFree$idRiskFree)
		rowToRemove <- (rowNames == "CLASS RISK FREE") | (rowNames == l.datiRiskFree$idRiskFree)
		return(x[!rowToRemove,!colToRemove,drop=FALSE])
	}
	return(x)
}




isEmptyClass <- function(v.nomi)
{
	isEmpty <- rep(FALSE,length(v.nomi))
	if (length(v.nomi)==0) return(isEmpty)
	previousIsClass = FALSE
	myIsClass <- substr(v.nomi,1,5)=="CLASS"
	for (i in 1:length(v.nomi))
	{
		if (myIsClass[i])
		{
			if (previousIsClass) isEmpty[i-1] <- TRUE
			previousIsClass = TRUE
		} else {
			previousIsClass = FALSE
		}
	}
	## considera l'ultimo elemento
	isEmpty[i] <- myIsClass[i]
	return(isEmpty)
}


trasformaVincoliClassi <- function(df_vincoliSempliciClasse,df.classiConNomi)
{
	## Attenzioni, df.Classi contiene il riskFree
	tipiVincoli = c("equals","alMax","alMin")
	valoreVincolo <- as.numeric()  
	l.results <- list()  
	df.nomi_classi <- df.classiConNomi[!is.Class(df.classiConNomi[["Name"]]),,drop=FALSE]    
	for (tipoVincolo in tipiVincoli)
	{
		## tmp ï¿½ un vettore che contiene vero/falso a seconda che lo strumento appartenga
		## alla classe coinvolta nel vincolo
		v.tmp <- as.numeric()
		validi <- !is.na(df_vincoliSempliciClasse[,tipoVincolo])
		if (any(validi))
		{
			valoreVincolo <- c(valoreVincolo,df_vincoliSempliciClasse[validi,tipoVincolo])
			nomiVincoli <- df_vincoliSempliciClasse[validi,"Name"]
			for (nomeVincolo in nomiVincoli)
			{
				v.tmp <- c(v.tmp,df.nomi_classi[,"Class"]==nomeVincolo)
			}
			A = matrix(v.tmp,nrow=sum(validi),ncol=nrow(df.nomi_classi),byrow=TRUE)
			colnames(A) = df.nomi_classi[["Name"]]
			rownames(A) = paste(tipoVincolo,"..",1:sum(validi),sep="")
			l.results[[tipoVincolo]] <- A
		}
		if (length(l.results)>0) M = l.results[[1]] else M <- NULL
		if (length(l.results)>1)
		{ 
			for (i in 2:length(l.results)) M <- rbind(M, l.results[[i]])
		}
		if (length(l.results)>0) M <- cbind(valoreVincolo,M)
	}
	return(M)
}

assegnaClassi <- function(df.nomi)
{
	# df.nomi ï¿½ un dataframe con un campo "Nomi" che contiene la lista delle
	# classi ciascuna seguita dai nomi dei prodotti ad essa associata
	
	x <- data.frame(Name=df.nomi[["Name"]],Class=df.nomi[["Name"]],stringsAsFactors = FALSE)
	sonoClassi <- is.Class(df.nomi[["Name"]])
	nrRighe <- length(df.nomi[["Name"]])
	if (any(sonoClassi))
	{
		nomiClassi <-  df.nomi[sonoClassi,"Name"]
		index = 1
		for (i in (1:nrRighe)[sonoClassi])
		{
			x[i:nrRighe,"Class"] = nomiClassi[index]
			index = index + 1
		}
	}
	return(x)
}


AlgencanErrorString <- function(value)
{
	idErrore <- c("0","1","2","3","4","5","-90","-91","-92","-93","-94","-95","-96")
	
	stringa  <- c("0: convergence with feasibility, optimality and complementarity.",
			"1: was achieved the maximum number of outer iterations (maxoutit).",
			"2: it was achieved the maximum total number of inner iterations (maxtotit).",
			"3: it was achieved the maximum total number of functional evaluations (maxtotfc).",
			"4: the algorithm stopped by ``lack of feasibility progress'', 
					i.e., the current point is infeasible (the 
					constraints violation is larger than the tolerance epsfeas) 
					and the constraint violation has not even simple decrease 
					during maxitncp consecutive iterations. In this case, the 
					problem may be infeasible.",
			"-90: subroutine evalf   retuned an error flag.",
			"-91: subroutine evalc   retuned an error flag.",
			"-92: subroutine evalg   retuned an error flag.",
			"-93: subroutine evaljac retuned an error flag.",
			"-94: subroutine evalh   retuned an error flag.", 
			"-95: subroutine evalhc  retuned an error flag.", 
			"-96: subroutine evalhlp retuned an error flag.")
	
	isValue <- idErrore == as.character(value)
	if (any(isValue))
	{
		return(stringa[isValue])
	} else {
		return(paste(value,": Error without id: please contact the developers",sep=""))
	}     
}


matriceTrasfLineare <- function(S,df.valutaRischioValuta)
{
	## S: matrice quadrata di varianza covarianza delle serie storiche disponi-
	##    bili. I cambi sono inclusi e le varianze e covarianze deveno essere 
	##    espresse rispetto a cambi definiti come "valuta riferi"/"valuta locale"
	## df.NomeValutaRischioValuta: un dataframe contenente il nome, la valuta e se
	##                             c'è rischio valuta per tutte le posizioni risk 
	##                             free escluso.
	## questa funzione costruisce la matrice della trasformazione lineare che porta
	## gli strumenti disponibili (in valuta locale) in quelli poi effettivamente 
	## inseriti in portafoglio tenendo anche conto di un eventuale rischio cambio.
	
	nomiS = rownames(S)
	nomiStrumenti = rownames(df.valutaRischioValuta)
	nrRighe = nrow(df.valutaRischioValuta)
	nrCol = ncol(S)
	A = matrix(0,nrow=nrRighe,ncol=nrCol)
	dimnames(A) = list(nomiStrumenti,nomiS)
	isRischioValuta <- as.logical(df.valutaRischioValuta[,"Currency risk"])
	
	for (i in 1:nrRighe)
	{
		tmp <- nomiS == nomiStrumenti[i]
		isValuta <- nomiS == df.valutaRischioValuta[i,"Currency"]
		tmp <- tmp | (isRischioValuta[i] & isValuta)
		A[i,] <- as.numeric(tmp)
	}
	
	return(A)  
}

completeBenchmarkWeights <- function(benchmarkWeights.df,df_expRetBenchmark)
{
	## this function join the two data.frames into a single one
	## benchmarkWeights.df: a data.frame with two columns ("Name","Weight")
	## df_expRetBenchmark: a data.frame with 4 columns ("Name","E(r)","Currency","Currency risk")
	
	colnames(benchmarkWeights.df) <- c("Name","Weight")
	benchmarkWeights.df[["Name"]] <- trim(benchmarkWeights.df[["Name"]])
	rownames(benchmarkWeights.df) <- benchmarkWeights.df[["Name"]]
	
	colnames(df_expRetBenchmark) <- c("Name","E(r)","Currency","Currency risk")
	df_expRetBenchmark[["Name"]] <- trim(df_expRetBenchmark[["Name"]])
	df_expRetBenchmark[["Currency"]] <- trim(df_expRetBenchmark[["Currency"]])
	rownames(df_expRetBenchmark) <- df_expRetBenchmark[["Name"]]
	
	benchmarkWeightsNotClass.v <- !is.Class(benchmarkWeights.df[["Name"]])
	desiredInstruments.v <- benchmarkWeights.df[["Name"]][benchmarkWeightsNotClass.v]
	availables.v <- is.element(desiredInstruments.v,df_expRetBenchmark[["Name"]])
	if (any(!availables.v))
	{
		string = "Some instruments in the benchmark do not have a corresponding expectation.\nVerify the Benchmark definition with the available expectations."
		tkmessageBox(message=string,icon="error")
		string = paste(desiredInstruments.v[!availables.v],collapse=",")
		tkmessageBox(message=string,icon="error")
		exitWithError <<- 1
		save.image("error.RData")
		stop()
	}
	
	tmp.df <- benchmarkWeights.df
	nbRows <- nrow(benchmarkWeights.df)
	tmp.df <- data.frame(tmp.df,"E(r)"=rep(NA_real_,nbRows),"Currency"=rep(NA_character_,nbRows),
			"Currency risk"=rep(NA_integer_,nbRows))
	colnames(tmp.df) <- c("Name","Weight","E(r)","Currency","Currency risk")
	
	tmp.df[desiredInstruments.v,"E(r)"] <- df_expRetBenchmark[desiredInstruments.v,"E(r)"]
	tmp.df[desiredInstruments.v,"Currency"] <- df_expRetBenchmark[desiredInstruments.v,"Currency"]
	tmp.df[desiredInstruments.v,"Currency risk"] <- df_expRetBenchmark[desiredInstruments.v,"Currency risk"]
	return(tmp.df)
}





## trasforma alcune variabili provenienti da Excel
conVincoloBenchmark = as.numeric(conVincoloBenchmark) - 1
relativeOptimisation = as.numeric(relativeOptimisation) - 1

# define the reportingScaleFactor used to adjust the output:
# it is assumed that the returns follows a geometric brownian motion so that the
# standard square root of time rule is used.
reportingScaleFactor = reportingHorizon/orizzonteExpectedReturns

## remove leading or trailing spaces from the idRiskFree
idRiskFree <- trim(idRiskFree)

## il data.frame aspettative
df_aspettative[["Name"]] <- trim(df_aspettative[["Name"]])
df_aspettative[["Currency"]] <- trim(df_aspettative[["Currency"]])
## modifica il nome della seconda colonna da E.r. in E(r) e della quarta colonna
## da "Rischio.valuta" in "Currency risk"
colnames(df_aspettative)[c(2,4)] <- c("E(r)","Currency risk")

## cambia i nomi alle aspettative sulle valute
colnames(df_aspettativeFx) <- c("Currency","E(r)","Hedge costs")
df_aspettativeFx[["Currency"]] <- trim(df_aspettativeFx[["Currency"]])

## il data.frame vincoliSemplici
df_vincoliSemplici[["Name"]] <- trim(df_vincoliSemplici[["Name"]])
df_vincoliSemplici[["Currency"]] <- trim(df_vincoliSemplici[["Currency"]])


## costruisci il data.frame df.vincoliMultipli a partire da df_vincoliMultipliDati e
## df_vincoliMultipliNomi
colnames(df_vincoliMultipliNomi) <- c("Name","Currency")
df.vincoliMultipli <- rbind(c("Value",""),df_vincoliMultipliNomi[,"Name",drop=FALSE])
if (!is.null(df_vincoliMultipliDati))
{
	df.vincoliMultipli <- cbind(df.vincoliMultipli,df_vincoliMultipliDati)
}

df.vincoliMultipli[["Name"]] <- trim(df.vincoliMultipli[["Name"]])

## considera i pesi benchmark
if (exists("df_pesiBenchmark",inherits=FALSE)) 
{
	df_pesiBenchmark <- completeBenchmarkWeights(df_pesiBenchmark,df_expRetBenchmark)
}
## considera il df_infoSerieStoricaBenchmark
if (exists("df_infoSerieStoricaBenchmark",inherits=FALSE))
{
	colnames(df_infoSerieStoricaBenchmark) <- c("Name","E(r)","Currency","Currency risk")
	rownames(df_infoSerieStoricaBenchmark) <- df_infoSerieStoricaBenchmark[1,"Name"]
}

# verifica che i nomi presenti nelle aspettative e nei vincoli siano gli stessi
# anche nell'ordine in cui figurano
sono.uguali = df_aspettative[["Name"]] == df_vincoliSemplici[["Name"]]
if (any(!sono.uguali))
{
	string = "The Excel spreadsheets 'Exp. returns' and 'simple constraints' contain different instruments.\nCheck the name to continue."
	tkmessageBox(message=string,icon="error")
	exitWithError = 1
	save.image("error.RData")
	stop()
}


sono.uguali = df_aspettative[["Name"]] == df.vincoliMultipli[-1,"Name"]
if (any(!sono.uguali))
{
	string = "The Excel spreadsheets 'Exp. returns' and 'multiple constraints' contain different instruments.\nCheck the name to continue."
	tkmessageBox(message=string,icon="error")
	exitWithError = 1
	save.image("error.RData")
	stop()
}
rm(sono.uguali)


## crea il vettore con i nomi delle classi
sonoClassi <- is.Class(df_aspettative[,"Name"])
v.nomiClassi <- df_aspettative[sonoClassi,"Name"]
## crea il data.frame con le colonne [Name,Class]. Attenzione: la colonna nomi
## contiene ancora i nomi delle classi!
df.classiConNomi <- assegnaClassi(df_aspettative)
## verifica che i nomi delle classi non siano doppi
verificaDoppioni(v.nomiClassi, messaggio="I nomi delle seguenti classi di assets si ripetono")

## estrai le righe "CLASS ..."
df_aspettative_orig <- df_aspettative
df_aspettative <- df_aspettative[!sonoClassi,,drop=FALSE]


## estrai le righe "CLASS ..."
sonoClassi <- is.Class(df_vincoliSemplici[,"Name"])
df_vincoliSempliciClasse <- df_vincoliSemplici[sonoClassi,,drop=FALSE]
df_vincoliSemplici_orig <- df_vincoliSemplici
df_vincoliSemplici <- df_vincoliSemplici[!sonoClassi,,drop=FALSE]



## estrai le righe "CLASS ..."
sonoClassi <- is.Class(df.vincoliMultipli[,"Name"]) 
df.vincoliMultipli_orig <- df.vincoliMultipli
df.vincoliMultipli <- df.vincoliMultipli[!sonoClassi,,drop=FALSE]
rm(sonoClassi)

## considera i pesi benchmark
if (exists("df_pesiBenchmark",inherits=FALSE)) 
{
	## elimina dal benchmark gli strumenti con peso 0 e le relative classi
	v.BenchmarkNamesClass_orig <- df_pesiBenchmark[["Name"]]
	noZeroPesoOrClass <- df_pesiBenchmark[["Weight"]] != 0  | is.Class(df_pesiBenchmark[["Name"]])
	df.tmp <- df_pesiBenchmark[noZeroPesoOrClass,,drop=FALSE]
	df.classiConNomiTmp <- assegnaClassi(df.tmp)
	v.tmp <- isEmptyClass(df.classiConNomiTmp[["Name"]])
	df_pesiBenchmark <- df.tmp[!v.tmp,,drop=FALSE]
	
	rm(noZeroPesoOrClass,df.tmp,df.classiConNomiTmp,v.tmp)
	
	
	## estrai le righe "CLASS ..." dal benchmark
	sonoClassiBenchmark <- is.Class(df_pesiBenchmark[,"Name"])
	v.nomiClassiBenchmark <- df_pesiBenchmark[sonoClassiBenchmark,"Name"]
	## crea il data.frame con le colonne [Name,Class]. Attenzione: la colonna nomi
	## contiene ancora i nomi delle classi!
	df.classiConNomiBenchmark <- assegnaClassi(df_pesiBenchmark)
	## verifica che i nomi delle classi non siano doppi
	verificaDoppioni(v.nomiClassiBenchmark, messaggio="Attention, within the benchmark the following asset classes are not unique")
	## elimina le classi di assets dal data.frame df.pesiBenchamrk
	df_pesiBenchmark_orig <- df_pesiBenchmark
	df_pesiBenchmark <- df_pesiBenchmark[!sonoClassiBenchmark,,drop=FALSE]
	## verifica che i nomi degli strumenti non siano doppi
	verificaDoppioni(df_pesiBenchmark[,"Name"], messaggio="Attention, within the benchmark the following instruments are not unique")
	## verifica che il riskFree non sia l'unico strumento nel benchmark
	if(nrow(df_pesiBenchmark) == 1)
	{
		if (df_pesiBenchmark[1,"Name"] == idRiskFree)
		{
			string = "The risk free is the benchmark => The Tracking Error corresponds the volatility of the portfolio.\nRemove the TE constraint to continue."
			tkmessageBox(message=string,icon="error")
			exitWithError = 1
			save.image("error.RData")
			stop()
		}
	}
}

## verifica che non ci siano nomi ripetuti più volte e poi assegna i nomi alle
## righe del data.frame
verificaDoppioni(df_aspettative[["Name"]],messaggio= "Attention, within the return expectations the following instrument names are not unique")
rownames(df_aspettative) <-  df_aspettative[["Name"]]
verificaDoppioni(df_aspettativeFx[["Name"]],messaggio= "Attention, within the currency names the following currencies are not unique")
rownames(df_aspettativeFx) <-  df_aspettativeFx[["Currency"]]
verificaDoppioni(df_vincoliSemplici[["Name"]],messaggio="Attention, within the single constraints the following names are not unique")
rownames(df_vincoliSemplici) <-  df_vincoliSemplici[["Name"]]
verificaDoppioni(df.vincoliMultipli[["Name"]],messaggio="Attention, within the multiple constraints the following names are not unique")
rownames(df.vincoliMultipli) <-  df.vincoliMultipli[["Name"]]
df_aspettative[["Name"]] <- NULL
df_vincoliSemplici[["Name"]] <- NULL
df.vincoliMultipli[["Name"]] <- NULL




# verifica che i nomi presenti nelle aspettative e nei vincoli siano gli stessi
# anche nell'ordine in cui figurano
sono.uguali = rownames(df_aspettative) == rownames(df_vincoliSemplici)
if (any(!sono.uguali))
{
	string = "The Excel spreadsheets 'Exp. returns' and 'simple constraints' contain different instruments.\nCheck the name to continue."
	tkmessageBox(message=string,icon="error")
	exitWithError = 1
	save.image("error.RData")
	stop()
}


sono.uguali = rownames(df_aspettative) == rownames(df.vincoliMultipli)[-1]
if (any(!sono.uguali))
{
	string = "The Excel spreadsheets 'Exp. returns' and 'simple constraints' contain different instruments.\nCheck the name to continue."
	tkmessageBox(message=string,icon="error")
	exitWithError = 1
	save.image("error.RData")
	stop()
}
rm(sono.uguali)


## modifica i rendimenti attesi delle aspettative
# verifica che non ci siano buchi
if (any(df_aspettative[,"Currency"] == ""))
{
	string = "Some currencies do not have the corresponding expectation. Complete the data to continue."
	tkmessageBox(message=string,icon="error")
	exitWithError = 1
	save.image("error.RData")
	stop() 
}
## verifica che il campo "Currency risk" sia sempre pieno
if (any(is.na(df_aspettative[,"Currency risk"])))
{
	string = "Some instruments do have an empty 'Currency risk' field. Complete the data to continue"
	tkmessageBox(message=string,icon="error")
	exitWithError = 1
	save.image("error.RData")
	stop() 
}

# verifica che ci siano tutti i rendimenti attesi sulle valute necessarie
v.conRischioValuta <- as.logical(df_aspettative[,"Currency risk"])
v.valuteSenzaHedge <- df_aspettative[v.conRischioValuta,"Currency"]
# aggiorna i rendimenti delle serie non hedged
if (length(v.valuteSenzaHedge) > 0)
{
	v.valuteSenzaHedge = unique(v.valuteSenzaHedge)
	v.valuteSenzaHedge = setdiff(v.valuteSenzaHedge,valutaRiferimento)
	valuteMancanti = setdiff(v.valuteSenzaHedge, df_aspettativeFx[,"Currency"])
	if (length(valuteMancanti) > 0)
	{
		string = "The following currencies do not have the expected return required to adjust for the currency risk: \n"
		string1 = paste(valuteMancanti,collapse=",")
		string = paste(string,string1,sep="") 
		tkmessageBox(message=string,icon="error")
		exitWithError = 1
		save.image("error.RData")
		stop()
	}
	rm(valuteMancanti)
	
	if (length(v.valuteSenzaHedge)>0)
	{
		## verifica che per tutte le valute senza Hedge ci sia il campo "E(r)"
		if (any(is.na(df_aspettativeFx[v.valuteSenzaHedge,"E(r)"])))
		{
			string = "Some currency expected returns are missing: \n"
			string1 = paste(v.valuteSenzaHedge,collapse=",")
			string = paste(string,string1,sep="") 
			tkmessageBox(message=string,icon="error")
			exitWithError = 1
			save.image("error.RData")
			stop()      
		}
		for (valuta in v.valuteSenzaHedge)
		{
			v.stessaValuta = df_aspettative[,"Currency"]== valuta
			ok = v.stessaValuta &  v.conRischioValuta
			df_aspettative[ok,"E(r)"] =  df_aspettative[ok,"E(r)"] +  df_aspettativeFx[valuta,"E(r)"]
		}
	}
} # if (length(v.valuteSenzaHedge) > 0)

# ora per le serie hedged vanno aggiunti i rendimenti dell'hedge
v.senzaRischioValuta <- !v.conRischioValuta
v.valuteConHedge <- df_aspettative[v.senzaRischioValuta,"Currency"]
# aggiorna i rendimenti delle serie non hedged
if (length(v.valuteConHedge) > 0)
{ 
	v.valuteConHedge = unique(v.valuteConHedge)
	v.valuteConHedge = setdiff(v.valuteConHedge,valutaRiferimento)
	valuteMancanti = setdiff(v.valuteConHedge, df_aspettativeFx[,"Currency"])
	if (length(valuteMancanti) > 0)
	{
		string = "The following currencies are missing: \n"
		string1 = paste(valuteMancanti,collapse=",")
		string = paste(string,string1,sep="") 
		tkmessageBox(message=string,icon="error")
		exitWithError = 1
		save.image("error.RData")
		stop()
	}
	rm(valuteMancanti)
	
	if (length(v.valuteConHedge)>0)
	{
		## verifica che per tutte le valute con Hedge ci sia il campo "Hedge costs"
		missingCurrencyWithHedge = is.na(df_aspettativeFx[v.valuteConHedge,"Hedge costs"])
		if (any(missingCurrencyWithHedge))
		{
			string = "The following currencies require 'Hedge costs' input: \n"
			string1 = paste(df_aspettativeFx[v.valuteConHedge,"Currency"][missingCurrencyWithHedge],collapse=",")
			string = paste(string,string1,sep="") 
			tkmessageBox(message=string,icon="error")
			exitWithError = 1
			save.image("error.RData")
			stop()      
		}
		rm(missingCurrencyWithHedge)
		for (valuta in v.valuteConHedge)
		{
			v.stessaValuta = df_aspettative[,"Currency"] == valuta
			ok = v.stessaValuta &  v.senzaRischioValuta
			df_aspettative[ok,"E(r)"] =  df_aspettative[ok,"E(r)"] +  df_aspettativeFx[valuta,"Hedge costs"]
		}
	}
} # if (length(v.valuteConHedge) > 0)


## costruisci la matrice delle serie storiche
Z = list()
Z$S <- as.matrix(covarianze)*orizzonteExpectedReturns
# verifica che il numero di serie storiche sia il medesimo di quello dei nomi
if (length(v.nomiSerieStoriche) != length(colnames(covarianze)))
{
	string = "The dimension of the covariance matrix does not correspond to the list of names to associate with it.\nCheck the file containing the covariance matrix."
	tkmessageBox(message=string,icon="error")
	exitWithError = 1
	save.image("error.RData")
	stop()
}

dimnames(Z$S) <- list(v.nomiSerieStoriche,v.nomiSerieStoriche)
rm(covarianze)

Z$serieDisponibili <- colnames(Z$S)
Z$Stdev <- matrix(sqrt(diag(Z$S)),ncol=1)
dimnames(Z$Stdev) <- list(Z$serieDisponibili,"Stdev")


#    Verifica se c'ï¿½ il risk free: il risk free ï¿½ identificato dal nome dell'unica
#    posizione della "CLASS RISK FREE"

riskFactors <- list()
riskFactors$classiConNomi <- df_aspettative_orig[,"Name"]

conRiskFree = is.element("CLASS RISK FREE",df_aspettative_orig[["Name"]])

# se c'ï¿½ il risk free levalo dal df_aspettative, df_vincoliSemplici, df.vincoliMultipli
# e consideralo a parte
if (conRiskFree)
{
	# identifica la posizione all'interno della lista originale
	riskFreePosizioneClasse <- (1:nrow(df_aspettative_orig))[df_aspettative_orig[["Name"]] == "CLASS RISK FREE"]
	# per convenzione l'identificativo del risk free deve essere messo dopo la classe RISK FREE
	# quindi se la dimensione del data.frame df_aspettative ï¿½ uguale alla posizione della classe RISK FREE
	# significa che la classe RISK FREE ï¿½ situata all'ultima riga della tabaella e quindi manca 
	# l'identificativo per il risk free
	if (nrow(df_aspettative_orig) == riskFreePosizioneClasse)
	{
		string = "Warning: the 'CLASS RISK FREE' is empty. Remove it from the list to continue."
		tkmessageBox(message=string,icon="error")
		exitWithError = 1
		save.image("error.RData")
		stop()      
	}
	if (is.Class(df_aspettative_orig[riskFreePosizioneClasse + 1,"Name"]))
	{
		string = "Warning: the 'CLASS RISK FREE' is empty. Remove it from the list to continue."
		tkmessageBox(message=string,icon="error")
		exitWithError = 1
		save.image("error.RData")
		stop()      
	}
	
	idRiskFree_tmp <- df_aspettative_orig[riskFreePosizioneClasse + 1,"Name"]
	# verifica che l'identificativo risk free sia quello indicato nel foglio Setup
	if (idRiskFree_tmp != idRiskFree)
	{
		string = "Error: the risk free identifier used in the spreadsheet 'Setup' does not\n to the one used in the 'CLASS RISK FREE'."
		tkmessageBox(message=string,icon="error")
		exitWithError = 1
		save.image("error.RData")
		stop() 
	}
	
	includi = riskFreePosizioneClasse:(riskFreePosizioneClasse + 1)
	riskFactors$classiConNomi <- riskFactors$classiConNomi[-includi]
	includi = is.Class(riskFactors$classiConNomi)
	riskFactors$classi <- riskFactors$classiConNomi[includi]
	riskFactors$nomi <-  riskFactors$classiConNomi[!includi]
	
	# il df_aspettative
	isRiskFree <- rownames(df_aspettative) == idRiskFree
	df.riskFreeAspettative  <- df_aspettative[isRiskFree,,drop=FALSE]
	df_aspettative <- df_aspettative[!isRiskFree,,drop=FALSE]
	
	# il df_vincoliSemplici
	isRiskFree <- rownames(df_vincoliSemplici) == idRiskFree
	df.riskFreeVincoliSemplici <- df_vincoliSemplici[isRiskFree,,drop=FALSE]
	df_vincoliSemplici <- df_vincoliSemplici[!isRiskFree,,drop=FALSE]
	
	# il df.vincoliMultipli
	isRiskFree <- rownames(df.vincoliMultipli) == idRiskFree
	df.riskFreeVincoliMultipli  <- df.vincoliMultipli[isRiskFree,,drop=FALSE]
	df.vincoliMultipli <- df.vincoliMultipli[!isRiskFree,,drop=FALSE]
	
	# verifica che i vincoli uguale, alMin e alMax contenuti in 
	# df_vincoliSempliciClasse per la classe "CLASS RISK FREE" siano vuoti
	isClasseRiskFree <- df_vincoliSempliciClasse[,"Name"] == "CLASS RISK FREE"
	notEmpty <- !is.na(df_vincoliSempliciClasse[isClasseRiskFree,c("equals","alMin","alMax")])
	if (any(notEmpty))
	{
		string = paste("Error: you assign one or more simple constraints to the 'CLASS RISK FREE'.\n",
				"Remove and assign them to the '", idRiskFree,"'.",sep="")
		tkmessageBox(message=string,icon="error")
		exitWithError = 1
		save.image("error.RData")
		stop()          
	}
	rm(isClasseRiskFree,notEmpty)
	
	l.datiRiskFree <- list()
	l.datiRiskFree$conRiskFree = conRiskFree
	l.datiRiskFree$idRiskFree = idRiskFree
	l.datiRiskFree$m.Aspettative = as.matrix(df.riskFreeAspettative[,"E(r)",drop=FALSE])
	l.datiRiskFree$m.VincoliSemplici = t(as.matrix(df.riskFreeVincoliSemplici[,c("equals","alMin","alMax"),drop=FALSE]))
	l.datiRiskFree$m.VincoliMultipli = t(as.matrix(df.riskFreeVincoliMultipli))
	rm(df.riskFreeAspettative,df.riskFreeVincoliSemplici,df.riskFreeVincoliMultipli)
	
	includi = riskFreePosizioneClasse:(riskFreePosizioneClasse + 1)
	l.datiRiskFree$classiConNomi <- df_aspettative_orig[includi,"Name"]
	l.datiRiskFree$classi <- df_aspettative_orig[riskFreePosizioneClasse,"Name"]
	l.datiRiskFree$nomi <- idRiskFree   
	rm(includi,riskFreePosizioneClasse,isRiskFree,conRiskFree,idRiskFree) 
	
} else {
	l.datiRiskFree <- list()
	l.datiRiskFree$conRiskFree = conRiskFree
	l.datiRiskFree$idRiskFree = idRiskFree    
	includi = is.Class(riskFactors$classiConNomi)
	riskFactors$classi <- riskFactors$classiConNomi[includi]
	riskFactors$nomi <- riskFactors$classiConNomi[!includi]
	rm(includi)
	rm(conRiskFree)
}





##  Verifica che tutte le serie con expected return abbiano una serie storica
##  per il calcolo della matrice di varianza covarianza 
nomiSerieConAspettative = rownames(df_aspettative)
senza.serie = !is.element(rownames(df_aspettative),Z$serieDisponibili)

if (any(senza.serie)) {
	string = "Some instruments do not have a corresponding time series in the covariance matrix:\n"
	string = paste(string,paste(nomiSerieConAspettative[senza.serie],sep="",collapse=", "),sep="")
	tkmessageBox(message=string,icon="error")
	exitWithError = 1
	save.image("error.RData")
	stop()
} else {
	rm(senza.serie)
}


aMatrix = matriceTrasfLineare(Z$S,df_aspettative)
covMatrix <- aMatrix %*% Z$S %*% t(aMatrix)


j <- nrow(covMatrix)
hessian <- list()
hessian$vechCovMatrix <- vech(covMatrix)
hessian$nnzh <- (j^2 + j)/2
hessian$hlin <- vech(matrix(1:j,ncol=j,nrow=j))
hessian$hcol <- vech(matrix(1:j,ncol=j,nrow=j,byrow=TRUE))
rm(j)



## se c'e' un un benchmark controlla che la serie storica o le serie per la sua costruzione
## siano effettivamente disponibili e modifica il valore del vincolo trasformandolo in un
## vincolo sulla varianza
datiBenchmark <- list()
datiBenchmark$conVincoloBenchmark <- conVincoloBenchmark
datiBenchmark$relativeOptimisation <- relativeOptimisation

if (conVincoloBenchmark | relativeOptimisation)
{
	datiBenchmark$timeHorizon <- orizzonteExpectedReturns
	
	if (conVincoloBenchmark) {
		datiBenchmark$trackErrConstrSquaredYearly <- vincoloTrackingError^2
		datiBenchmark$trackErrorConstrSquared <- vincoloTrackingError^2 * orizzonteExpectedReturns
	} else {
		if (exists(vincoloTrackingError, inherits = FALSE)) {
			datiBenchmark$trackErrConstrSquaredYearly <- vincoloTrackingError^2
			datiBenchmark$trackErrorConstrSquared <- vincoloTrackingError^2 * orizzonteExpectedReturns  
		} else {
			datiBenchmark$trackErrConstrSquaredYearly <- Inf
			datiBenchmark$trackErrorConstrSquared <- Inf
		}
	}
	
	datiBenchmark$conPesiBenchmark <- conPesiBenchmark
	if (conPesiBenchmark)
	{
		datiBenchmark$df_pesiBenchmark_orig <- df_pesiBenchmark_orig
		rm(df_pesiBenchmark_orig)
		
		## aggiusta gli expected returns del benchmark in valuta di riferimento
		# verifica che ci siano tutti i rendimenti attesi sulle valute necessarie
		v.conRischioValuta <- as.logical(df_pesiBenchmark[,"Currency risk"])
		v.valuteSenzaHedge <- df_pesiBenchmark[v.conRischioValuta,"Currency"]
		if (length(v.valuteSenzaHedge) > 0)
		{
			# first verify that the currencies without hedge (for which the expected
			# return is required) exist in the currency universe
			v.valuteSenzaHedge = unique(v.valuteSenzaHedge)
			v.valuteSenzaHedge = setdiff(v.valuteSenzaHedge,valutaRiferimento)
			valuteMancanti = setdiff(v.valuteSenzaHedge, df_aspettativeFx[,"Currency"])
			if (length(valuteMancanti) > 0)
			{
				string = "Error: benchmark's currency risk requires expected returns on the following currencies:\n"
				string1 = paste(valuteMancanti,collapse=",")
				string = paste(string,string1,sep="") 
				tkmessageBox(message=string,icon="error")
				exitWithError = 1
				save.image("error.RData")
				stop()
			}
			rm(valuteMancanti)
			
			# now verify that the expected returns are available
			if (length(v.valuteSenzaHedge)>0)
			{
				## verifica che per tutte le valute senza Hedge ci sia il campo "E(r)"
				if (any(is.na(df_aspettativeFx[v.valuteSenzaHedge,"E(r)"])))
				{
					string = "Dal Benchmark risulta che mancano alcuni rendimenti attesi su valute"
					tkmessageBox(message=string,icon="error")
					exitWithError = 1
					save.image("error.RData")
					stop()      
				}
				for (valuta in v.valuteSenzaHedge)
				{
					v.stessaValuta = df_pesiBenchmark[,"Currency"] == valuta
					ok = v.stessaValuta &  v.conRischioValuta
					df_pesiBenchmark[ok,"E(r)"] =  df_pesiBenchmark[ok,"E(r)"] +  df_aspettativeFx[valuta,"E(r)"]
				}
			}
		}  # if (length(v.valuteSenzaHedge) > 0)
		
		
		# ora per le serie hedged vanno aggiunti i rendimenti dell'hedge
		v.senzaRischioValuta <- !v.conRischioValuta
		v.valuteConHedge <- df_pesiBenchmark[v.senzaRischioValuta,"Currency"]
		# aggiorna i rendimenti delle serie non hedged
		if (length(v.valuteConHedge) > 0)
		{ 
			v.valuteConHedge = unique(v.valuteConHedge)
			v.valuteConHedge = setdiff(v.valuteConHedge,valutaRiferimento)
			valuteMancanti = setdiff(v.valuteConHedge, df_aspettativeFx[,"Currency"])
			if (length(valuteMancanti) > 0)
			{
				string = "The following currencies in the benchmark are missing:\n"
				string1 = paste(valuteMancanti,collapse=",")
				string = paste(string,string1,sep="") 
				tkmessageBox(message=string,icon="error")
				exitWithError = 1
				save.image("error.RData")
				stop()
			}
			rm(valuteMancanti)
			
			if (length(v.valuteConHedge)>0)
			{
				## verifica che per tutte le valute con Hedge ci sia il campo "Hedge costs"
				missingCurrencyWithHedge = is.na(df_aspettativeFx[v.valuteConHedge,"Hedge costs"])
				if (any(missingCurrencyWithHedge))
				{
					string = "The following currencies in the benchmark require 'Hedge costs' input: \n"
					string1 = paste(df_aspettativeFx[v.valuteConHedge,"Currency"][missingCurrencyWithHedge],collapse=",")
					string = paste(string,string1,sep="")
					tkmessageBox(message=string,icon="error")
					exitWithError = 1
					save.image("error.RData")
					stop()      
				}
				rm(missingCurrencyWithHedge)
				for (valuta in v.valuteConHedge)
				{
					v.stessaValuta = df_pesiBenchmark[,"Currency"] == valuta
					ok = v.stessaValuta &  v.senzaRischioValuta
					df_pesiBenchmark[ok,"E(r)"] =  df_pesiBenchmark[ok,"E(r)"] +  df_aspettativeFx[valuta,"Hedge costs"]
				}
			}
		} # if (length(v.valuteConHedge) > 0) 
		
		v.pesiBenchmark = df_pesiBenchmark[["Weight"]]
		names(v.pesiBenchmark) = df_pesiBenchmark[["Name"]]
		
		## verifica che abbiano tutti una serie storica
		
		isNotRiskFree <- df_pesiBenchmark[["Name"]] != l.datiRiskFree$idRiskFree
		
		senza.serie = !is.element(df_pesiBenchmark[["Name"]][isNotRiskFree],Z$serieDisponibili)
		if (any(senza.serie)) 
		{
			string = "Some instruments in the benchmark do not have a corresponding time series in the covariance matrix:\n"
			string = paste(string,paste(df_pesiBenchmark[["Name"]][senza.serie],sep="",collapse=", "),sep="")
			tkmessageBox(message=string,icon="error")
			exitWithError = 1
			save.image("error.RData")
			stop()
		} else {
			rm(senza.serie)
		}
		
		## verifica che abbiano tutti un peso
		senza.peso = is.na(v.pesiBenchmark)
		if (any(senza.peso)) {
			string = "Error: some weights in the benchmark are empty:\n"
			string = paste(string,paste(names(v.pesiBenchmark[senza.peso]),sep="",collapse=", "),sep="")
			tkmessageBox(message=string,icon="error")
			exitWithError = 1
			save.image("error.RData")
			stop()    
		} else {
			rm(senza.peso)
		}
		datiBenchmark$v.pesi <- v.pesiBenchmark
		datiBenchmark$v.pesiRiskAssets <- v.pesiBenchmark[isNotRiskFree]
		
		datiBenchmark$df.datiRiskAssets <- df_pesiBenchmark[isNotRiskFree,,drop=FALSE]
		rownames(datiBenchmark$df.datiRiskAssets) = datiBenchmark$df.datiRiskAssets[["Name"]]
		if (any(!isNotRiskFree)) datiBenchmark$conRiskFree = TRUE else datiBenchmark$conRiskFree = FALSE
		if (datiBenchmark$conRiskFree) datiBenchmark$df.pesoRiskFree <- df_pesiBenchmark[!isNotRiskFree,,drop=FALSE]
		rm(isNotRiskFree)
		datiBenchmark$df_pesiBenchmark <- df_pesiBenchmark
		rm(df_pesiBenchmark)
		
		## construct the covariance matrices to compute the tracking error
		## TE = Var(r_p) -2Cov(r_p,r_b) + Var(r_b) = x'Vx - 2 x'C1h + h'C2h
		## V ï¿½ calcolata piï¿½ sotto ed ï¿½ chiamata covMatrix.
		
		cMatrix = matriceTrasfLineare(Z$S,datiBenchmark$df.datiRiskAssets)
		tmp <- Z$S %*% t(cMatrix)
		datiBenchmark$C1 <- aMatrix %*% tmp
		datiBenchmark$C2 <- cMatrix %*% tmp
		
		## crea le matrici per l'esportazione in excel ed il calcolo del TE da Excel
		## dapprima la covarianza fra il portafoglio ed il benchmark
		tmp_nomi <- v.BenchmarkNamesClass_orig # datiBenchmark$df_pesiBenchmark_orig[,"Name"]
		tmp <- datiBenchmark$C1
		tmp <- t(aggiungiClassi(tmp_nomi,t(tmp)))
		datiBenchmark$C1toExport <- aggiungiClassi(riskFactors$classiConNomi,tmp)
		## poi la varianza del banchmark
		tmp <- datiBenchmark$C2
		datiBenchmark$C2toExport <-  
				aggiungiClassi(tmp_nomi,tmp,ancheColonne=TRUE)
		
		## rimuovi la classe risk free se disponibile
		datiBenchmark$C1toExport <- rimuoviRiskFreeClass(datiBenchmark$C1toExport)
		datiBenchmark$C2toExport <- rimuoviRiskFreeClass(datiBenchmark$C2toExport)    
		rm(tmp,tmp_nomi)
		
		## compute the expected return of the benchmark
		datiBenchmark$expectedReturn = sum(datiBenchmark$df.datiRiskAssets[,"E(r)"] * datiBenchmark$v.pesiRiskAssets)
		## add the riskFree return
		if (datiBenchmark$conRiskFree)
		{
			datiBenchmark$expectedReturn = datiBenchmark$expectedReturn + datiBenchmark$df.pesoRiskFree[1,"Weight"] * datiBenchmark$df.pesoRiskFree[1,"E(r)"]
		}
		
		## compute the variance and then the stdev of the benchmark
		datiBenchmark$stdev = (t(datiBenchmark$v.pesiRiskAssets) %*% datiBenchmark$C2 %*% datiBenchmark$v.pesiRiskAssets)[1,1]
		datiBenchmark$stdev = sqrt(datiBenchmark$stdev)
		
	} else {  ## senza pesi benchmark
		## verifica se il benchmark è in valuta di riferimento
		## se si non fare nulla, se no verifica se deve essere hedged
		## e se il caso modifica gli expected returns altrimenti considera i costi
		## dell'hedge
		
		valutaBenchmark <- df_infoSerieStoricaBenchmark[1,"Currency"]
		## verifica che la serie benchmark abbia la corrispondente serie storica
		if (!is.element(nomeBenchmark,Z$serieDisponibili)) {
			string = paste("The benchmark '",nomeBenchmark,"' does not have a corresponding time series in the covariance matrix.",sep="")
			tkmessageBox(message=string,icon="error")
			exitWithError = 1
			save.image("error.RData")
			stop()        
		}
		
		if (valutaBenchmark != valutaRiferimento)
		{
			## verifica che il rendimento atteso della valuta sia disponibile
			if (!is.element(valutaBenchmark,df_aspettativeFx[["Currency"]]))
			{
				string = paste("The currency of the benchmark '",nomeBenchmark,"' is missing.",sep="")
				tkmessageBox(message=string,icon="error")
				exitWithError = 1
				save.image("error.RData")
				stop()        
			}
			
			if (df_infoSerieStoricaBenchmark[1,"Currency risk"] == 1)
			{
				## verifica che la serie storica della valuta sia disponibile
				if (!is.element(valutaBenchmark,Z$serieDisponibili))
				{
					string = paste("The currency of the benchmark '",nomeBenchmark,"' does not have a corresponding time series in the covariance matrix.",sep="")
					tkmessageBox(message=string,icon="error")
					exitWithError = 1
					save.image("error.RData")
					stop()        
				}
				
				## Modifica il rendimento atteso del benchmark
				df_infoSerieStoricaBenchmark[1,"E(r)"] <- df_infoSerieStoricaBenchmark[1,"E(r)"] + df_aspettativeFx[[valutaBenchmark,"E(r)"]]
				
			} else {  # if (df_infoSerieStoricaBenchmark[1,"Currency risk"] == 1)
				## quindi il rischio cambio sul benchamrk è hedged
				## controlla che il rendimento hedge corrispondente alla valuta  ci sia
				if (is.na(df_aspettativeFx[valutaBenchmark,"Hedge costs"]))
				{
					string = paste("The benchmark '",nomeBenchmark,"' requires the 'Hedge costs' on the corresponding currency.",sep="")
					tkmessageBox(message=string,icon="error")
					exitWithError = 1
					save.image("error.RData")
					stop()        
				}
				## Modifica il rendimento atteso del benchmark
				df_infoSerieStoricaBenchmark[1,"E(r)"] <- df_infoSerieStoricaBenchmark[1,"E(r)"] + df_aspettativeFx[[valutaBenchmark,"Hedge costs"]]
			}   
		} ## if (valutaBenchmark != valutaRiferimento)
		datiBenchmark$v.pesiRiskAssets <- 1
		names(datiBenchmark$v.pesiRiskAssets) <- nomeBenchmark
		
		## costruisci le matrici di covarianza per il calcolo del tracking error
		## TE = Var(r_p) -2Cov(r_p,r_b) + Var(r_b) = x'Vx - 2 x'C1h + h'C2h
		## V ï¿½ calcolata piï¿½ sotto ed ï¿½ chiamata covMatrix.
		
		cMatrix = matriceTrasfLineare(Z$S,df_infoSerieStoricaBenchmark)        
		tmp <- Z$S %*% t(cMatrix)
		datiBenchmark$C1 <- aMatrix %*% tmp
		datiBenchmark$C2 <- cMatrix %*% tmp
		
		## crea le matrici per l'esportazione in excel ed il calcolo del TE da Excel
		## dapprima la covarianza fra il portafoglio ed il benchmark
		tmp <- datiBenchmark$C1
		datiBenchmark$C1toExport <- aggiungiClassi(riskFactors$classiConNomi,tmp)
		## poi la varianza del banchmark
		datiBenchmark$C2toExport <- datiBenchmark$C2
		datiBenchmark$C1toExport <- rimuoviRiskFreeClass(datiBenchmark$C1toExport)
		datiBenchmark$C2toExport <- rimuoviRiskFreeClass(datiBenchmark$C2toExport)  
		rm(tmp)
		## compute the expected return of the benchmark
		datiBenchmark$expectedReturn = df_infoSerieStoricaBenchmark[1,"E(r)"]
		
		## compute the variance and then the stdev of the benchmark
		datiBenchmark$stdev = sqrt(datiBenchmark$C2[1,1])    
		
	} ## senza pesi benchmark
} # if (conVincoloBenchmark)






# 3) costruisci le matrici dei vincoli
m.vincoliSemplici <- t(as.matrix(df_vincoliSemplici[,c("equals","alMin","alMax")]))
m.vincoliMultipli <- t(as.matrix(df.vincoliMultipli))
m.aspettative <- t(as.matrix(df_aspettative[,"E(r)",drop=FALSE]))
rm(df_aspettative,df_vincoliSemplici,df.vincoliMultipli)

## aggiungi i vincoli semplici definiti direttamente sulle classi
M <- trasformaVincoliClassi(df_vincoliSempliciClasse,df.classiConNomi)
## rimuovi il riskfree se necessario e modifica l.datiRiskFree$m.VincoliMultipli
if (!is.null(M) & l.datiRiskFree$conRiskFree)
{
	isRiskFree <- colnames(M) == l.datiRiskFree$idRiskFree
	tmp <- as.matrix(M[,isRiskFree,drop=FALSE])
	l.datiRiskFree$m.VincoliMultipli <- rbind(l.datiRiskFree$m.VincoliMultipli,tmp)
	M <- M[,!isRiskFree,drop=FALSE]
	rm(isRiskFree,tmp)
}
if (!is.null(M))
{
	if (ncol(M)>0) m.vincoliMultipli <- rbind(m.vincoliMultipli,M)
}
rm(M)

# le righe seguenti sono probabilmente da rimuovere
#L = ncol(m.aspettative)
#S_ = rep(1, L)





inizializzaVincoliSemplici <- function(m.vincoliSemplici)
{ 
	
	## verifica che tutti i nomi che determinano il tipo di vincolo siano stati
	## inseriti correttamente
	nrValidi = sum(c(rownames(m.vincoliSemplici)=="equals",
					rownames(m.vincoliSemplici)=="alMax",rownames(m.vincoliSemplici)=="alMin"))
	if (nrValidi != 3)
	{
		string = "Error: the types of the simple constraints do not correspond to\nequals, alMin, alMax!"
		tkmessageBox(message=string,icon="error")
		exitWithError = 1
		save.image("error.RData")
		stop()       
	}
	
	x = list()  
	x$m.vincoliSemplici = m.vincoliSemplici
	## determina il numero complessivo di restrizioni esatte, alMin e alMax
	## imposta le restrizioni sui singoli titoli
	x$vincoliUguale <- !is.na(m.vincoliSemplici["equals",])
	x$vincoliAlMin <- !is.na(m.vincoliSemplici["alMin",])
	x$vincoliAlMax <- !is.na(m.vincoliSemplici["alMax",])
	
	x$nrVincoliUguale = sum(x$vincoliUguale)
	x$nrVincoliAlMin = sum(x$vincoliAlMin)
	x$nrVincoliAlMax = sum(x$vincoliAlMax)
	x$nrVincoli = x$nrVincoliUguale +  x$nrVincoliAlMin + x$nrVincoliAlMax
	return(x)
}

inizializzaVincoliMultipli <- function(m.vincoliMultipli)
{
	## m.vincoliMultipli: la matrice di vincoli multipli dove le righe sono i vincoli
	## e le colonne sono gli strumenti. La prima colonna ï¿½ "Value", il valore del vincolo
	x = list()
	## verifica che ci siano ulteriori restrizioni
	x$nrVincoli = 0
	x$nrVincoliUguale = 0
	x$nrVincoliAlMax = 0
	x$nrVincoliAlMin = 0
	
	if (nrow(m.vincoliMultipli) > 0)
	{
		## estrai l'ultima riga del dataframe contenente i valori limite dei vincoli 
		x$valoreVincoli = m.vincoliMultipli[,"Value"]
		nonValidi <- is.na(x$valoreVincoli)
		if (any(nonValidi))
		{
			string = "Some multiple constraints have an irregular value:\nCheck the multiple constraints and the assigned values: "
			string = paste(string,paste((1:nrow(m.vincoliMultipli))[nonValidi],sep="",collapse=", "),sep="")
			tkmessageBox(message=string,icon="error")
			exitWithError = 1
			save.image("error.RData")
			stop()
		}
		
		x$m.vincoli <- m.vincoliMultipli[,colnames(m.vincoliMultipli) != "Value",drop=FALSE]
		if (any(is.na(as.vector(x$m.vincoli))))
		{
			string = "Some multiple constraints have an irregular value:\nCheck the multiple constraints and the assigned values."
			tkmessageBox(message=string,icon="error")
			exitWithError = 1
			save.image("error.RData")
			stop()    
		}
		
		if (ncol(x$m.vincoli) > 0)
		{
			x$nomiVincoli = rownames(x$m.vincoli)
			x$vincoliUguale = substr(x$nomiVincoli,1,6) == "equals"
			x$vincoliAlMax = substr(x$nomiVincoli,1,5) == "alMax"
			x$vincoliAlMin = substr(x$nomiVincoli,1,5) == "alMin"
			
			## verifica che tutti i nomi che determinano il tipo di vincolo siano stati
			## inseriti correttamente
			nrValidi = sum(c(x$vincoliUguale,x$vincoliAlMin,x$vincoliAlMax))
			if (nrValidi != nrow(x$m.vincoli))
			{
				string = "Error: the types of the multiple constraints do not correspond to\nequals, alMin, alMax!"
				tkmessageBox(message=string,icon="error")
				exitWithError = 1
				save.image("error.RData")
				stop()       
			}
			
			## calcola il numero di ulteriori restrizioni e la loro ripartizione fra
			## esatte, max e min
			x$nrVincoli = as.numeric(length(x$valoreVincoli))
			x$nrVincoliUguale = sum(x$vincoliUguale)
			x$nrVincoliAlMax = sum(x$vincoliAlMax)
			x$nrVincoliAlMin = sum(x$vincoliAlMin)                        
		}
	}
	return(x)
}

l.vS <- inizializzaVincoliSemplici(m.vincoliSemplici)
l.vM <-  inizializzaVincoliMultipli(m.vincoliMultipli)
rm(m.vincoliMultipli,m.vincoliSemplici)

setupAlgencanSenzaRiskFree <- function(l.vS,l.vM,m.aspettative,datiBenchmark)
{
	## setup se non c'ï¿½ il risk free 
	## A) Vincoli semplici
	## A1) i vincoli uguali vanno trasformati in C_j(x)=0 (vedi vincoli multipli)
	## A2) le disuguaglianze vanno lasciate: l[] <- alMin e u[] <- alMax
	
	## B) Vincoli multipli C_j(x) = 0
	## B1) i vincoli uguali vanno lasciati C_j(x) = 0
	## B2) i vincoli alMax vanno lasciati
	## B3) i vincoli alMin vanno girati di segno
	
	## C) vanno aggiunti i due vincoli sum(x)=1 e x'mu = k (rendimentoDesiderato)
	##    dove x sono i pesi e mu indica il vettore dei rendimenti attesi
	##
	## Al termine si avrï¿½ una matrice M contenenti i vincoli x'mu = k, 
	## sum(x)=1, i semplici uguale, i vincoli multipli uguale
	
	## Implementazione:
	nrPesi <- as.numeric(ncol(m.aspettative))
	## A1) i vincoli uguali vanno trasformati in C_j(x)=0 (vedi vincoli multipli)
	##     Le restrizioni vanno trasformate come Mx - b = C_j(x) = 0 dove M ï¿½
	##     una matrice con un solo 1 per riga nella posizione corrispondente.
	nrVincUgualeAlgencan = l.vS$nrVincoliUguale
	
	if (l.vS$nrVincoliUguale > 0)
	{
		M <- matrix(0,nrow=l.vS$nrVincoliUguale,ncol=nrPesi)
		## poni 1 dove necessario
		i <- matrix(c(1:l.vS$nrVincoliUguale,(1:nrPesi)[l.vS$vincoliUguale]),byrow=FALSE,ncol=2)
		M[i] <- 1
		rm(i)
		b = l.vS$m.vincoliSemplici["equals",l.vS$vincoliUguale]
	}
	
	
	## A2) le disuguaglianze vanno lasciate: l[] <- alMin e u[] <- alMax
	l_bounds <- rep(- 1.0e20,nrPesi)
	u_bounds <- rep(1.0e20,nrPesi)
	if (l.vS$nrVincoliAlMin>0)
	{
		l_bounds[l.vS$vincoliAlMin] <- l.vS$m.vincoliSemplici["alMin",l.vS$vincoliAlMin]
	}
	if (l.vS$nrVincoliAlMax>0)
	{
		u_bounds[l.vS$vincoliAlMax] <- l.vS$m.vincoliSemplici["alMax",l.vS$vincoliAlMax]
	}
	## B) Vincoli multipli C_j(x) = 0
	## B1) i vincoli uguali vanno lasciati C_j(x) = 0
	if (l.vM$nrVincoliUguale>0)
	{
		nrVincUgualeAlgencan = l.vS$nrVincoliUguale + l.vM$nrVincoliUguale
		M1 <- l.vM$m.vincoli[l.vM$vincoliUguale,,drop=FALSE]
		b1 <- l.vM$valoreVincoli[l.vM$vincoliUguale]
		if (exists("M",inherits = FALSE))
		{
			M <- rbind(M,M1)
			b <- c(b,b1)
		} else {
			M <- M1
			b <- b1
		}
		rm(b1,M1) 
	}
	## B2) i vincoli alMax vanno lasciati
	## B3) i vincoli alMin vanno girati di segno
	nrVincAlMaxAlgencan = l.vM$nrVincoliAlMax
	if (l.vM$nrVincoliAlMax > 0)
	{
		M1 <- l.vM$m.vincoli[l.vM$vincoliAlMax,,drop=FALSE]
		b1 <- l.vM$valoreVincoli[l.vM$vincoliAlMax]
	}
	if (l.vM$nrVincoliAlMin > 0)
	{
		nrVincAlMaxAlgencan = l.vM$nrVincoliAlMax + l.vM$nrVincoliAlMin
		if (exists("M1",inherits = FALSE))
		{
			M2 <- - l.vM$m.vincoli[l.vM$vincoliAlMin,,drop=FALSE]
			b2 <- - l.vM$valoreVincoli[l.vM$vincoliAlMin]
			M1 <- rbind(M1,M2)
			b1 <- c(b1,b2)
			rm(M2,b2)          
		} else {
			M1 <- - l.vM$m.vincoli[l.vM$vincoliAlMin,,drop=FALSE]
			b1 <- - l.vM$valoreVincoli[l.vM$vincoliAlMin]
		}
	}  
	
	## C) vanno aggiunti i due vincoli sum(x)=1 e x'mu = rendimentoDesiderato
	##    dove x sono i pesi e mu indica il vettore dei rendimenti attesi  
	## Aggiungi la restrizione sum(x)=1
	nrVincUgualeAlgencan = nrVincUgualeAlgencan + 1
	if (exists("M",inherits = FALSE))
	{
		M = rbind(rep(1.0,nrPesi),M)
		b = c(1.0,b)
	} else {
		M = matrix(rep(1.0,nrPesi), nrow=1)
		colnames(M) <- colnames(l.vS$m.vincoliSemplici)
		b = 1.0
	}
	
	## Aggiungi la restrizione x'mu = rendimento desiderato
	nrVincUgualeAlgencan = nrVincUgualeAlgencan + 1
	M = rbind(m.aspettative,M)
	b = c(0.0,b)
	rownames(M) = NULL
	
	## Aggiungi la restrizione sul benchmark
	if (datiBenchmark$conVincoloBenchmark)
	{
		nrVincAlMaxAlgencan = nrVincAlMaxAlgencan + 1
		#if (exists("b1",inherits=FALSE))
		#  {
		#    b1 = c(b1,datiBenchmark$trackErrorConstrSquared)  
		#  } else {
		#    b1 =  datiBenchmark$trackErrorConstrSquared
		#  }
	}
	
	x <- list()
	x$nrPesi <- nrPesi
	x$l_bounds <- l_bounds
	x$u_bounds <- u_bounds
	x$nrVincUgualeAlgencan <- nrVincUgualeAlgencan
	x$nrVincAlMaxAlgencan <- nrVincAlMaxAlgencan
	x$nrVincoliSempliciUguale <- l.vS$nrVincoliUguale
	x$nrVincoliMultipliUguale <- l.vM$nrVincoliUguale
	x$nrVincoliMultipliAlMax <- l.vM$nrVincoliAlMax
	x$nrVincoliMultipliAlMin <- l.vM$nrVincoliAlMin
	
	x$M <- M
	x$b <- b
	## aggiungi la matrice x$M1
	if (exists("M1",inherits=FALSE))
	{
		x$M1 <- M1
		x$b1 <- b1
	}
	
	x$rf = 0
	return(x)
}

setupAlgencanConRiskFree <- function(l.vS,l.vM,m.aspettative,l.datiRiskFree,datiBenchmark)
{ 
	
	## setup se c'ï¿½ il risk free 
	## A) Vincoli semplici
	## A1) i vincoli uguali vanno trasformati in C_j(x)=0 (vedi vincoli multipli).
	##     In particolare il vincolo sul risk free andrï¿½ espresso in termini degli
	##     altri coefficienti (utilizzo sum(x) = 1)  
	## A2) le disuguaglianze vanno lasciate: l[] <- alMin e u[] <- alMax tranne
	##     quella sul risk free che andrï¿½ trasformata
	
	## B) Vincoli multipli C_j(x) = 0
	## B1) i vincoli uguali vanno trasformati
	## B2) i vincoli alMax vanno trasformati
	## B3) i vincoli alMin vanno trasformati e girati di segno
	
	## C) va aggiunto il vincolo x'mu = rendimentoDesiderato che andrï¿½ trasformato
	##    per tener conto del risk free. mu indica il vettore dei rendimenti attesi
	##    Il vincolo sum(x) = 1 ï¿½ utilizzato: p + sum(x) = 1 dove p ï¿½ il peso del
	##    risk free (vedi Implementazione)
	
	## Implementazione:
	nrPesi <- as.numeric(ncol(m.aspettative))
	S_ = rep(1.0,nrPesi)
	nrVincUgualeAlgencan = 0
	nrVincAlMaxAlgencan = 0
	nrVincoliUgualeQuadSolve = 0
	
	## C) Il vincolo esatto mu'w = b (rendDesiderato)
	##    mu'[p,x] = b
	##    mu[1]*p + mu[-1]'x = b
	##    mu[1](1-S'x) + mu[-1]'x = b
	##    (-mu[1]S + mu[-1])'x + mu[1]  = b 
	##    (mu[-1] - mu[1]S)'x  = b - mu[1]
	##    C_j(x) = 0
	nrVincUgualeAlgencan = nrVincUgualeAlgencan + 1
	nrVincoliUgualeQuadSolve = nrVincoliUgualeQuadSolve + 1
	mu1 <- l.datiRiskFree$m.Aspettative[1,1] 
	b <- - mu1
	M <- matrix(as.vector(m.aspettative)-mu1*S_,nrow=1) 
	
	## A1) Vincoli semplici esatti risk free. p: peso risk free, x pesi componenti a rischio
	##     p = b, da cui essendo p = 1 - sum(x) => 1 - sum(x) = b
	##     Quest'ultimo vincolo andrï¿½ implementato come
	##                    S'x = 1-b  che sarï¿½ infine adattato  
	if (!is.na(l.datiRiskFree$m.VincoliSemplici["equals",1]))## vincolo esatto
	## risk free != NA
	{
		nrVincUgualeAlgencan = nrVincUgualeAlgencan + 1
		nrVincoliUgualeQuadSolve = nrVincoliUgualeQuadSolve + 1
		b <- c(b,1 - l.datiRiskFree$m.VincoliSemplici["equals",1])
		M <- rbind(M,matrix(S_,nrow=1))
	}
	
	
	## A1 bis) Vincoli semplici esatti sulle componenti a rischio
	##         Le restrizioni vanno trasformate come Mx - b = C_j(x) = 0 dove M ï¿½
	##         una matrice con un solo 1 per riga nella posizione corrispondente.
	if (l.vS$nrVincoliUguale > 0)
	{
		nrVincUgualeAlgencan = nrVincUgualeAlgencan + l.vS$nrVincoliUguale
		nrVincoliUgualeQuadSolve = nrVincoliUgualeQuadSolve + l.vS$nrVincoliUguale
		M1 <- matrix(0,nrow=l.vS$nrVincoliUguale,ncol=nrPesi)
		## poni 1 dove necessario
		i <- matrix(c(1:l.vS$nrVincoliUguale,(1:nrPesi)[l.vS$vincoliUguale]),ncol=2,byrow=FALSE)
		M1[i] <- 1
		M = rbind(M,M1)
		b = c(b,l.vS$m.vincoliSemplici["equals",l.vS$vincoliUguale])
		rm(M1,i)
	}
	
	## A2) Vincoli semplici alMin e alMax
	##     p <= b, da cui 1 - sum(x) <= b e quindi -sum(x) <= b - 1, C_j(x) <=0
	##     p >= b, da cui 1 - sum(x) >= b e quindi -1 + sum(x) <= - b, C_j(x) <=0
	if (!is.na(l.datiRiskFree$m.VincoliSemplici["alMax",1]))
	{
		nrVincAlMaxAlgencan = nrVincAlMaxAlgencan + 1
		b1 = l.datiRiskFree$m.VincoliSemplici["alMax",1] - 1
		M1 = matrix(-S_,nrow=1)
	}
	if (!is.na(l.datiRiskFree$m.VincoliSemplici["alMin",1]))
	{
		nrVincAlMaxAlgencan = nrVincAlMaxAlgencan + 1
		if (exists("M1", inherits = FALSE))
		{
			b1 = c(b1,1-l.datiRiskFree$m.VincoliSemplici["alMin",1])
			M1 = rbind(M1,matrix(S_,nrow=1))          
		} else {
			b1 = 1-l.datiRiskFree$m.VincoliSemplici["alMin",1]
			M1 = matrix(S_,nrow=1)           
		}
	}
	
	## A2 bis) Vincoli semplici alMin e alMax sulle componenti a rischio
	##    l[] = b_low e u[] = b_up
	##
	l_bounds <- rep(- 1.0e20,nrPesi)
	u_bounds <- rep(1.0e20,nrPesi)
	if (l.vS$nrVincoliAlMin>0)
	{
		l_bounds[l.vS$vincoliAlMin] <- l.vS$m.vincoliSemplici["alMin",l.vS$vincoliAlMin]
	}
	if (l.vS$nrVincoliAlMax>0)
	{
		u_bounds[l.vS$vincoliAlMax] <- l.vS$m.vincoliSemplici["alMax",l.vS$vincoliAlMax]
		# qui mettere le restrizioni per quadSolve
	}
	
	## B1) Vincoli multipli esatti
	##     Sia w = (p,x), il vincolo ï¿½ Aw = b, o Aw > b o Aw < b.
	##     [a,C]w = b
	##     ap + Cx = b
	##     a(1-sum(x)) + Cx = b
	##     a - a sum(x) + Cx = b
	##     -a S'x + Cx + a - b = 0
	##     (C-aS')x + (a - b) = 0
	##     Dx + c = 0 dove D = C-aS' e c = a - b
	##     C_j(x) = 0
	
	if (l.vM$nrVincoliUguale > 0)
	{
		nrVincUgualeAlgencan = nrVincUgualeAlgencan + l.vM$nrVincoliUguale
		M2 <- l.vM$m.vincoli[l.vM$vincoliUguale,,drop=FALSE] - l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliUguale,,drop=FALSE]%*%matrix(S_,nrow=1)
		b2 <- l.vM$valoreVincoli[l.vM$vincoliUguale] - l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliUguale,]      
		M = rbind(M,M2)
		b = c(b,b2)
		rm(b2,M2)
	}
	
	## B2) Nel caso di una disuguaglianza [a,C]w <= b avremo 
	##     Dx + c <= 0 dove D = C-aS' e c = a - b
	##     C_j(x) <= 0
	nrVincAlMaxAlgencan = nrVincAlMaxAlgencan + l.vM$nrVincoliAlMax
	if (l.vM$nrVincoliAlMax > 0)
	{
		if (exists("M1",inherits=FALSE))
		{
			M1 <- rbind(M1,l.vM$m.vincoli[l.vM$vincoliAlMax,,drop=FALSE] - 
							l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMax,,drop=FALSE]%*%matrix(S_,nrow=1))
			b1 <- c(b1,l.vM$valoreVincoli[l.vM$vincoliAlMax] - 
							l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMax,])
		} else {
			M1 <- l.vM$m.vincoli[l.vM$vincoliAlMax,,drop=FALSE] - l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMax,,drop=FALSE]%*%matrix(S_,nrow=1)
			b1 <- l.vM$valoreVincoli[l.vM$vincoliAlMax] - l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMax,]      
		}
	}  
	
	
	## B3) Nel caso di una disuguaglianza [a,C]w >= b avremo 
	##     Dx + c <= 0 dove D = aS'-C e c = - (a - b)
	##     C_j(x) <= 0 
	if (l.vM$nrVincoliAlMin > 0)
	{
		nrVincAlMaxAlgencan = nrVincAlMaxAlgencan + l.vM$nrVincoliAlMin
		M2 <- - l.vM$m.vincoli[l.vM$vincoliAlMin,,drop=FALSE] + l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMin,,drop=FALSE]%*%matrix(S_,nrow=1)
		b2 <- - l.vM$valoreVincoli[l.vM$vincoliAlMin] + l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMin,]
		if (exists("M1",inherits=FALSE))
		{
			M1 <- rbind(M1,M2)
			b1 <- c(b1,b2)
		} else {
			M1 <- M2
			b1 <- b2      
		}
		rm(M2,b2)
	}
	
	## Aggiungi la restrizione sul benchmark
	if (datiBenchmark$conVincoloBenchmark)
	{
		nrVincAlMaxAlgencan = nrVincAlMaxAlgencan + 1
	}    
	
	x <- list()
	x$nrPesi <- nrPesi
	x$l_bounds <- l_bounds
	x$u_bounds <- u_bounds
	x$nrVincUgualeAlgencan <- nrVincUgualeAlgencan
	x$nrVincAlMaxAlgencan <- nrVincAlMaxAlgencan
	x$nrVincoliSempliciUguale <- l.vS$nrVincoliUguale
	x$nrVincoliMultipliUguale <- l.vM$nrVincoliUguale
	x$nrVincoliMultipliAlMax <- l.vM$nrVincoliAlMax
	x$nrVincoliMultipliAlMin <- l.vM$nrVincoliAlMin
	
	x$M <- M
	x$b <- b
	## aggiungi la matrice x$M1
	if (exists("M1",inherits=FALSE))
	{
		x$M1 <- M1
		x$b1 <- b1
	}
	
	x$rf = mu1
	
	return(x)
}




if (!l.datiRiskFree$conRiskFree)
{
	datiVincoli <- setupAlgencanSenzaRiskFree(l.vS,l.vM,m.aspettative,datiBenchmark)
} else {
	datiVincoli <- setupAlgencanConRiskFree(l.vS,l.vM,m.aspettative,l.datiRiskFree,datiBenchmark)
}



setupQuadProgSenzaRiskFree <- function(l.vS,l.vM,m.aspettative)
{
	## setup se non c'ï¿½ il risk free 
	## A) Vincoli semplici
	## A1) i vincoli uguali vanno trasformati in C_j(x)=0 (vedi vincoli multipli)
	## A2) le disuguaglianze vanno lasciate: l[] <- alMin e u[] <- alMax
	
	## B) Vincoli multipli C_j(x) = 0
	## B1) i vincoli uguali vanno lasciati C_j(x) = 0
	## B2) i vincoli alMax vanno lasciati
	## B3) i vincoli alMin vanno girati di segno
	
	## C) vanno aggiunti i due vincoli sum(x)=1 e x'mu = k (rendimentoDesiderato)
	##    dove x sono i pesi e mu indica il vettore dei rendimenti attesi
	##
	## Al termine si avrï¿½ una matrice M contenenti i vincoli x'mu = k, 
	## sum(x)=1, i semplici uguale, i vincoli multipli uguale
	
	
	## Implementazione:
	nrPesi <- as.numeric(ncol(m.aspettative))
	nrVincAlMax = 0
	
	## A1) i vincoli uguali vanno trasformati in C_j(x)=0 (vedi vincoli multipli)
	##     Le restrizioni vanno trasformate come Mx - b = C_j(x) = 0 dove M ï¿½
	##     una matrice con un solo 1 per riga nella posizione corrispondente.
	nrVincUguale = l.vS$nrVincoliUguale
	
	if (l.vS$nrVincoliUguale > 0)
	{
		M <- matrix(0,nrow=l.vS$nrVincoliUguale,ncol=nrPesi)
		## poni 1 dove necessario
		i <- matrix(c(1:l.vS$nrVincoliUguale,(1:nrPesi)[l.vS$vincoliUguale]),ncol=2,byrow=FALSE)
		M[i] <- 1
		rm(i)
		b = l.vS$m.vincoliSemplici["equals",l.vS$vincoliUguale]
	}
	
	## A2) le disuguaglianze vanno lasciate: l[] <- alMin e u[] <- alMax
	if (l.vS$nrVincoliAlMin>0)
	{
		nrVincAlMax = nrVincAlMax + sum(l.vS$vincoliAlMin)
		b1 <- -l.vS$m.vincoliSemplici["alMin",l.vS$vincoliAlMin]
		# qui mettere le restrizioni per quadSolve tenendo conto che stiamo utilizzando la specificazione di algencan Ax <= b
		M1 = matrix(0,nrow=l.vS$nrVincoliAlMin,ncol=nrPesi)
		id <- matrix(c(1:l.vS$nrVincoliAlMin,(1:nrPesi)[l.vS$vincoliAlMin]),ncol=2)
		M1[id] <- -1        
		rm(id)
	}
	if (l.vS$nrVincoliAlMax>0)
	{
		nrVincAlMax = nrVincAlMax + sum(l.vS$vincoliAlMax)
		b2 <- l.vS$m.vincoliSemplici["alMax",l.vS$vincoliAlMax]
		# qui mettere le restrizioni per quadSolve  tenendo conto che stiamo utilizzando la specificazione di algencan Ax <= b
		M2 = matrix(0,nrow=l.vS$nrVincoliAlMax,ncol=nrPesi)
		id <- matrix(c(1:l.vS$nrVincoliAlMax,(1:nrPesi)[l.vS$vincoliAlMax]),ncol=2)
		M2[id] <- 1
		if (exists("M1", inherits = FALSE))
		{
			b1 = c(b1,b2)
			M1 = rbind(M1,M2)          
		} else {
			b1 = b2
			M1 = M2         
		}
		rm(b2,id,M2)
	}  
	
	## B) Vincoli multipli C_j(x) = 0
	## B1) i vincoli uguali vanno lasciati C_j(x) = 0
	if (l.vM$nrVincoliUguale>0)
	{
		nrVincUguale = l.vS$nrVincoliUguale + l.vM$nrVincoliUguale
		M1 <- l.vM$m.vincoli[l.vM$vincoliUguale,,drop=FALSE]
		b1 <- l.vM$valoreVincoli[l.vM$vincoliUguale]
		if (exists("M",inherits = FALSE))
		{
			M <- rbind(M,M1)
			b <- c(b,b1)
		} else {
			M <- M1
			b <- b1
		}
		rm(b1,M1) 
	}
	## B2) i vincoli alMax vanno lasciati
	## B3) i vincoli alMin vanno girati di segno
	nrVincAlMax = nrVincAlMax + l.vM$nrVincoliAlMax
	if (l.vM$nrVincoliAlMax > 0)
	{
		M1 <- l.vM$m.vincoli[l.vM$vincoliAlMax,,drop=FALSE]
		b1 <- l.vM$valoreVincoli[l.vM$vincoliAlMax]
	}
	if (l.vM$nrVincoliAlMin > 0)
	{
		nrVincAlMax = nrVincAlMax + l.vM$nrVincoliAlMin
		if (exists("M1",inherits = FALSE))
		{
			M2 <- - l.vM$m.vincoli[l.vM$vincoliAlMin,,drop=FALSE]
			b2 <- - l.vM$valoreVincoli[l.vM$vincoliAlMin]
			M1 <- rbind(M1,M2)
			b1 <- c(b1,b2)
			rm(M2,b2)          
		} else {
			M1 <- - l.vM$m.vincoli[l.vM$vincoliAlMin,,drop=FALSE]
			b1 <- - l.vM$valoreVincoli[l.vM$vincoliAlMin]
		}
	}  
	
	## C) vanno aggiunti i due vincoli sum(x)=1 e x'mu = rendimentoDesiderato
	##    dove x sono i pesi e mu indica il vettore dei rendimenti attesi  
	## Aggiungi la restrizione sum(x)=1
	nrVincUguale = nrVincUguale + 1
	if (exists("M",inherits = FALSE))
	{
		M = rbind(rep(1.0,nrPesi),M)
		b = c(1.0,b)
	} else {
		M = matrix(rep(1.0,nrPesi), nrow=1)
		colnames(M) <- colnames(l.vS$m.vincoliSemplici)
		b = 1.0
	}
	
	## Aggiungi la restrizione x'mu = rendimento desiderato
	nrVincUguale = nrVincUguale + 1
	M = rbind(m.aspettative,M)
	b = c(0.0,b)
	rownames(M) = NULL
	
	
	x <- list()
	x$nrPesi <- nrPesi
	x$nrVincUguale <- nrVincUguale
	x$nrVincAlMax <- nrVincAlMax
	
	x$M <- M
	x$b <- b
	## aggiungi la matrice x$M1
	if (exists("M1",inherits=FALSE))
	{
		x$M1 <- M1
		x$b1 <- b1
	}
	
	x$rf = 0
	return(x)
}



setupQuadProgConRiskFree <- function(l.vS,l.vM,m.aspettative,l.datiRiskFree)
{ 
	
	## setup se c'ï¿½ il risk free 
	## A) Vincoli semplici
	## A1) i vincoli uguali vanno trasformati in C_j(x)=0 (vedi vincoli multipli).
	##     In particolare il vincolo sul risk free andrï¿½ espresso in termini degli
	##     altri coefficienti (utilizzo sum(x) = 1)  
	## A2) le disuguaglianze vanno lasciate: l[] <- alMin e u[] <- alMax tranne
	##     quella sul risk free che andrï¿½ trasformata
	
	## B) Vincoli multipli C_j(x) = 0
	## B1) i vincoli uguali vanno trasformati
	## B2) i vincoli alMax vanno trasformati
	## B3) i vincoli alMin vanno trasformati e girati di segno
	
	## C) va aggiunto il vincolo x'mu = rendimentoDesiderato che andrï¿½ trasformato
	##    per tener conto del risk free. mu indica il vettore dei rendimenti attesi
	##    Il vincolo sum(x) = 1 ï¿½ utilizzato: p + sum(x) = 1 dove p ï¿½ il peso del
	##    risk free (vedi Implementazione)
	
	
	## Implementazione:
	nrPesi <- as.numeric(ncol(m.aspettative))
	S_ = rep(1.0,nrPesi)
	nrVincUguale = 0
	nrVincAlMax = 0
	
	## C) Il vincolo esatto mu'w = b (rendDesiderato)
	##    mu'[p,x] = b
	##    mu[1]*p + mu[-1]'x = b
	##    mu[1](1-S'x) + mu[-1]'x = b
	##    (-mu[1]S + mu[-1])'x + mu[1]  = b 
	##    (mu[-1] - mu[1]S)'x  = b - mu[1]
	##    C_j(x) = 0
	nrVincUguale = nrVincUguale + 1
	
	mu1 <- l.datiRiskFree$m.Aspettative[1,1] 
	b <- - mu1
	M <- matrix(as.vector(m.aspettative)-mu1*S_,nrow=1) 
	
	## A1) Vincoli semplici esatti risk free. p: peso risk free, x pesi componenti a rischio
	##     p = b, da cui essendo p = 1 - sum(x) => 1 - sum(x) = b
	##     Quest'ultimo vincolo andrï¿½ implementato come
	##                    S'x = 1-b  che sarï¿½ infine adattato  
	if (!is.na(l.datiRiskFree$m.VincoliSemplici["equals",1]))## vincolo esatto
	## risk free != NA
	{
		nrVincUguale = nrVincUguale+ 1
		b <- c(b,1 - l.datiRiskFree$m.VincoliSemplici["equals",1])
		M <- rbind(M,matrix(S_,nrow=1))
	}
	
	
	## A1 bis) Vincoli semplici esatti sulle componenti a rischio
	##         Le restrizioni vanno trasformate come Mx - b = C_j(x) = 0 dove M ï¿½
	##         una matrice con un solo 1 per riga nella posizione corrispondente.
	if (l.vS$nrVincoliUguale > 0)
	{
		nrVincUguale = nrVincUguale + l.vS$nrVincoliUguale
		M1 <- matrix(0,nrow=l.vS$nrVincoliUguale,ncol=nrPesi)
		## poni 1 dove necessario
		i <- matrix(c(1:l.vS$nrVincoliUguale,(1:nrPesi)[l.vS$vincoliUguale]),ncol=2,byrow=FALSE)
		M1[i] <- 1
		M = rbind(M,M1)
		b = c(b,l.vS$m.vincoliSemplici["equals",l.vS$vincoliUguale])
		rm(M1,i)
	}
	
	## A2) Vincoli semplici alMin e alMax
	##     p <= b, da cui 1 - sum(x) <= b e quindi -sum(x) <= b - 1, C_j(x) <=0
	##     p >= b, da cui 1 - sum(x) >= b e quindi -1 + sum(x) <= - b, C_j(x) <=0
	if (!is.na(l.datiRiskFree$m.VincoliSemplici["alMax",1]))
	{
		nrVincAlMax = nrVincAlMax + 1
		b1 = l.datiRiskFree$m.VincoliSemplici["alMax",1] - 1
		M1 = matrix(-S_,nrow=1)
	}
	if (!is.na(l.datiRiskFree$m.VincoliSemplici["alMin",1]))
	{
		nrVincAlMax = nrVincAlMax + 1
		if (exists("M1", inherits = FALSE))
		{
			b1 = c(b1,1-l.datiRiskFree$m.VincoliSemplici["alMin",1])
			M1 = rbind(M1,matrix(S_,nrow=1))          
		} else {
			b1 = 1-l.datiRiskFree$m.VincoliSemplici["alMin",1]
			M1 = matrix(S_,nrow=1)           
		}
	}
	
	## A2 bis) Vincoli semplici alMin e alMax sulle componenti a rischio
	
	if (l.vS$nrVincoliAlMin>0)
	{
		nrVincAlMax = nrVincAlMax + sum(l.vS$vincoliAlMin)
		b2 <- l.vS$m.vincoliSemplici["alMin",l.vS$vincoliAlMin]
		# qui mettere le restrizioni per quadSolve tenendo conto che stiamo utilizzando la specificazione di algencan Ax <= b
		M2 = matrix(0,nrow=l.vS$nrVincoliAlMin,ncol=nrPesi)
		id <- matrix(c(1:l.vS$nrVincoliAlMin,(1:nrPesi)[l.vS$vincoliAlMin]),ncol=2)
		M2[id] <- 1
		if (exists("M1", inherits = FALSE))
		{
			b1 = c(b1,-b2)
			M1 = rbind(M1,-M2)          
		} else {
			b1 = -b2
			M1 = -M2         
		}
		rm(b2,id,M2)
	}
	if (l.vS$nrVincoliAlMax>0)     # vincoli Ax <= l.vS$m.vincoliSemplici["alMax",l.vS$vincoliAlMax]
	{
		nrVincAlMax = nrVincAlMax + sum(l.vS$vincoliAlMax)
		b2 <- l.vS$m.vincoliSemplici["alMax",l.vS$vincoliAlMax]
		# qui mettere le restrizioni per quadSolve  tenendo conto che stiamo utilizzando la specificazione di algencan Ax <= b
		M2 = matrix(0,nrow=l.vS$nrVincoliAlMax,ncol=nrPesi)
		id <- matrix(c(1:l.vS$nrVincoliAlMax,(1:nrPesi)[l.vS$vincoliAlMax]),ncol=2)
		M2[id] <- 1
		if (exists("M1", inherits = FALSE))
		{
			b1 = c(b1,b2)
			M1 = rbind(M1,M2)          
		} else {
			b1 = b2
			M1 = M2         
		}
		rm(b2,id,M2)
	}
	
	## B1) Vincoli multipli esatti
	##     Sia w = (p,x), il vincolo ï¿½ Aw = b, o Aw > b o Aw < b.
	##     [a,C]w = b
	##     ap + Cx = b
	##     a(1-sum(x)) + Cx = b
	##     a - a sum(x) + Cx = b
	##     -a S'x + Cx + a - b = 0
	##     (C-aS')x + (a - b) = 0
	##     Dx + c = 0 dove D = C-aS' e c = a - b
	##     C_j(x) = 0
	
	if (l.vM$nrVincoliUguale > 0)
	{
		nrVincUguale = nrVincUguale + l.vM$nrVincoliUguale
		M2 <- l.vM$m.vincoli[l.vM$vincoliUguale,,drop=FALSE] - l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliUguale,,drop=FALSE]%*%matrix(S_,nrow=1)
		b2 <- l.vM$valoreVincoli[l.vM$vincoliUguale] - l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliUguale,]      
		M = rbind(M,M2)
		b = c(b,b2)
		rm(b2,M2)
	}
	
	## B2) Nel caso di una disuguaglianza [a,C]w <= b avremo 
	##     Dx + c <= 0 dove D = C-aS' e c = a - b
	##     C_j(x) <= 0
	nrVincAlMax = nrVincAlMax + l.vM$nrVincoliAlMax
	if (l.vM$nrVincoliAlMax > 0)
	{
		if (exists("M1",inherits=FALSE))
		{
			M1 <- rbind(M1,l.vM$m.vincoli[l.vM$vincoliAlMax,,drop=FALSE] - 
							l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMax,,drop=FALSE]%*%matrix(S_,nrow=1))
			b1 <- c(b1,l.vM$valoreVincoli[l.vM$vincoliAlMax] - 
							l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMax,])
		} else {
			M1 <- l.vM$m.vincoli[l.vM$vincoliAlMax,,drop=FALSE] - l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMax,,drop=FALSE]%*%matrix(S_,nrow=1)
			b1 <- l.vM$valoreVincoli[l.vM$vincoliAlMax] - l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMax,]      
		}
	}  
	
	
	## B3) Nel caso di una disuguaglianza [a,C]w >= b avremo 
	##     Dx + c <= 0 dove D = aS'-C e c = - (a - b)
	##     C_j(x) <= 0 
	if (l.vM$nrVincoliAlMin > 0)
	{
		nrVincAlMax = nrVincAlMax + l.vM$nrVincoliAlMin
		M2 <- - l.vM$m.vincoli[l.vM$vincoliAlMin,,drop=FALSE] + l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMin,,drop=FALSE]%*%matrix(S_,nrow=1)
		b2 <- - l.vM$valoreVincoli[l.vM$vincoliAlMin] + l.datiRiskFree$m.VincoliMultipli[l.vM$vincoliAlMin,]
		if (exists("M1",inherits=FALSE))
		{
			M1 <- rbind(M1,M2)
			b1 <- c(b1,b2)
		} else {
			M1 <- M2
			b1 <- b2      
		}
		rm(M2,b2)
	}
	
	x <- list()
	x$nrPesi <- nrPesi
	x$nrVincUguale <- nrVincUguale
	x$nrVincAlMax <- nrVincAlMax
	
	x$M <- M
	x$b <- b
	## aggiungi la matrice x$M1
	if (exists("M1",inherits=FALSE))
	{
		x$M1 <- M1
		x$b1 <- b1
	}
	
	x$rf = mu1
	
	return(x)
}

if (!l.datiRiskFree$conRiskFree)
{
	datiVincoliQuadProg <- setupQuadProgSenzaRiskFree(l.vS,l.vM,m.aspettative)
} else {
	datiVincoliQuadProg <- setupQuadProgConRiskFree(l.vS,l.vM,m.aspettative,l.datiRiskFree)
}


rm(l.vS,l.vM) 
rm(vech)

## perform a first optimisation without tracking error constraints in order to assess convergence of the
## simplest optmisation problem

ottimizSenzaBench <- list()
ottimizConBench <- list()
r.range = seq(rendimentoMinimo,rendimentoMassimo,length.out=nrOttimizzazioni)
nbOptimizations = length(r.range)


## create the P matrix containing the optimal portfolio weights. The columns of
## the matrix contain the optimal weights given the desired level of expected return.
ottimizSenzaBench$P = matrix(, ncol = nbOptimizations, nrow = datiVincoliQuadProg$nrPesi)
dimnames(ottimizSenzaBench$P) <- list(colnames(m.aspettative),paste("E[R] = ",round(reportingScaleFactor*r.range*100,digits=2),"%",sep=""))
ottimizConBench$P = ottimizSenzaBench$P


## crea il vettore contenente il messaggio d'errore
ottimizSenzaBench$message.v <- rep("",nbOptimizations)
ottimizConBench$message.v <- rep("",nbOptimizations)

## crea la matrice optimalStdev contenente la varianza minima per dato rendimento
ottimizSenzaBench$optimalStdev <- matrix(,nrow=nbOptimizations,ncol=2)
colnames(ottimizSenzaBench$optimalStdev) <- c("E(r)","Portfolio stdev")
ottimizSenzaBench$optimalStdev[,"E(r)"] <- r.range
rownames(ottimizSenzaBench$optimalStdev) <- as.character(1:nrow(ottimizSenzaBench$optimalStdev))
ottimizConBench$optimalStdev <- ottimizSenzaBench$optimalStdev
ottimizConBench$optimalStdev <- cbind(ottimizConBench$optimalStdev,"Tracking Error"=ottimizSenzaBench$optimalStdev[,"Portfolio stdev"])

## crea il vettore contenente vero/falso a seconda della convergenza del risolutore
ottimizSenzaBench$feasible = rep(FALSE,nbOptimizations)
ottimizConBench$feasible = rep(FALSE,nbOptimizations)
ottimizConBench$inform = vector(mode = "integer", length = nbOptimizations)

b = datiVincoliQuadProg$b
A <- t(datiVincoliQuadProg$M)
if (is.element("b1",names(datiVincoliQuadProg)))
{
	b <- c(b,datiVincoliQuadProg$b1)   
	A <- cbind(A,t(datiVincoliQuadProg$M1))
}


index = 1
for (desired.r in r.range) 
{
	b[1] <- desired.r - datiVincoli$rf
	solution <- try(solve.QP(covMatrix, rep(0,datiVincoli$nrPesi), -A, -b, meq=datiVincoliQuadProg$nrVincUguale),silent=TRUE)
	
	## verifica che la soluzione esista
	if (class(solution)=="try-error") {
		errorMessage <- geterrmessage()
		ottimizSenzaBench$feasible[index] = FALSE
		if (length(grep("constraints are inconsistent",errorMessage))>0) {
			ottimizSenzaBench$message.v[index] <- paste("The linear constraints are too binding. The problem does not admit solutions\nfor a portfolio expected return of ",desired.r*reportingScaleFactor,".",sep="")
			index <- index + 1
			next
		}
		if (length(grep("matrix D in quadratic",errorMessage))>0) {
			tkmessageBox(message="The covariance matrix is singular.",icon="error")
			ottimizSenzaBench$message.v <- rep("The covariance matrix is singular.",nbOptimizations)
			ottimizSenzaBench$feasible <- rep(FALSE,nbOptimizations)
			exitWithError <- 1
			save.image("error.RData")
			break
		} else {
			ottimizSenzaBench$message.v <- rep("The covariance matrix is singular.",nbOptimizations)
			ottimizSenzaBench$feasible[index] <- FALSE
			ottimizSenzaBench$message.v[index] <- errorMessage
			next       
		}
	} else {
		ottimizSenzaBench$feasible[index] <- TRUE
		ottimizSenzaBench$P[,index] <- solution$solution
		ottimizSenzaBench$optimalStdev[index,"Portfolio stdev"] = sqrt(2*solution$value)
	}
	index <- index + 1
}

if (l.datiRiskFree$conRiskFree)
{
	# compute the risk free weight as 1 - sum(risky weights)
	tmp = apply(X=ottimizSenzaBench$P,MARGIN=2,FUN=sum) # the risky weights
	ottimizSenzaBench$P = rbind(1-tmp,ottimizSenzaBench$P) # add the row of the risk free weight to the matrix of results
	rownames(ottimizSenzaBench$P)[1] <- l.datiRiskFree$idRiskFree
	## compute the sharpe ratio
	ottimizSenzaBench$optimalStdev <- cbind(ottimizSenzaBench$optimalStdev,"Sharpe Ratio"=rep(NA_real_,nrow(ottimizSenzaBench$optimalStdev)))
	ottimizSenzaBench$optimalStdev[ottimizSenzaBench$feasible,"Sharpe Ratio"] = 
			(ottimizSenzaBench$optimalStdev[ottimizSenzaBench$feasible,"E(r)"]-l.datiRiskFree$m.Aspettative[l.datiRiskFree$idRiskFree,"E(r)"]) /
			ottimizSenzaBench$optimalStdev[ottimizSenzaBench$feasible,"Portfolio stdev"]
	rm(tmp)
}

# adjust the output's scale in order to satisfy the reporting horizon
ottimizSenzaBench$optimalStdev[,"E(r)"] = reportingScaleFactor * ottimizSenzaBench$optimalStdev[,"E(r)"]
ottimizSenzaBench$optimalStdev[ottimizSenzaBench$feasible,"Portfolio stdev"] = sqrt(reportingScaleFactor) *
		ottimizSenzaBench$optimalStdev[ottimizSenzaBench$feasible,"Portfolio stdev"]
if (l.datiRiskFree$conRiskFree)
{
	ottimizSenzaBench$optimalStdev[ottimizSenzaBench$feasible,"Sharpe Ratio"] =  sqrt(reportingScaleFactor) *
			ottimizSenzaBench$optimalStdev[ottimizSenzaBench$feasible,"Sharpe Ratio"]
}
rm(A,b,index,solution)


if (any(ottimizSenzaBench$feasible))
{
	## Load the problem definition file
	# source("C:/R/r-source/markowitz.r")
	source(paste(rSourceDirectory,"/markowitz",markowitzVersion,".r",sep=""))
	
#   Load the solver wrapper
	RHome <- R.home()
	## dyn.load("C:/progra~1/R/R-2.6.0/bin/algencan.so.dll")
	## dyn.load(paste(RHome,"\\bin\\algencan.so.dll",sep=""))
	dyn.load(paste(rSourceDirectory,"\\algencan.so.dll",sep=""))
	rimuoviAlgencanOutput()
	index = 1
	for (desired.r in r.range)
	{
		if (ottimizSenzaBench$feasible[index])
		{
			datiVincoli$b[1] = desired.r - datiVincoli$rf
#   Call the solver
			.Call("algencan",body(evalf),body(evalg),body(evalh),body(evalc),
					body(evaljac),body(evalhc),body(evalhlp),body(inip),body(endp),
					body(param),sys.frame(0))
			ottimizConBench$inform[index] = inform
			ottimizConBench$message.v[index] = AlgencanErrorString(inform)
			ottimizConBench$feasible[index] <- (if (inform==0) TRUE else FALSE)
			file.rename(from="algencan.out",to=paste("myOut_",index,".txt",sep=""))
			
			if (ottimizConBench$feasible[index])
			{
				ottimizConBench$P[,index] <- x
				ottimizConBench$optimalStdev[index,"Portfolio stdev"] = sqrt(f)
			}
			if (datiBenchmark$conVincoloBenchmark)
			{
				## ind = datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan
				## ottimizConBench$optimalStdev[index,"Tracking Error"] = evalc(n,x,ind,c,flag) + 10000*datiBenchmark$trackErrorConstrSquared
				ottimizConBench$optimalStdev[index,"Tracking Error"] <- (x %*% (covMatrix %*% x) -2 * t(x)%*%(datiBenchmark$C1%*%datiBenchmark$v.pesiRiskAssets) + datiBenchmark$v.pesiRiskAssets %*% (datiBenchmark$C2%*%datiBenchmark$v.pesiRiskAssets))[1,1]      
				ottimizConBench$optimalStdev[index,"Tracking Error"] <- sqrt(ottimizConBench$optimalStdev[index,"Tracking Error"])
			}
		}
		index <- index + 1
	}
	## add the E(r) of the benchmark, its volatility and the Information Ratio    
	ottimizConBench$optimalStdev <- cbind(ottimizConBench$optimalStdev,
			"Information Ratio"=rep(NA_real_,nrow(ottimizConBench$optimalStdev)))
	
	if (conVincoloBenchmark) {
		ottimizConBench$optimalStdev <- cbind(ottimizConBench$optimalStdev,
				"Bench. E(r)"=rep(datiBenchmark$expectedReturn,nrow(ottimizConBench$optimalStdev)),
				"Bench. Stdev"=rep(datiBenchmark$stdev,nrow(ottimizConBench$optimalStdev)))
	} else {
		ottimizConBench$optimalStdev <- cbind(ottimizConBench$optimalStdev,
				"Bench. E(r)"=rep(NA_real_,nrow(ottimizConBench$optimalStdev)),
				"Bench. Stdev"=rep(NA_real_,nrow(ottimizConBench$optimalStdev)))
	}
	## adjust the "Information Ratio" and "Tracking Error" for non convergence
	ottimizConBench$optimalStdev[!ottimizConBench$feasible,"Information Ratio"] = NA_real_
	ottimizConBench$optimalStdev[!ottimizConBench$feasible,"Tracking Error"] = NA_real_
	
	if (conVincoloBenchmark) {
		ottimizConBench$optimalStdev[ottimizConBench$feasible,"Information Ratio"] = 
				(ottimizConBench$optimalStdev[ottimizConBench$feasible,"E(r)"]-datiBenchmark$expectedReturn) /
				ottimizConBench$optimalStdev[ottimizConBench$feasible,"Tracking Error"]
	}
	# adjust the output's scale in order to satisfy the reporting horizon
	tmp <- sqrt(reportingScaleFactor)  
	ottimizConBench$optimalStdev[,"E(r)"] = reportingScaleFactor * ottimizConBench$optimalStdev[,"E(r)"]
	ottimizConBench$optimalStdev[ottimizConBench$feasible,"Portfolio stdev"] = tmp *
			ottimizConBench$optimalStdev[ottimizConBench$feasible,"Portfolio stdev"]
	ottimizConBench$optimalStdev[ottimizConBench$feasible,"Tracking Error"] = tmp *
			ottimizConBench$optimalStdev[ottimizConBench$feasible,"Tracking Error"]
	ottimizConBench$optimalStdev[ottimizConBench$feasible,"Information Ratio"] = tmp *
			ottimizConBench$optimalStdev[ottimizConBench$feasible,"Information Ratio"]
	ottimizConBench$optimalStdev[,"Bench. E(r)"] = reportingScaleFactor * ottimizConBench$optimalStdev[,"Bench. E(r)"]
	ottimizConBench$optimalStdev[,"Bench. Stdev"] = tmp * ottimizConBench$optimalStdev[,"Bench. Stdev"]
	datiBenchmark$C1toExport <- datiBenchmark$C1toExport * reportingScaleFactor
	datiBenchmark$C2toExport <- datiBenchmark$C2toExport * reportingScaleFactor    
	rm(tmp)
} ## fine   if (any(ottimizSenzaBench$feasible))






if (l.datiRiskFree$conRiskFree)
{
	tmp = apply(X=ottimizConBench$P,MARGIN=2,FUN=sum)
	ottimizConBench$P = rbind(1-tmp,ottimizConBench$P)
	rownames(ottimizConBench$P)[1] <- l.datiRiskFree$idRiskFree
	rm(tmp)
}


## create some variables to export to MS Excel
## create the yearly covariance matrix, the correlation matrix and the matrix of
## the eigenvalues of the correlation matrix 
Z$covMatrix <- covMatrix[riskFactors$nomi,riskFactors$nomi]/orizzonteExpectedReturns
Z$rho <- cov2cor(Z$covMatrix)
Z$eigenvalues <- as.matrix(eigen(Z$rho)[["values"]],ncol=1)
Z$noClassCovMatrix <- Z$covMatrix

Z$covMatrix <- aggiungiClassi(riskFactors$classiConNomi,Z$covMatrix,ancheColonne=TRUE)                                           
Z$corrMatrix <- aggiungiClassi(riskFactors$classiConNomi,Z$rho,ancheColonne=TRUE)
dimnames(Z$eigenvalues) <- list(as.character(1:nrow(Z$eigenvalues)),"Eigenvalues")

Z$devStand = Z$Stdev 
Z$devStand[,"Stdev"] = Z$Stdev[,"Stdev"]/sqrt(orizzonteExpectedReturns)
Z$devStand <- aggiungiClassi(riskFactors$classiConNomi,Z$devStand[riskFactors$nomi,"Stdev",drop=FALSE]) 

## aggiungi le classi al risultato dell'ottimizzazione vincolata
if (l.datiRiskFree$conRiskFree)
{
	classiConNomi <- c(l.datiRiskFree$classiConNomi,riskFactors$classiConNomi)
} else {
	classiConNomi <- riskFactors$classiConNomi
}

ottimizSenzaBench$P <- aggiungiClassi(classiConNomi,ottimizSenzaBench$P)
ottimizConBench$P <- aggiungiClassi(classiConNomi,ottimizConBench$P)


graphs <- list()
graphs$m.dati <- ottimizSenzaBench$optimalStdev[,c("E(r)","Portfolio stdev")]
graphs$m.dati <- cbind(graphs$m.dati,ottimizConBench$optimalStdev[,"Portfolio stdev"])

colnames(graphs$m.dati)[c(2,3)] <- c("Portf. stdev without TE","Portf. stdev with TE")
if (is.element("Bench. Stdev",colnames(ottimizConBench$optimalStdev))) {
	graphs$m.dati <- cbind(graphs$m.dati,ottimizConBench$optimalStdev[,"Bench. E(r)"],ottimizConBench$optimalStdev[,"Bench. Stdev"]) 
	colnames(graphs$m.dati)[c(4,5)] <- c("Benchmark E(r)","Benchmark stdev")
}

#pesi<-cbind(t(v.nomiSerieStoriche),ottimizSenzaBench$P[complete.cases(ottimizSenzaBench$P[,2]),2])


pesi[as.yearqtr(i,format="%Y Q%q"),]<-t(ottimizSenzaBench$P[complete.cases(ottimizSenzaBench$P[,2]),2])
sharpe[as.yearqtr(i,format="%Y Q%q"),]<-ottimizSenzaBench$optimalStdev[3,3]
indice=indice+1

}

rendimentieffettivi<-window(rendimentiannualizzati,start=as.yearqtr(inizio,format ="%Y Q%q")+3/12,end=as.yearqtr(fine,format ="%Y Q%q")+3/12)
rendimentieffettivi<-rendimentieffettivi[,-14]
rendimentieffettivi<-rendimentieffettivi[,-14]
rendimentieffettivi<-rendimentieffettivi[,-14]
rendimentieffettivi<-cbind(rendimentieffettivi[,1:8],rendimentieffettivi[,14],rendimentieffettivi[,9:13])
aspettative<-matrix(,ncol=17,nrow=41)
aspettative<-zooreg(aspettative,start=as.yearqtr(inizio,format ="%Y Q%q"),end=as.yearqtr(fine,format ="%Y Q%q"),frequency=4)
for(i in index(pesi2)){
	primoytrmedia<-as.yearqtr(i,format ="%Y Q%q" )-nrrendimentixmedia*4/12
campionerendimenti<-window(rendimentiannualizzati,start=as.yearqtr(primoytrmedia,format ="%Y Q%q"),end=as.yearqtr(i,format ="%Y Q%q"))
aspettative[as.yearqtr(i,format="%Y Q%q"),]<-apply(campionerendimenti,2,FUN=mean)
}

rendimentiportofolio<-diag(pesi%*%t(rendimentieffettivi))
prod(rendimentiportofolio+1)^(1/41)-1