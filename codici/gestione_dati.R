# TODO: Add comment
# 
# Author: muffin
###############################################################################


#cartella e libreria zoo per dataframe con serie storiche
setwd("C:\\R\\r-output")
library(zoo)

#tabella confronto vola day vs vola qtr

rendimentigeometrici<-function(x) {diff(log(x))}
datigiornalieri<-read.zoo("dati.txt", format ="%d.%m.%Y") 
datitrimestrali<-aggregate(datigiornalieri,as.yearqtr,tail,1)
rendimentigiornalieri<-rendimentigeometrici(datigiornalieri)
rendimentitrimestrali<-rendimentigeometrici(datitrimestrali)
qtrtoyear<-function(x) (1+x)^4-1
daytoyear<-function(x) (1+x)^365-1
rdayannual<-daytoyear(rendimentigiornalieri)
rqtrannual<-qtrtoyear(rendimentitrimestrali)
dayvola<-round(diag(var(rdayannual))^0.5, digit=5)
qtrvola<-round(diag(var(rqtrannual))^0.5, digit=5)
write.table(dayvola,sep="\t", col.names = F, row.names = F)
write.table(qtrvola,sep="\t", col.names = F, row.names = F)
