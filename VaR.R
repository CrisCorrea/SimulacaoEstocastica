VaR=function(RM, p){
##Calcula o VaR para uma Reserva Matemática com probabilidade p.
return(quantile(RM,p)[[1]])
}

DE=function(RM, p){
return(quantile(RM,p)[[1]]-mean(RM))
}

TVaR=function(RM, p){
return(quantile(RM,p)[[1]]+(quantile(RM,p)[[1]]-mean(RM))/(1-p))
}

VaRtempo=function(RM, p){
##Calcula o VaR para uma Reserva Matemática com probabilidade p.
tempo=dim(RM)[1]
VAR=vector(length=tempo-1)
for (t in 1:tempo) VAR[t]=quantile(RM[t,],p)
return(VAR)
}


DEtempo=function(RM, p){
tempo=dim(RM)[1]
DE=vector(length=tempo-1)
for (t in 1:tempo) DE[t]=(quantile(RM[t,],p)[[1]]-mean(RM[t,])) 
return (DE)
}

TVaRtempo=function(RM, p){
tempo=dim(RM)[1]
tvar=vector(length=tempo-1)
for (t in 1:tempo) tvar[t]=quantile(RM[t,],p)[[1]]+(quantile(RM[t,],p)[[1]]-mean(RM[t,]))/(1-p)
return(tvar)
}






GraficoPercentil=function(RM, ymin, ymax, titulo){
###Gráficos por percentil
tempo=dim(RM)[1]
P10=P90=P01=P99=P25=P75=P50=Media=vector(length=tempo)
for (t in 1:tempo) {
    P10[t]=quantile(RM[t,],.05)
    P90[t]=quantile(RM[t,],.95)
    P01[t]=quantile(RM[t,],.005)
    P99[t]=quantile(RM[t,],.995)
    P50[t]=quantile(RM[t,],.5)
    Media[t]=mean(RM[t,],.50)
}

plot(P50/100000, type="l", ylim=c(ymin/100000,ymax/100000), xlab="Tempo",
 ylab="Reserva Matemática padronizada (100 mil)",lwd=2,main=titulo)
lines (P01/100000, type="l",lty=2, col=4,lwd=2)
lines (P99/100000, type="l",lty=2, col=4,lwd=2)
lines (P10/100000, type="l",lty=4, col=2,lwd=2)
lines (P90/100000, type="l",lty=4, col=2,lwd=2)
lines (Media/100000, type="l",lty=5, col=3,lwd=2)
legend (x=1, y = (ymin+ymax)/2/100000,lty = c(1,2,4,5),lwd=2,col = c(1,4,2,3),c("Mediana","IC 90%", "IC 99%", "Média"))
abline(h=0)
}












