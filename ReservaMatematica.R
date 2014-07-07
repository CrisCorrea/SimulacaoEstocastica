ReservaMatematica=function(Contribuicao,Beneficio,rentabilidade){
##Reserva com ajuste para meio do período

tempo = dim(Contribuicao)[1]
rodadas=dim(Contribuicao)[2]

###Reserva Matemática COM rentabilidade
Reserva=array(data = 0,  dim=c(tempo,rodadas))
#Reserva Matemática em t=0 é 0. 
for (t in 2:tempo)     Reserva[t,]=Reserva[t-1,]*(1+rentabilidade)+(Contribuicao[t-1,]*(1+rentabilidade)^1.5+Contribuicao[t,]*(1+rentabilidade)^0.5)/2-(Beneficio[t-1,]*(1+rentabilidade)^1.5+Beneficio[t,]*(1+rentabilidade)^0.5)/2

return(Reserva)

}



#ReservaMatematica=function(Contribuicao,Beneficio,rentabilidade){
##Reserva sem ajuste para meio do período

#tempo = dim(Contribuicao)[1]
#rodadas=dim(Contribuicao)[2]

#Reserva=array(data = 0,  dim=c(tempo,rodadas))
#Reserva Matemática em t=0 é 0. 
#for (t in 2:tempo) Reserva[t,]=Reserva[t-1,]*(1+rentabilidade)+Contribuicao[t,]-Beneficio[t,]

#return(Reserva)
#}




