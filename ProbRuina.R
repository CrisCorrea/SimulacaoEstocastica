ProbRuina=function(RM){
##Calcula a probabilidade de ru�na para uma matriz de reservas matem�ticas com k simula��es

tempo=dim(RM)[1]
P.Ruina=vector(length=tempo)

for (t in 1:tempo) P.Ruina[t]=mean(RM[t,]<0)   ###Diz quantas vezes foi menor que 0em frequencia e atribui a  P.Ruina

return(P.Ruina)
}


TamRuina=function(RM){
##Calcula o tamanho m�dio da ru�na dado que houve ruina para uma matriz de reservas matem�ticas com k simula��es

tempo=dim(RM)[1]
Tam.Ruina=rep(0,tempo)

for (t in 1:tempo) if (sum(RM[t,]<0)>0) Tam.Ruina[t]=-mean(RM[t,][RM[t,]<0])

return(Tam.Ruina)
}







