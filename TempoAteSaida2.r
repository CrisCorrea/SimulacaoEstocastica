TempoAteSaida=function(idade, sexo, TabuaFeminina, TabuaMasculina){
##Estima tempo até saída a aprtir de tábua de vida

Tempo=vector(length=length(idade))
###Calcula tempo até a saíde por morte ou invalidez pela TMD.
for (i in 1:length(idade)){
    Tempo[i][sexo[i]==1] =rLife(n=1,object=TabuaFeminina,x=idade[i],type="Kx")     ##Tipe=Kx - tempo discreto (Tx=Kx+0.5)
    Tempo[i][sexo[i]==2]=rLife(n=1,object=TabuaMasculina,x=idade[i],type="Kx")
}

return (Tempo)
}



                    





