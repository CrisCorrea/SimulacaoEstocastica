TempoAteSaida=function(idade, sexo, TabuaFeminina, TabuaMasculina){
##Estima tempo at� sa�da a aprtir de t�bua de vida

Tempo=vector(length=length(idade))
###Calcula tempo at� a sa�de por morte ou invalidez pela TMD.
for (i in 1:length(idade)){
    if (sexo[i]==1 ) Tempo[i]=rLife(n=1,object=TabuaFeminina,x=idade[i],type="Kx")     ##Tipe=Kx - tempo discreto (Tx=Kx+0.5)
    if (sexo[i]==2 ) Tempo[i]=rLife(n=1,object=TabuaMasculina,x=idade[i],type="Kx")
}

return (Tempo)
}









