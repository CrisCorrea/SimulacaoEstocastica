TempoB1=function(MSA,DadosServidores,ProbFamilia,TabuaF, TabuaM){
#Estima o tempo de dura��o do primeiro benef�cio (se houver)

MotivoSai=MSA[[1]]
SB1=MSA[[2]]
IB1=MSA[[3]]

##Tempo que benefici�rio1 recebeu benef�cio (s� sai por morte)
TB1=TempoAteSaida(IB1, SB1,TabuaF, TabuaM)
####Se benefici�rio era filho menor, benef�cio acaba quando completa 21 anos ou quando morre, o que acontecer primeiro.
pop=length(IB1)
for (j in 1:pop)   if (MotivoSai[j]==4 & (21-IB1[j]< TB1[j])) TB1[j]=21-IB1[j]

#png("IB1 x TB1.png")
#plot(IB1[SB1==1],TB1[SB1==1], xlab="Idade inicial do benefici�rio 1", ylab="Dura��o do benef�cio em anos",col="red",pch=1)
#points(IB1[SB1==2],TB1[SB1==2], ,col="blue", pch=0)
#legend(x=80, y = 50, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"))
#dev.off()

return (TB1)
}









