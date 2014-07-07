TempoB1=function(MSA,DadosServidores,ProbFamilia,TabuaF, TabuaM){
#Estima o tempo de duração do primeiro benefício (se houver)

MotivoSai=MSA[[1]]
SB1=MSA[[2]]
IB1=MSA[[3]]

##Tempo que beneficiário1 recebeu benefício (só sai por morte)
TB1=TempoAteSaida(IB1, SB1,TabuaF, TabuaM)
####Se beneficiário era filho menor, benefício acaba quando completa 21 anos ou quando morre, o que acontecer primeiro.
pop=length(IB1)
for (j in 1:pop)   if (MotivoSai[j]==4 & (21-IB1[j]< TB1[j])) TB1[j]=21-IB1[j]

#png("IB1 x TB1.png")
#plot(IB1[SB1==1],TB1[SB1==1], xlab="Idade inicial do beneficiário 1", ylab="Duração do benefício em anos",col="red",pch=1)
#points(IB1[SB1==2],TB1[SB1==2], ,col="blue", pch=0)
#legend(x=80, y = 50, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"))
#dev.off()

return (TB1)
}









