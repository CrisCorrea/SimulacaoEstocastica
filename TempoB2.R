TempoB2=function(IdadeBeneficiario2, SexoBeneficiario2,MotivoSaiBeneficiario1,TabuaF, TabuaM){
##Estima dura��o do benef�cio do segundo benefici�rio (caso houver)

TempoBeneficiario2=TempoAteSaida(IdadeBeneficiario2, SexoBeneficiario2,TabuaF, TabuaM)
####Se benefici�rio era filho menor, benef�cio acaba quando completa 21 anos ou quando morre, o que acontecer primeiro.
pop=length(IdadeBeneficiario2)
for (j in 1:pop){
  if (MotivoSaiBeneficiario1[j]==4 & (21-IdadeBeneficiario2[j]< TempoBeneficiario2[j])) TempoBeneficiario2[j]=21-IdadeBeneficiario2[j]
}
#png("IdadeBeneficiario2 x TempoBeneficiario2.png")
#plot(IdadeBeneficiario2[SexoBeneficiario2==1],TempoBeneficiario2[SexoBeneficiario2==1], xlab="Idade inicial do benefici�rio 2", ylab="Dura��o do benef�cio em anos", col="red",pch=1)
#points(IdadeBeneficiario2[SexoBeneficiario2==2],TempoBeneficiario2[SexoBeneficiario2==2],col="blue", pch=0)
#legend(x=80, y = 40, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"))
#dev.off()


return(TempoBeneficiario2)
}





