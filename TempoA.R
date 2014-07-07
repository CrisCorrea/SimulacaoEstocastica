TempoA=function(DadosServidores,r,TMDf,TMDm){
###Calcula tempo até a saíde por morte ou invalidez pela TMD.

TempoAtivo=TempoAteSaida(DadosServidores$x, DadosServidores$Sexo,TMDf,TMDm)
#png("TempoAtivo x Idade.png")
#plot(DadosServidores$x[DadosServidores$Sexo==1],TempoAtivo[DadosServidores$Sexo==1], xlab="Idade atual", ylab="Tempo até a saída por invalidez ou morte", col="red",pch=1)
#points(DadosServidores$x[DadosServidores$Sexo==2],TempoAtivo[DadosServidores$Sexo==2], xlab="Idade atual", ylab="Tempo até a saída por invalidez ou morte", col="blue", pch=0)
#legend(x=50, y = 60, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"))
#dev.off()

TempoAp=r-DadosServidores$x
TempoAtivo[TempoAtivo>TempoAp]=TempoAp[TempoAtivo>TempoAp]

return(TempoAtivo)
}




