Simula=function(DadosServidores,premissas, TMD,TabuasMorte,ProbFamilia, rodadas, tempo){
###Faz microssimula��o para reserva matem�tica para uma popula��o de servidores



#### Estima estados dos indiv�duos no tempo
##Lembrar que em t=0 todos s�o ativos. Em t=1 alguns j� mudaram de estado.
#A simula��o come�a em t=1.
REstados=EstadoNoTempo(DadosServidores, TMD,TabuasMorte,ProbFamilia, tempo, rodadas)
Estados=REstados[[1]]
y=REstados[[2]]
IdadeAposenta=REstados[[3]]
#plot(Estados[1,,1])
#write.table(Estados[,,1], "C:\\Users\\Cris Corr�a\\Desktop\\Estados.xls", sep="\t") ##Exporta para arquivo excel




#GraficosNRodadas(Estados)

#Matriz de contribui��es
##Estima contribui��es considerando al�quota =100%
ContribuicaoMaxima=Contribuicao(Estados, DadosServidores, premissas)
#matplot(ContribuicaoMaxima/1000000, type = "l", ylab="Sal�rio de Contribui��o ($ Milh�es)", xlab="Tempo")

### Matriz de benef�cios
##Benef�cios dos indiv�duos com status de benefici�rios em t.
BeneficioTotal=Beneficios(Estados,DadosServidores,y,IdadeAposenta,premissas,TabuasMorte)
#matplot(BeneficioTotal/1000, type="l",ylab="Beneficios Pagos (Mil)", xlab="Tempo" )



return(list(ContribuicaoMaxima,BeneficioTotal))

}













