Simula=function(DadosServidores,premissas, TMD,TabuasMorte,ProbFamilia, rodadas, tempo){
###Faz microssimulação para reserva matemática para uma população de servidores



#### Estima estados dos indivíduos no tempo
##Lembrar que em t=0 todos são ativos. Em t=1 alguns já mudaram de estado.
#A simulação começa em t=1.
REstados=EstadoNoTempo(DadosServidores, TMD,TabuasMorte,ProbFamilia, tempo, rodadas)
Estados=REstados[[1]]
y=REstados[[2]]
IdadeAposenta=REstados[[3]]
#plot(Estados[1,,1])
#write.table(Estados[,,1], "C:\\Users\\Cris Corrêa\\Desktop\\Estados.xls", sep="\t") ##Exporta para arquivo excel




#GraficosNRodadas(Estados)

#Matriz de contribuições
##Estima contribuições considerando alíquota =100%
ContribuicaoMaxima=Contribuicao(Estados, DadosServidores, premissas)
#matplot(ContribuicaoMaxima/1000000, type = "l", ylab="Salário de Contribuição ($ Milhões)", xlab="Tempo")

### Matriz de benefícios
##Benefícios dos indivíduos com status de beneficiários em t.
BeneficioTotal=Beneficios(Estados,DadosServidores,y,IdadeAposenta,premissas,TabuasMorte)
#matplot(BeneficioTotal/1000, type="l",ylab="Beneficios Pagos (Mil)", xlab="Tempo" )



return(list(ContribuicaoMaxima,BeneficioTotal))

}













