Contribuicao<- function(MatrizStatus, DadosServidores, premissas) {
###Calcula as contribuições vertidas ao plano pelos servidores a cada tempo t em cada rodada

pop=dim(MatrizStatus)[1]
tempo = dim(MatrizStatus)[2]
rodadas=dim(MatrizStatus)[3]
ss=premissas$ss   ##Taxa de aumento das remunerações

MatrizContribuicoes=array(data = 0,  dim=c(pop, tempo,rodadas))  ## Vetor de frequencia de cada status no tempo

for (k in 1:rodadas){
    for (j in 1:pop){
        for (t in 1:tempo){
            if (MatrizStatus[j,t,k]==1) MatrizContribuicoes[j,t,k]=(DadosServidores$Salário[j]*12*(1+ss)^(t) )        ###Transforma salário mensal em anual
            if (MatrizStatus[j,t,k]!=1)(MatrizContribuicoes[j,t,k]=0)
        }
    }
}

ContribuicaoTotal=TotalPorTempoeRodada(MatrizContribuicoes)


return(ContribuicaoTotal)

}



TotalPorTempoeRodada=function(Matriz){
##Soma todos os valores por tempo de simulaçãoe  rodada.
tempo = dim(Matriz)[2]     
rodadas=dim(Matriz)[3]


MatrizTotal=array(data = NA,  dim=c(tempo, rodadas))  ## Total de valores pagos a cada tempo em cada rodada
for (k in 1:rodadas){
    for (t in 1:tempo){
        MatrizTotal[t,k]=sum(Matriz[,t,k])
    }
}

return(MatrizTotal)

}



