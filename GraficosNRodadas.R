GraficosNRodadas <- function (Matriz){ #faz gr�ficos para v�rias rodadas

tempo = dim(Matriz)[2]
rodadas=dim(Matriz)[3]
status=6

#### Gera matrizes auxiliares
MatrizResumo=array(data = NA,  dim=c(6, tempo,rodadas))  ## Vetor de frequencia de cada status no tempo
for (k in 1:rodadas){
    for (i in 1:tempo){
        for (j in 1:6){
            MatrizResumo[j,i,k]=sum(Matriz[,i,k]==j)
        }
    }
}
dimnames(MatrizResumo)=list(c("Ativos","Inv�lidos", "Aposentados", "Filhos benefici�rios", "C�njuge benefici�rio", "Morto sem benefici�rio"),seq(1:tempo) ,seq(1:rodadas))


MatrizMedia=array(data = NA,  dim=c(status, tempo))  ## Vetor de m�dia de cada status no tempo
for (t in 1:tempo){
    for (j in 1:status){
        MatrizMedia[j,t]=mean(MatrizResumo[j,t,])
    }
}

yMax= vector(length=status)
yMin= vector(length=status)
for (i in 1:status){
    yMax[i]=max(MatrizResumo[i,,])
    yMin[i]=min(MatrizResumo[i,,])
}
                       

###Gr�ficos dos valores observados
#png("D:\\Dropbox\\Tese\\Resultados\\Resultados das simula��es\\Varia��oEstados.png")
par(mfrow=c(3,2))
for (j in 1:status){
    plot(MatrizResumo[j,,1], type="l", xlab="Tempo", ylab=(dimnames(MatrizResumo)[[1]][j]), ylim=c(yMin[j],yMax[j]))
    for (k in 2:rodadas){
        lines(MatrizResumo[j,,])
    }
    lines(MatrizMedia[j,], col="red", lwd=2)
 }
#dev.off()

MatrizDiferenca=array(data = NA,  dim=c(6, tempo,rodadas))  ## Vetor de frequencia de cada status no tempo
for (k in 1:rodadas){
    for (i in 1:tempo){
        for (j in 1:6){
            MatrizDiferenca[j,i,k]=MatrizResumo[j,i,k]-mean(MatrizResumo[j,i,])
        }
    }
}
dimnames(MatrizDiferenca)=list(c("Diferen�a Ativos","Diferen�a Inv�lidos", "Diferen�a Aposentados", "Diferen�a Filhos benefici�rios", "Diferen�a C�njuge benefici�rio", "Diferen�a Morto sem benefici�rio"),seq(1:tempo) ,seq(1:rodadas))


###Gr�ficos das diferen�as observadas
for (i in 1:status){
    yMax[i]=max(MatrizDiferenca[i,,])
    yMin[i]=min(MatrizDiferenca[i,,])
}
    
par(mfrow=c(3,2))
for (j in 1:status){
    plot(MatrizDiferenca[j,,1], type="l", xlab="Tempo", ylab=(dimnames(MatrizResumo)[[1]][j]), ylim=c(yMin[j],yMax[j]))
    for (k in 2:rodadas){
        lines(MatrizDiferenca[j,,])
    }
    abline(h=0, col="red", lwd=2)
 }



} 








