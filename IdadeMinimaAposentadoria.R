IdadeMinimaAposentadoria <- function (DadosServidores,y, rodadas){
##Estima idade mínima de aposentadoria programada.

pop=length(DadosServidores[,1])   #declara o tamanho da população inicial
r=rTempo=rIdade=array(data = 0, dim=c(pop,rodadas))
## Idade de aposentadoria por idade:rIdade    
#Idade de Aposentadoria por idade e tempo de contribuição:rTempo   
## Menor idade em que é elegível à aposentadoria:r

for (k in 1:rodadas){       
##Elegibilidade à aposentadoria por idade (mínimo de 10 anos no serviço público)
rIdade[DadosServidores$Sexo==1 & y[,k]<=50,k]=60
rIdade[DadosServidores$Sexo==1 & y[,k]>50,k]=y[DadosServidores$Sexo==1 & y[,k]>50,k]+10     
rIdade[DadosServidores$Sexo==2 & y[,k]<=55,k]=65
rIdade[DadosServidores$Sexo==2 & y[,k]>55,k]=y[DadosServidores$Sexo==2 & y[,k]>55,k]+10

# Elegibilidade à aposentadoria por tempo e idade
##Se entrou antes dos 25 anos de idade, aposenta por tempo de contribuição aos 60, se homem, e aos 55, se mulher. 
rTempo[(DadosServidores$Sexo==1 & y[,k]<=25),k]=55
rTempo[(DadosServidores$Sexo==1) & (y[,k]>25),k]= 30+y[(DadosServidores$Sexo==1) & (y[,k]>25),k]

rTempo[(DadosServidores$Sexo==2 & y[,k]<=25),k]=60
rTempo[(DadosServidores$Sexo==2 & y[,k]>25),k]= y[(DadosServidores$Sexo==2 & y[,k]>25),k]+35
                                      
##Avalia idade mínima de aposentadoria. 
for (j in 1:pop)  {
    if (rIdade[j,k]<=DadosServidores$x[j]) rIdade[j,k]=DadosServidores$x[j]+1       ##Ajusta para que aposentadoria não ocorra em idade antes da idade atual. 
    if (rTempo[j,k]<=DadosServidores$x[j]) rTempo[j,k]=DadosServidores$x[j]+1 
    r[j,k]=min(rIdade[j,k],rTempo[j,k],70)  ##Aposentadoria compulsória aos 70 anos
}
}


#ymin=min(r)
#ymax=max(r)
#plot(y[DadosServidores$Sexo==1,1],r[DadosServidores$Sexo==1,1],xlab="Idade de entrada", ylab="Idade Aposentadoria Programada" , col="red",pch=1, ylim=c(ymin, ymax))
#points(y[DadosServidores$Sexo==2,1],r[DadosServidores$Sexo==2,1],col="blue", pch=0)
#legend(x=18, y = 70, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"))

#plot(DadosServidores$x[DadosServidores$Sexo==1],r[DadosServidores$Sexo==1,1],xlab="Idade atual", ylab="Idade Aposentadoria Programada" , col="red",pch=1, ylim=c(ymin, ymax))
#points(DadosServidores$x[DadosServidores$Sexo==2],r[DadosServidores$Sexo==2,1],col="blue", pch=0)
#legend(x=18, y = 70, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"))



IdadeAposenta=list(r,rTempo,rIdade)
return(IdadeAposenta)
}


