IdadeEntrada <- function (DadosServidores, rodadas){
### estima idade em que o servidor entrou no serviço público.


x=DadosServidores$x
Sexo=DadosServidores$Sexo
pop= length(DadosServidores[,1])
IdadeEntrada=array(data = 0,  dim=c(pop, rodadas)) 

#Estima média da distribuição.
media=vector(length=pop)
#Homem
media[Sexo==2]=(6.94+0.62*x[Sexo==2])
#Mulher
media[Sexo==1]=6.94+0.62*x[Sexo==1]+1.99-0.07*x[Sexo==1]

#estima desvio padrão
dp=vector(length=pop)
dp[Sexo==2 & x<=19] = 0.51
dp[Sexo==2 & (x>=20 & x<=24) ] = 1.63
dp[Sexo==2 & (x>=25 & x<=29) ] = 2.57
dp[Sexo==2 & (x>=30 & x<=34) ] = 3.97
dp[Sexo==2 & (x>=35 & x<=39) ] = 5.29
dp[Sexo==2 & (x>=40 & x<=44) ] = 6.95
dp[Sexo==2 & (x>=45 & x<=49) ] = 9.10
dp[Sexo==2 & (x>=50)] = 10.89
dp[Sexo==1 & x<=19] = 0.50
dp[Sexo==1 & (x>=20 & x<=24) ] = 1.60
dp[Sexo==1 & (x>=25 & x<=29) ] = 2.75
dp[Sexo==1 & (x>=30 & x<=34) ] = 4.04
dp[Sexo==1 & (x>=35 & x<=39) ] = 5.50
dp[Sexo==1 & (x>=40 & x<=44) ] = 7.06
dp[Sexo==1 & (x>=45 & x<=49) ] = 8.74
dp[Sexo==1 & (x>=50)] = 10.10

for (k in 1:rodadas) {
    randon=rnorm(pop, mean = 0, sd = 1)    ##Gera número aleatório 
    for (j in 1:pop)    {
    IdadeEntrada[j,k]= min((x[j]-1),floor(randon[j]*dp[j]+media[j]))    #Arredonda para baixo o valor da idade, indicando idade completa. Assume que idade de entrada mínima é a idade anterior à atual. (Não dá para ser a atual porque pode geral tempo de contribuição igual a zero na função de benefícios). 
    }
}       

IdadeEntrada[IdadeEntrada<18]=18

#png("Idade de entrada.png")
#ymax=max(IdadeEntrada[,1])
#plot(DadosServidores$x[DadosServidores$Sexo==1],IdadeEntrada[DadosServidores$Sexo==1,1],xlab="Idade atual", ylab="Idade de entrada", col="red",pch=1, ylim=c(18,ymax))
#points(DadosServidores$x[DadosServidores$Sexo==2],IdadeEntrada[DadosServidores$Sexo==2,1],col="blue", pch=0)
#legend(x=25, y = ymax, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"))
#dev.off()
      
      
return (IdadeEntrada)

}





