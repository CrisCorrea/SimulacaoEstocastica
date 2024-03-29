
BeneficioIdadeR=function(DadosServidores,y,IdadeAposenta) {
##Br � o valor que receberia por aposentadoria programada. Depende do tipo de aposentadoria. 

Sexo=DadosServidores$Sexo
x=DadosServidores$x    ##Considerar tempo em anos
r=IdadeAposenta[[1]] 
TempoContribuicao=r-y
BrInt=DadosServidores$BrInt


Br= array(data = 0, dim=c(length(DadosServidores[,1]),length(r[1,])))  ##dimen��es: pop e rodadas
#Se aposentadoria por idade
a=BrInt*TempoContribuicao/(IdadeAposenta[[2]]-y)
Br[(Sexo==1 & r==60 & TempoContribuicao<30)]=a[(Sexo==1 & r==60 & TempoContribuicao<30)]
Br[(Sexo==2 & r==65 & TempoContribuicao<35)]=a[(Sexo==2 & r==65 & TempoContribuicao<35)]
Br[(Sexo==1 & r==60 & TempoContribuicao>=30)]=BrInt[(Sexo==1 & r==60 & TempoContribuicao>=30)]
Br[(Sexo==2 & r==65 & TempoContribuicao>=35)]=BrInt[(Sexo==2 & r==65 & TempoContribuicao>=35)]

#Se aposentadoria  por idade e tempo
Br[(Sexo==1 & r>=55 & TempoContribuicao>=30)]= BrInt[(Sexo==1 & r>=55 & TempoContribuicao>=30)]
Br[(Sexo==2 & r>=60 & TempoContribuicao>=35)]= BrInt[(Sexo==2 & r>=60 & TempoContribuicao>=35)]

#Se aposentadoria  compuls�ria
a=BrInt*TempoContribuicao
Br[((Sexo==1) & (r==70) & (TempoContribuicao>=30))]= BrInt[((Sexo==1) & (r==70) & (TempoContribuicao>=30))]
Br[(Sexo==1 & r==70 & TempoContribuicao<30)]= (a[(Sexo==1 & r==70 & TempoContribuicao<30)]/30)
Br[(Sexo==2 & r==70 & TempoContribuicao>=35)]= BrInt[(Sexo==2 & r==70 & TempoContribuicao>=35)]
Br[(Sexo==2 & r==70 & TempoContribuicao<35)]= (a[(Sexo==2 & r==70 & TempoContribuicao<35)]/35)

#matplot(r,Br)

return(Br)
}







BeneficioPeloRPPS=function (DadosServidores,Estados,y,IdadeAposenta){
##Calcula benef�cio pelas regras do RPPS

ssm=premissas$ss     ##Taxa de aumento das remunera��es 

pop=dim(Estados)[1]
tempo = dim(Estados)[2]     
rodadas=dim(Estados)[3]

x=DadosServidores$x    
Sexo=DadosServidores$Sexo
r=IdadeAposenta[[1]] 
sy=DadosServidores$sy
Sal�rio=DadosServidores$Sal�rio*12  ##TRansforma sal�rio mensal em anual
BrInt=DadosServidores$BrInt
TempoContribuicao=r-y

Br=BeneficioIdadeR(DadosServidores,y,IdadeAposenta)


BeneficiosRPPS=array(data = NA,  dim=c(pop,tempo,rodadas))  ## Vetor de frequencia de cada status no tempo
BeneficiosRPPS[(Estados==1 | Estados==6)]=0   # Se n�o tem benefici�rio, benef�cio=0

#Para tempo t=0, ou seja, coluna=1
BeneficiosRPPS[,1,]=0
for (k in 1:rodadas){
    for (j in 1:pop){
        #Para tempo t>1, ou seja, da coluna 2 em diante.
        for (t in 2:tempo){
           ##Se � aposentado, valor do benef�cio j� foi calculado
           if (Estados[j,t,k]==3) BeneficiosRPPS[j,t,k]=Br[j,k]
           ## Se � inv�lido, recebe benef�cio integral calculado � idade de invalidez
           if ((Estados[j,t-1,k]==1) & (Estados[j,t,k]==2))  BeneficiosRPPS[j,t,k]=(sy[j])*((1+ssm)^((x[j]+t)-1)-(1+ssm)^(0.2*(x[j]+t)+0.8*y[j,k]))/(0.8*((x[j]+t)-y[j,k])*ssm)
           ##Se � pensionista, benef�cio = �ltimo sal�rio da ativa, se ativo, ou �ltimo benef�cio de aposentadoria
           if ((Estados[j,t-1,k]==1) & (Estados[j,t,k]==4 | Estados[j,t,k]==5)) BeneficiosRPPS[j,t,k]= Sal�rio[j]*(1+ssm)^t
           if ((Estados[j,t-1,k]==3) & (Estados[j,t,k]==4 | Estados[j,t,k]==5)) BeneficiosRPPS[j,t,k]= BeneficiosRPPS[j,t-1,k]
           ## Se j� � benefici�rio, repete valor do benef�cio
           if ((Estados[j,t-1,k]!=1) & (Estados[j,t,k]!=6)) BeneficiosRPPS[j,t,k]=BeneficiosRPPS[j,t-1,k]
       }
    }
}

return(BeneficiosRPPS)

}







FatorPrevidenciario=function(TabuaFP,DadosServidores,y,IdadeAposenta,j){
#Calcula fotor previdenci�rio para indiv�duo j. 
ex=exn(TabuaFP,x=IdadeAposenta[[1]] [j],type="curtate")
if (DadosServidores$Sexo[j]==1) tc=IdadeAposenta[[1]] [j]-y[j]+5
if (DadosServidores$Sexo[j]==2) tc=IdadeAposenta[[1]] [j]-y[j]

f=tc*0.31/ex*(1+(IdadeAposenta[[1]] [j]+tc*0.31)/100)
return(f)
} 






BeneficioPeloRGPS=function(DadosServidores,y,IdadeAposenta,TabuasMorte,Estados, premissas) {

#Calcula benef�cio pelas regras do RGPS

ssm=premissas$ss  ##Taxa de aumento das remunera��es

pop=dim(Estados)[1]
tempo = dim(Estados)[2]    
rodadas=dim(Estados)[3]

x=DadosServidores$x
y=y
r=IdadeAposenta[[1]] 
sy=DadosServidores$sy
Sal�rio=DadosServidores$Sal�rio*12  ##Transforma sal�rio mensal em anual
BrInt=DadosServidores$BrInt
TempoContribuicao=IdadeAposenta[[1]] -y           
TabuaFP=new("lifetable",x=TabuasMorte$idade,lx=TabuasMorte$lxMorte.AS)

BeneficiosRGPS=array(data = NA,  dim=c(pop, tempo,rodadas))  ## Valor do benefc�cio pelas normas do RGPS
BeneficiosRGPS[(Estados==1 | Estados==6)]=0   # Se n�o tem benefici�rio, benef�cio=0

#Para tempo t=0, ou seja, para a primeira coluna da matrix
BeneficiosRGPS[,1,]=0
for (k in 1:rodadas){
        for (j in 1:pop){
            #Para tempo t>1
            for (t in 2:tempo){
                ## Se � inv�lido, recebe benef�cio integral calculado � idade de invalidez
                if ((Estados[j,t-1,k]==1) & (Estados[j,t,k]>=2)){
                   BeneficiosRGPS[j,t,k]=(sy[j])*((1+ssm)^((x[j]+t)-1)-(1+ssm)^(0.2*(x[j]+t)+0.8*y[j]))/(0.8*((x[j]+t)-y[j])*ssm)
                }
                ##Se � pensionista, benef�cio = �ltimo sal�rio da ativa, se ativo, ou �ltimo benef�cio de aposentadoria
                if ((Estados[j,t-1,k]==1) & (Estados[j,t,k]==4 | Estados[j,t,k]==5)) {
                   BeneficiosRGPS[j,t,k]= Sal�rio[j]*(1+ssm)^t
                }
                ## Se j� era benefici�rio, repete valor do benef�cio
                if ((Estados[j,t-1,k]!=1) & (Estados[j,t,k]!=6)) BeneficiosRGPS[j,t,k]=BeneficiosRGPS[j,t-1,k]
                ## Se se aposentou, benef�cio = m�dia 80% maiores remunera��o * fator previdenci�rio
                if (Estados[j,t,k]==3) BeneficiosRGPS[j,t,k]=BrInt[j]*FatorPrevidenciario(TabuaFP,DadosServidores,y,IdadeAposenta,j)
            }
       }
}

return(BeneficiosRGPS)
}







CompensacaoFinanceira=function(Estados,y,BeneficiosRPPS,BeneficiosRGPS,DadosServidores) {
##Calcula Compensa��o financeira

### Calcular Compensa��o financeira
#CF=Min(valor de benef�cio pelo RGPS; valor de benef�cio pelo RPPS)*Tc(Rgps)/Tc(total)
#Valor do benef�cio pelo RPPS = BeneficiosRPPS
#Valor do benef�cio pelo RPPS = BeneficiosRGPS
#Tc (tempo de contribui��o) do RPPS = r-x
#Tc total(tempo de contribui��o total)= r-y

pop=dim(Estados)[1]
tempo = dim(Estados)[2]     
rodadas=dim(Estados)[3]

x=DadosServidores$x    

Compensacao=BeneficiosRPPS  
Compensacao[(Estados==1 | Estados==6)]=0   # Se n�o tem benefici�rio, compensa��o=0
Compensacao[,1,]=0    ##Para tempo t=0, ou seja, para a primeira coluna da matriz
for (k in 1:rodadas){
    ##Para tempo t=1
    for (j in 1:pop){
        ##Para tempo t>1
        for (t in 2:tempo){
            ##Se in�cio de benef�cio
            if (Estados[j,t-1,k]==1 & (Estados[j,t,k]>=2 & Estados[j,t,k]<=5)) {
               Compensacao[j,t,k]=(min(BeneficiosRPPS[j,t,k],BeneficiosRGPS[j,t,k])*(x[j]-y[j,k])/((x[j]+t)-y[j,k]))
            }
            ## Se continua��o de benefc�io
            if (Estados[j,t-1,k]!=1 & (Estados[j,t,k]>=2 & Estados[j,t,k]<=5)) {
               Compensacao[j,t,k]=Compensacao[j,t-1,k]
            }
        }
    }
}

return(Compensacao)
}






Beneficios <- function (Estados,DadosServidores,y,IdadeAposenta,premissas,TabuasMorte){
### Calcula a matriz de benef�cios para as n rodadas


x=DadosServidores$x
Sal�rio=DadosServidores$Sal�rio*12  ##Transforma sal�rio mensal em anual
ssa=premissas$ss               ##Taxa de aumento das remunera��es 
r=IdadeAposenta[[1]]

##Completa informa��es na base de dados dos servidores. 
###Sal�rio na idade de entrada y.
DadosServidores$sy=(Sal�rio)/((1+ssa)^((x-y)))        
sy=DadosServidores$sy
### Benef�cio integral
DadosServidores$BrInt=sy*((1+ssa)^((r-1))-(1+ssa)^(0.2*r+0.8*y))/(0.8*(r-y)*ssa)

BeneficiosRPPS=BeneficioPeloRPPS (DadosServidores,Estados,y,IdadeAposenta)       ##Benef�cio pelas regras do RPPS
BeneficiosRPPSTotal=TotalPorTempoeRodada(BeneficiosRPPS)

BeneficiosRGPS=BeneficioPeloRGPS (DadosServidores,y,IdadeAposenta,TabuasMorte,Estados, premissas)    ##Benef�cio pelas regras do RGPS
BeneficiosRGPSTotal=TotalPorTempoeRodada(BeneficiosRGPS)

Compensacao=CompensacaoFinanceira(Estados,y,BeneficiosRPPS,BeneficiosRGPS,DadosServidores)        ##Valor pago pelo RGPS pelo tempo que pessoa contribuiu para o RGPS antes da implementa��o do RPPS.
CompensacaoTotal=TotalPorTempoeRodada(Compensacao)
 
BeneficiosPagos= BeneficiosRPPS-Compensacao      ##Calcula benef�cio pago pelo plano, j� descontada a compensa��o financeira.
BeneficiosPagosTotal=TotalPorTempoeRodada(BeneficiosPagos)



#par(mfrow=c(2,2))
#ymax=max(BeneficiosRPPSTotal,BeneficiosRGPSTotal)
#matplot(BeneficiosRPPSTotal/1000000, type = "l",ylab="Beneficios pela regra do RPPS (Milh�es)", xlab="Tempo" , ylim=c(0,ymax/1000000)) 
#abline(h=0)
#matplot(BeneficiosRGPSTotal/1000000, type="l", ylab="Benefc�io pela regra do RGPS (Milh�es)", xlab="Tempo" , ylim=c(0,ymax/1000000))
#abline(h=0)
#matplot(CompensacaoTotal/1000000, type="l", ylab="Compensa��o Financeira (Milh�es)", xlab="Tempo" , ylim=c(0,ymax/1000000))
#abline(h=0)
#matplot(BeneficiosPagosTotal/1000000, type="l",ylab="Beneficios pagos(Milh�es)", xlab="Tempo" , ylim=c(0,ymax/1000000))
#abline(h=0)

return (BeneficiosPagosTotal)

}


