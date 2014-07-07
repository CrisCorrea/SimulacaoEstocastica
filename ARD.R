ARDp=function(RM, SalarioContribuicao, i, tempo,rodadas,p){
##Calcula a alíquota de risco demográfico
##RM = Matriz de Reserva Matemática a cada tempo em cada rodada
##VPCF=Valor presente dos salários de contribuição de todos os servidores (valor médio nas k rodadas)
##TP = tamanho da população
##i= taxa de rentabilidade
##p=probabilidade de não ter déficit
##t=tempo de análise

limite=0
if (mean(RM[tempo,]<0)>0) limite=quantile(RM[tempo,][RM[tempo,]<0],1-p)   ##Se existem valores abaixo de 0, calcula limite
DE=sum(RM[tempo,][RM[tempo,]<limite])/rodadas       ###DE = tamanho médio da ruína 
VPDEF=DE*(1/(1+i)^(tempo))  ##Valor presente do déficit esperado futuro

v=1/(1+i)^seq(0,tempo)
VPCF=vector(length=rodadas)
for (k in 1:rodadas){
    VPCF[k]=sum(SalarioContribuicao[,k]*v)
}
mediaVPCF=mean(VPCF)


aliquotaRD=-VPDEF/mediaVPCF
aliquotaRD

return(aliquotaRD)
}







