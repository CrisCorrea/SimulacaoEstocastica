ARDp=function(RM, SalarioContribuicao, i, tempo,rodadas,p){
##Calcula a al�quota de risco demogr�fico
##RM = Matriz de Reserva Matem�tica a cada tempo em cada rodada
##VPCF=Valor presente dos sal�rios de contribui��o de todos os servidores (valor m�dio nas k rodadas)
##TP = tamanho da popula��o
##i= taxa de rentabilidade
##p=probabilidade de n�o ter d�ficit
##t=tempo de an�lise

limite=0
if (mean(RM[tempo,]<0)>0) limite=quantile(RM[tempo,][RM[tempo,]<0],1-p)   ##Se existem valores abaixo de 0, calcula limite
DE=sum(RM[tempo,][RM[tempo,]<limite])/rodadas       ###DE = tamanho m�dio da ru�na 
VPDEF=DE*(1/(1+i)^(tempo))  ##Valor presente do d�ficit esperado futuro

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







