Aliquota=function(SalarioContribuicao,Beneficio,juros){

tempo=dim(Beneficio)[[1]]-1
rodadas=dim(Beneficio)[2]
####Valor presente das contribuições e dos benefícios
v=1/(1+juros)^seq(0,tempo)
VPBF=vector(length=rodadas)
VPCF=vector(length=rodadas)
for (k in 1:rodadas){
    VPBF[k]=sum(Beneficio[,k]*v)
    VPCF[k]=sum(SalarioContribuicao[,k]*v)
}

###Alíquota média = alíquota ideal=alíquota média em k simulações
#Pressuposto: alíquota média mantém equilíbrio atuarial do plano considerando a compensação previdenciária, pois assume que contribuições futuras são capazes de arcar com benefícios futuros já descontada a compensação previdenciária.
#boxplot(VPBF/VPCF, ylab="Valor da Alíquota")
aliquota=mean(VPBF/VPCF)
print("Alíquota de contribuição")
print(aliquota)

return(aliquota)
}



