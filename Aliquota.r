Aliquota=function(SalarioContribuicao,Beneficio,juros){

tempo=dim(Beneficio)[[1]]-1
rodadas=dim(Beneficio)[2]
####Valor presente das contribui��es e dos benef�cios
v=1/(1+juros)^seq(0,tempo)
VPBF=vector(length=rodadas)
VPCF=vector(length=rodadas)
for (k in 1:rodadas){
    VPBF[k]=sum(Beneficio[,k]*v)
    VPCF[k]=sum(SalarioContribuicao[,k]*v)
}

###Al�quota m�dia = al�quota ideal=al�quota m�dia em k simula��es
#Pressuposto: al�quota m�dia mant�m equil�brio atuarial do plano considerando a compensa��o previdenci�ria, pois assume que contribui��es futuras s�o capazes de arcar com benef�cios futuros j� descontada a compensa��o previdenci�ria.
#boxplot(VPBF/VPCF, ylab="Valor da Al�quota")
aliquota=mean(VPBF/VPCF)
print("Al�quota de contribui��o")
print(aliquota)

return(aliquota)
}



