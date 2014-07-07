ConjugeEFilho=function (MotivoSaida,SexoQuemMorreu,IdadeQuemMorreu,ProbFamilia) {
####Avalia se deixou cônjuge ou filho menor de 21 anos

##MotivoSaida = Motivo pelo qual saiu da situação anterior
##SexoQuemMorreu = Sexo de quem morreu, ou seja, sexo do servidor.
##IdadeQuemMorreu = idade do servidor quando morreu.
pop=length(IdadeQuemMorreu)
aleatorio1=runif(pop, min = 0, max = 1)
aleatorio2=runif(pop, min = 0, max = 1)  #Não posso usar o mesmo número aleatório para avaliar probabilidade de ter cônjuge e de ter filho, porque um resultado enviesaria o outro. Pensei em usar 1-aleatório, mas os resultados também foram enviesados, de forma que todos os servidores deixavam algum beneficiário. Então, achei melhor fazer dois sorteios diferentes, um para filhos e outro para cônjuges.
IdadeBeneficiario=IdadeQuemMorreu
SexoBeneficiario=SexoQuemMorreu


for (i in 1:pop){
    if (MotivoSaida[i]==7){##Se morreu
       if (SexoQuemMorreu[i]==1){ ##Se é mulher
           if (aleatorio1[i]<=ProbFamilia$P.TerConj.F[IdadeQuemMorreu[i]]) {
              MotivoSaida[i]=5 ##Morreu e deixou cônjuge
              IdadeBeneficiario[i]=floor(max(18,rnorm(1,mean=3.45+0.84*IdadeQuemMorreu[i]+3.61+0.07*IdadeQuemMorreu[i], sd =6.9))) ##Estima idade do cônjuge, com idade mínima de 18 anos, e arredonda para baixo, indicando idade completa
              SexoBeneficiario[i]=2
           }
           if (MotivoSaida[i]==7 & aleatorio2[i]<=ProbFamilia$P.TerFilho.F[IdadeQuemMorreu[i]]) {
              MotivoSaida[i]=4  ##Morreu e deixou filho menor de 21 anos
              IdadeBeneficiario[i]=floor(max(0,rnorm(1, mean =-9.314+0.443*IdadeQuemMorreu[i]+1.782, sd=4.773)))  ##Estima idade do filho. Idade mínima=0.
              SexoBeneficiario[i]=1  ##Assume tabela de vida de menor mortalidade
              if (IdadeBeneficiario[i]>=21) {MotivoSaida[i]=6} ##Se filho tem 21 anos ou mais, não é beneficiário.

           }
       }
       if (SexoQuemMorreu[i]==2){ ##Se é homem
           if (aleatorio1[i]<=ProbFamilia$P.TerConj.M[IdadeQuemMorreu[i]]) {
              MotivoSaida[i]=5 ##Morreu e deixou cônjuge
              IdadeBeneficiario[i]=floor(max(18,rnorm(1,mean=3.45+0.84*IdadeQuemMorreu[i], sd =6.9))) ##Estima idade do cônjuge, com idade mínima de 18 anos
              SexoBeneficiario[i]=1
           }
           if (MotivoSaida[i]==7 & aleatorio2[i]<=ProbFamilia$P.TerFilho.M[IdadeQuemMorreu[i]]) {
              MotivoSaida[i]=4  ##Morreu e deixou filho menor de 21 anos
              IdadeBeneficiario[i]=floor(max(0,rnorm(1, mean =-9.314+0.443*IdadeQuemMorreu[i], sd=4.773)))  ##Estima idade do filho. Idade mínima=0.
              if (IdadeBeneficiario[i]>=21) {MotivoSaida[i]=6}  ##Se filho tem 21 anos ou mais, não é beneficiário.
              SexoBeneficiario[i]=1  ##Assume tabela de vida de menor mortalidade
           }
       }
   }
}
MotivoSaida[MotivoSaida==7]=6  ##Se não tem filho nem cônjuge, não deixou dependente
IdadeBeneficiario[IdadeBeneficiario>=110]=109    ##Ajustes idades dos beneficiários
IdadeBeneficiario[MotivoSaida==6]=109        ##Se não existe beneficiário, não existe idade do beneficiário


return(list(MotivoSaida,SexoBeneficiario,IdadeBeneficiario))

}





