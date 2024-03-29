ConjugeEFilho=function (MotivoSaida,SexoQuemMorreu,IdadeQuemMorreu,ProbFamilia) {
####Avalia se deixou c�njuge ou filho menor de 21 anos

##MotivoSaida = Motivo pelo qual saiu da situa��o anterior
##SexoQuemMorreu = Sexo de quem morreu, ou seja, sexo do servidor.
##IdadeQuemMorreu = idade do servidor quando morreu.
pop=length(IdadeQuemMorreu)
aleatorio1=runif(pop, min = 0, max = 1)
aleatorio2=runif(pop, min = 0, max = 1)  #N�o posso usar o mesmo n�mero aleat�rio para avaliar probabilidade de ter c�njuge e de ter filho, porque um resultado enviesaria o outro. Pensei em usar 1-aleat�rio, mas os resultados tamb�m foram enviesados, de forma que todos os servidores deixavam algum benefici�rio. Ent�o, achei melhor fazer dois sorteios diferentes, um para filhos e outro para c�njuges.
IdadeBeneficiario=IdadeQuemMorreu
SexoBeneficiario=SexoQuemMorreu


for (i in 1:pop){
    if (MotivoSaida[i]==7){##Se morreu
       if (SexoQuemMorreu[i]==1){ ##Se � mulher
           if (aleatorio1[i]<=ProbFamilia$P.TerConj.F[IdadeQuemMorreu[i]]) {
              MotivoSaida[i]=5 ##Morreu e deixou c�njuge
              IdadeBeneficiario[i]=floor(max(18,rnorm(1,mean=3.45+0.84*IdadeQuemMorreu[i]+3.61+0.07*IdadeQuemMorreu[i], sd =6.9))) ##Estima idade do c�njuge, com idade m�nima de 18 anos, e arredonda para baixo, indicando idade completa
              SexoBeneficiario[i]=2
           }
           if (MotivoSaida[i]==7 & aleatorio2[i]<=ProbFamilia$P.TerFilho.F[IdadeQuemMorreu[i]]) {
              MotivoSaida[i]=4  ##Morreu e deixou filho menor de 21 anos
              IdadeBeneficiario[i]=floor(max(0,rnorm(1, mean =-9.314+0.443*IdadeQuemMorreu[i]+1.782, sd=4.773)))  ##Estima idade do filho. Idade m�nima=0.
              SexoBeneficiario[i]=1  ##Assume tabela de vida de menor mortalidade
              if (IdadeBeneficiario[i]>=21) {MotivoSaida[i]=6} ##Se filho tem 21 anos ou mais, n�o � benefici�rio.

           }
       }
       if (SexoQuemMorreu[i]==2){ ##Se � homem
           if (aleatorio1[i]<=ProbFamilia$P.TerConj.M[IdadeQuemMorreu[i]]) {
              MotivoSaida[i]=5 ##Morreu e deixou c�njuge
              IdadeBeneficiario[i]=floor(max(18,rnorm(1,mean=3.45+0.84*IdadeQuemMorreu[i], sd =6.9))) ##Estima idade do c�njuge, com idade m�nima de 18 anos
              SexoBeneficiario[i]=1
           }
           if (MotivoSaida[i]==7 & aleatorio2[i]<=ProbFamilia$P.TerFilho.M[IdadeQuemMorreu[i]]) {
              MotivoSaida[i]=4  ##Morreu e deixou filho menor de 21 anos
              IdadeBeneficiario[i]=floor(max(0,rnorm(1, mean =-9.314+0.443*IdadeQuemMorreu[i], sd=4.773)))  ##Estima idade do filho. Idade m�nima=0.
              if (IdadeBeneficiario[i]>=21) {MotivoSaida[i]=6}  ##Se filho tem 21 anos ou mais, n�o � benefici�rio.
              SexoBeneficiario[i]=1  ##Assume tabela de vida de menor mortalidade
           }
       }
   }
}
MotivoSaida[MotivoSaida==7]=6  ##Se n�o tem filho nem c�njuge, n�o deixou dependente
IdadeBeneficiario[IdadeBeneficiario>=110]=109    ##Ajustes idades dos benefici�rios
IdadeBeneficiario[MotivoSaida==6]=109        ##Se n�o existe benefici�rio, n�o existe idade do benefici�rio


return(list(MotivoSaida,SexoBeneficiario,IdadeBeneficiario))

}





