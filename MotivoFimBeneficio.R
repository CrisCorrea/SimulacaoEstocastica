MotivoFimBeneficio=function(TempoBeneficiario1,IdadeBeneficiario1,SexoBeneficiario1,MotivoSaiAtivo,ProbFamilia){

pop=length(IdadeBeneficiario1)
###Se era aposentado ou inv�lido e morreu, pode ter deixado conjuge e filho.
####Avalia se deixou c�njuge ou filho menor de 21 anos
MotivoSaiBeneficiario1=vector(length=pop)
MotivoSaiBeneficiario1[MotivoSaiAtivo<=3]=7   ##Motivo pelo qual deixou de ser benefici�rio. 7 - morreu. 4-morreu e deixou filho; 5 - morreu e deixou conjuge, 6 - morreu e n�o deixou dependentes.
MotivoSaiBeneficiario1[MotivoSaiAtivo>3]=6
IdadeMorteBeneficiario1=IdadeBeneficiario1+TempoBeneficiario1
Beneficiario2=ConjugeEFilho(MotivoSaiBeneficiario1,SexoBeneficiario1,IdadeMorteBeneficiario1,ProbFamilia)
MotivoSaiBeneficiario1=Beneficiario2[[1]]
SexoBeneficiario2=Beneficiario2[[2]]
IdadeBeneficiario2=Beneficiario2[[3]]
#png("IdadeBeneficiario2 x MotivoSaiBeneficiario1.png")
#boxplot(IdadeBeneficiario2~MotivoSaiBeneficiario1, ylab="Idade inicial do benefici�rio 2", xlab="Motivo do benef�cio", names=c('Filho.', 'Conj.', 'Sem  Dep.'))
#dev.off()

return(list(MotivoSaiBeneficiario1,IdadeBeneficiario2,SexoBeneficiario2))
}




