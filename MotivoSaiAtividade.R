MotivoSaiAtividade=function(DadosServidores,r, TempoAtivo, TMD) {
##Motivo pelo qual saiu da situa��o de ativo.
pop=length(DadosServidores[[1]])
T.extra=DadosServidores$x+TempoAtivo-r
MotivoSaiAtivo=vector(length=pop)
aleatorio=runif(pop, min = 0, max = 1)

MotivoSaiAtivo[T.extra>=0]=3  ##Aposentadoria
for (i in 1:pop){
    if (T.extra[i]<0){
        if (DadosServidores$Sexo[i]==1){          ##Feminino
           if (TMD$lxMorteTMD.F[DadosServidores$x[i]]/TMD$lxTotalTMD.F[DadosServidores$x[i]]>=aleatorio[i])MotivoSaiAtivo[i]=2 #inv�lido
           if (TMD$lxMorteTMD.F[DadosServidores$x[i]]/TMD$lxTotalTMD.F[DadosServidores$x[i]]<aleatorio[i])MotivoSaiAtivo[i]=7  #morto
        }
        if (DadosServidores$Sexo[i]==2){          ##Masculino
           if (TMD$lxMorteTMD.M[DadosServidores$x[i]]/TMD$lxTotalTMD.M[DadosServidores$x[i]]>=aleatorio[i])MotivoSaiAtivo[i]=2
           if (TMD$lxMorteTMD.M[DadosServidores$x[i]]/TMD$lxTotalTMD.M[DadosServidores$x[i]]<aleatorio[i])MotivoSaiAtivo[i]=7
        }
    }
}

#png("MotivoSaiAtivo.png")
#barplot(table(MotivoSaiAtivo)/length(MotivoSaiAtivo), xlab="Causa da sa�da do ativo",ylab="Frequ�ncia",         names=c('Invalidez', 'aposentadoria', 'morte') )
#dev.off()

IdadeSaiAtivo=DadosServidores$x+TempoAtivo
#png("IdadeSaiAtivo~MotivoSaiAtivo.png")
#boxplot(IdadeSaiAtivo~MotivoSaiAtivo, ylab="Idade de sa�da da atividade", xlab="Motivo da sa�da",names=c('Invalidez', 'Aposentadoria', 'Morte'))
#dev.off()


####Avalia se deixou c�njuge ou filho menor de 21 anos
Beneficiario1=ConjugeEFilho (MotivoSaiAtivo,DadosServidores$Sexo,IdadeSaiAtivo,ProbFamilia)
MotivoSaiAtivo=Beneficiario1[[1]]
SexoBeneficiario1=Beneficiario1[[2]]
IdadeBeneficiario1=Beneficiario1[[3]]
#png("IdadeBeneficiario1~MotivoSaiAtivo.png")
#boxplot(IdadeBeneficiario1~MotivoSaiAtivo, ylab="Idade inicial do benefici�rio 1", xlab="Motivo do benef�cio", names=c('Invalido', 'Aposentado', 'Filho', 'C�njuge', 'Sem Dep.'))
#dev.off()

#png("Estado benefici�rio 1.png")
#barplot(table(MotivoSaiAtivo)/length(MotivoSaiAtivo), xlab="Estado benefici�rio 1",ylab="Frequ�ncia",    names=c('Inv.', 'Apos.', 'Filho.', 'Conj.', 'Sem Dep.'))
#dev.off()

return(list(MotivoSaiAtivo,SexoBeneficiario1,IdadeBeneficiario1))

}




