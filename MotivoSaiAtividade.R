MotivoSaiAtividade=function(DadosServidores,r, TempoAtivo, TMD) {
##Motivo pelo qual saiu da situação de ativo.
pop=length(DadosServidores[[1]])
T.extra=DadosServidores$x+TempoAtivo-r
MotivoSaiAtivo=vector(length=pop)
aleatorio=runif(pop, min = 0, max = 1)

MotivoSaiAtivo[T.extra>=0]=3  ##Aposentadoria
for (i in 1:pop){
    if (T.extra[i]<0){
        if (DadosServidores$Sexo[i]==1){          ##Feminino
           if (TMD$lxMorteTMD.F[DadosServidores$x[i]]/TMD$lxTotalTMD.F[DadosServidores$x[i]]>=aleatorio[i])MotivoSaiAtivo[i]=2 #inválido
           if (TMD$lxMorteTMD.F[DadosServidores$x[i]]/TMD$lxTotalTMD.F[DadosServidores$x[i]]<aleatorio[i])MotivoSaiAtivo[i]=7  #morto
        }
        if (DadosServidores$Sexo[i]==2){          ##Masculino
           if (TMD$lxMorteTMD.M[DadosServidores$x[i]]/TMD$lxTotalTMD.M[DadosServidores$x[i]]>=aleatorio[i])MotivoSaiAtivo[i]=2
           if (TMD$lxMorteTMD.M[DadosServidores$x[i]]/TMD$lxTotalTMD.M[DadosServidores$x[i]]<aleatorio[i])MotivoSaiAtivo[i]=7
        }
    }
}

#png("MotivoSaiAtivo.png")
#barplot(table(MotivoSaiAtivo)/length(MotivoSaiAtivo), xlab="Causa da saída do ativo",ylab="Frequência",         names=c('Invalidez', 'aposentadoria', 'morte') )
#dev.off()

IdadeSaiAtivo=DadosServidores$x+TempoAtivo
#png("IdadeSaiAtivo~MotivoSaiAtivo.png")
#boxplot(IdadeSaiAtivo~MotivoSaiAtivo, ylab="Idade de saída da atividade", xlab="Motivo da saída",names=c('Invalidez', 'Aposentadoria', 'Morte'))
#dev.off()


####Avalia se deixou cônjuge ou filho menor de 21 anos
Beneficiario1=ConjugeEFilho (MotivoSaiAtivo,DadosServidores$Sexo,IdadeSaiAtivo,ProbFamilia)
MotivoSaiAtivo=Beneficiario1[[1]]
SexoBeneficiario1=Beneficiario1[[2]]
IdadeBeneficiario1=Beneficiario1[[3]]
#png("IdadeBeneficiario1~MotivoSaiAtivo.png")
#boxplot(IdadeBeneficiario1~MotivoSaiAtivo, ylab="Idade inicial do beneficiário 1", xlab="Motivo do benefício", names=c('Invalido', 'Aposentado', 'Filho', 'Cônjuge', 'Sem Dep.'))
#dev.off()

#png("Estado beneficiário 1.png")
#barplot(table(MotivoSaiAtivo)/length(MotivoSaiAtivo), xlab="Estado beneficiário 1",ylab="Frequência",    names=c('Inv.', 'Apos.', 'Filho.', 'Conj.', 'Sem Dep.'))
#dev.off()

return(list(MotivoSaiAtivo,SexoBeneficiario1,IdadeBeneficiario1))

}




