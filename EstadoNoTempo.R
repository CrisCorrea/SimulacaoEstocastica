EstadoNoTempo=function(DadosServidores, TMD,TabuasMorte,ProbFamilia, tempo, rodadas){
###Estima tempo até saída de cada status e indica status para o qual mudou

###Estados observados no decorrer do tempo
##1 = Ativo; 2 = Inválido, 3 - aposentado, 4=Filho beneficiário, 5=Conjuge beneficiário, 6=Morto sem sependentes, 
pop=length(DadosServidores[[1]])

#Transforma tábuas em objetos da classe lifetable
TMDf=new("lifetable",x=TMD$idade,lx=TMD$lxTotalTMD.F, name="TMD Total F")
TMDm=new("lifetable",x=TMD$idade,lx=TMD$lxTotalTMD.M)

TabuaF=new("lifetable",x=TabuasMorte$idade,lx=TabuasMorte$lxMorte.F)
TabuaM=new("lifetable",x=TabuasMorte$idade,lx=TabuasMorte$lxMorte.M)

#Estima idade de entrada
y=IdadeEntrada(DadosServidores, rodadas)  ###Idade de entrada
    ##Guarda idades de aposentadoria para cada rodada

## Estima idade em que poderia se aposentar.
IdadeAposenta=IdadeMinimaAposentadoria(DadosServidores,y, rodadas)     
r=IdadeAposenta[[1]]        

MatrizEstado=array(data = 1,  dim=c(pop, tempo+1, rodadas))        ###Atribui estado do tempo t=0 até o tempo t. Portanto, tem t+1 colunas

for(k in 1:rodadas){   ##Repete para cada rodada         

      TempoAtivo=TempoA(DadosServidores,r[,k],TMDf,TMDm)
      MSA= MotivoSaiAtividade(DadosServidores,r[,k], TempoAtivo, TMD) 
      MotivoSaiAtivo=MSA[[1]]
      SexoBeneficiario1=MSA[[2]]
      IdadeBeneficiario1=MSA[[3]]

      TempoBeneficiario1=TempoB1(MSA,DadosServidores,ProbFamilia,TabuaF, TabuaM)

      MFB=MotivoFimBeneficio(TempoBeneficiario1,IdadeBeneficiario1,SexoBeneficiario1,MotivoSaiAtivo,ProbFamilia)
      MotivoSaiBeneficiario1=MFB[[1]]
      IdadeBeneficiario2=MFB[[2]]
      SexoBeneficiario2=MFB[[3]]

      TempoBeneficiario2=TempoB2(IdadeBeneficiario2, SexoBeneficiario2,MotivoSaiBeneficiario1,TabuaF, TabuaM)


      for (j in 1:pop){
          for (t in (TempoAtivo[j]+1):(tempo+1)){
            if (t>TempoAtivo[j]+TempoBeneficiario1[j]+TempoBeneficiario2[j]+1) MatrizEstado[j,t,k]=6
            if (t<=TempoAtivo[j]+TempoBeneficiario1[j]+TempoBeneficiario2[j]+1) MatrizEstado[j,t,k]=MotivoSaiBeneficiario1[j]
            if (t<=TempoAtivo[j]+TempoBeneficiario1[j]+1) MatrizEstado[j,t,k]=MotivoSaiAtivo[j]
          }        
      }
}

#GraficosNRodadas(Estados)

return (list(MatrizEstado,y,IdadeAposenta)) 

}







