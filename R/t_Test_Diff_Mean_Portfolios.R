#' t_Test_Diff_Mean_Portfolios
#'@description
#' Analyse the difference of means portfolio´s returns. Return p-value.

#'@param Ratio inform the metrics to analysis:
#' Rm - Average_Return - Means of Returns from each portfolio;
#' Annualized_Returns - This is the standard ratio;
#' RCum - Cumulative_Returns;
#' Annualized_Volatility - Annualized Volatility;
#' Var - Variance with 95% confidence;
#' CVar - Conditional Variance 95% confidence;
#' Sharpe - Sharpe Ratio;
#' Alpha - Jensen's Alpha;
#' Beta - Covariance between portfolio and market proxy over market proxy Variance;
#' Sortino - Sortino Ratio;
#' Treynor - Treynor Ratio;
#' Select_Arch_RCum - Select ANN's architecture.
#'@param Model 1 or 2 with or witout significance
#'
#'@param Ratio2 Standard is NULL. If informed, can assume the metrics to analysis.
#'@examples
#' t_Test_Diff_Mean_Portfolios(Ratio="Treynor", Ratio2="Var", Model=1)

#'@export
t_Test_Diff_Mean_Portfolios<-function(Ratio="Annualized_Returns", Ratio2="", Model=1){

  ################################################################################
  # Graphic Annualized Returns
  library(writexl)
  library(stringr)
  library(dplyr)
  Dados_teste=NULL
  if(Ratio=="RCum"){
    load('~/Comparativo_RCum_Horizon_Anual.rda')
    Dados_teste=Comparativo_RCum_Horizon_Anual
  }
  if(Ratio=="Annualized_Volatility"){
    load('~/Comparativo_Volatility_Horizon_Anual.rda')
    Dados_teste=Comparativo_Volatility_Horizon_Anual
  }
  if(Ratio=="Annualized_Returns"){
    load('~/Comparativo_RETORNOS_Horizon_Anual.rda')
    Dados_teste=Comparativo_RETORNOS_Horizon_Anual
  }
    if(Ratio=="Sharpe"){
      load('~/Comparativo_Sharpe_Horizon_Anual.rda')
      Dados_teste=Comparativo_Sharpe_Horizon_Anual
    }
  if(Ratio=="Alpha"){
    load('~/Comparativo_Alpha_Horizon_Anual.rda')
    Dados_teste=Comparativo_Alpha_Horizon_Anual
  }
  if(Ratio=="Beta"){
    load('~/Comparativo_Beta_Horizon_Anual.rda')
    Dados_teste=Comparativo_Beta_Horizon_Anual
  }
  if(Ratio=="Sortino"){
    load('~/Comparativo_Sortino_Horizon_Anual.rda')
    Dados_teste=Comparativo_Sortino_Horizon_Anual
  }
  if(Ratio=="Treynor"){
    load('~/Comparativo_Treynor_Horizon_Anual.rda')
    Dados_teste=Comparativo_Treynor_Horizon_Anual
  }
  if(Ratio=="Var"){
    load('~/Comparativo_Var_Horizon_Anual.rda')
    Dados_teste=Comparativo_Var_Horizon_Anual
  }
  if(Ratio=="CVar"){
    load('~/Comparativo_CVar_Horizon_Anual.rda')
    Dados_teste=Comparativo_CVar_Horizon_Anual
  }
  if(Ratio=="Rm"){
    load('~/Comparativo_Rm_Horizon_Anual.rda')
    Dados_teste=Comparativo_Rm_Horizon_Anual
  }
  if(Ratio=="Select_Arch_RCum"){
    load('~/Select_Arch_RCum_ANNt_Sharpe.rda')
    Dados_teste=as.matrix(Select_Arch_RCum_ANNt_Sharpe)
  }


  if(Ratio2==""){

    #Dados_teste2=matrix(nrow=nrow(Dados_teste),ncol=ncol(Dados_teste) )
    Linhas_Colunas=as.numeric(ncol(Dados_teste))
    Dados_teste2 = matrix(nrow=Linhas_Colunas, ncol=Linhas_Colunas)
    Dados_teste2[]=0
  }
  if(Ratio2=="RCum"){
    load('~/Comparativo_RCum_Horizon_Anual.rda')
    Dados_teste2=Comparativo_RCum_Horizon_Anual
  }
  if(Ratio2=="Annualized_Volatility"){
    load('~/Comparativo_Volatility_Horizon_Anual.rda')
    Dados_teste2=Comparativo_Volatility_Horizon_Anual
  }
  if(Ratio2=="Annualized_Returns"){
    load('~/Comparativo_RETORNOS_Horizon_Anual.rda')
    Dados_teste2=Comparativo_RETORNOS_Horizon_Anual
  }
  if(Ratio2=="Sharpe"){
    load('~/Comparativo_Sharpe_Horizon_Anual.rda')
    Dados_teste2=Comparativo_Sharpe_Horizon_Anual
  }
  if(Ratio2=="Alpha"){
    load('~/Comparativo_Alpha_Horizon_Anual.rda')
    Dados_teste2=Comparativo_Alpha_Horizon_Anual
  }
  if(Ratio2=="Beta"){
    load('~/Comparativo_Beta_Horizon_Anual.rda')
    Dados_teste2=Comparativo_Beta_Horizon_Anual
  }
  if(Ratio2=="Sortino"){
    load('~/Comparativo_Sortino_Horizon_Anual.rda')
    Dados_teste2=Comparativo_Sortino_Horizon_Anual
  }
  if(Ratio2=="Treynor"){
    load('~/Comparativo_Treynor_Horizon_Anual.rda')
    Dados_teste2=Comparativo_Treynor_Horizon_Anual
  }
  if(Ratio2=="Var"){
    load('~/Comparativo_Var_Horizon_Anual.rda')
    Dados_teste2=Comparativo_Var_Horizon_Anual
  }
  if(Ratio2=="CVar"){
    load('~/Comparativo_CVar_Horizon_Anual.rda')
    Dados_teste2=Comparativo_CVar_Horizon_Anual
  }
  if(Ratio2=="Rm"){
    load('~/Comparativo_Rm_Horizon_Anual.rda')
    Dados_teste2=Comparativo_Rm_Horizon_Anual
  }
  if(Ratio2=="Select_Arch_RCum"){
    load('~/Select_Arch_RCum_ANNt_Sharpe.rda')
    Dados_teste2=as.data.frame(Select_Arch_RCum_ANNt_Sharpe)
  }
  if(Ratio2=="Select_Arch_Volatility"){
    load('~/Select_Arch_Volatility_ANNt_Sharpe.rda')
    Dados_teste2=Select_Arch_Volatility_ANNt_Sharpe
  }
#####
  if (Model==1){
    Linhas_Colunas=as.numeric(ncol(Dados_teste))
    Comparativo_t_test = matrix(nrow=Linhas_Colunas, ncol=Linhas_Colunas)
    col_names=colnames(Dados_teste)
    colnames(Comparativo_t_test)=col_names
    rownames(Comparativo_t_test)=colnames(Dados_teste)


    for (i in (1:(ncol(Dados_teste)))){
      for (j in (1:ncol(Dados_teste))){
        if(i<=j){
          Comparativo_t_test[j,i]=t.test(Dados_teste[,i],Dados_teste[,j])$p.value
         } else {
          Comparativo_t_test[j,i]=t.test(Dados_teste2[,i],Dados_teste2[,j])$p.value
        }

      }
    }
    Comparativo_t_test[is.nan(Comparativo_t_test)]=NA
    Comparativo_t_test[Comparativo_t_test=="NaN"]=NA
    options(scipen=999)
    Comparativo_t_test=format(round(Comparativo_t_test,3), nsmall=4)
    Comparativo_t_test=as.data.frame(Comparativo_t_test)
    Comparativo_t_test[is.na(Comparativo_t_test)]=as.character('')
    View(Comparativo_t_test)
    nome_ttest=paste("~/Comparativo_t_test_", Ratio,"_vs_", Ratio2,".rda", sep="")
    nome_ttest_Tab=paste("~/Comparativo_t_test_", Ratio,"_vs_", Ratio2,".xlsx", sep="")
    save(Comparativo_t_test, file=nome_ttest)
    t_test= colnames(as.data.frame(Dados_teste))
    Comparativo_t_test_Tabela=as.data.frame(cbind(t_test,Comparativo_t_test))
    write_xlsx(Comparativo_t_test_Tabela, nome_ttest_Tab)
  }

  ######
if (Model==2){
  Linhas=as.numeric(ncol(Dados_teste))
  Comparativo_t_test = matrix(nrow=Linhas, ncol=2*Linhas)
  col_names=colnames(Dados_teste)
  Insert=rep('',2*ncol(Dados_teste))
  for(i in 1:length(col_names)){
    Insert[i*2-1]=col_names[i]
  }
  colnames(Comparativo_t_test)=Insert
  rownames(Comparativo_t_test)=colnames(Dados_teste)


    for (i in (1:(ncol(Dados_teste)))){
    for (j in (1:ncol(Dados_teste))){
      if(i<=j){
        Comparativo_t_test[j,i*2-1]=t.test(Dados_teste[,i],Dados_teste[,j])$p.value
        if (Comparativo_t_test[j,i*2-1]<0.1){
          Comparativo_t_test[j,i*2]=0.1
        }
        if (Comparativo_t_test[j,i*2-1]<0.05){
          Comparativo_t_test[j,i*2]=0.05
        }
        if (Comparativo_t_test[j,i*2-1]<0.01){
          Comparativo_t_test[j,i*2]=0.01
        }

      } else {
        Comparativo_t_test[j,i*2-1]=t.test(Dados_teste2[,i],Dados_teste2[,j])$p.value
        if (Comparativo_t_test[j,i*2-1]<0.1){
          Comparativo_t_test[j,i*2]=0.1
        }
        if (Comparativo_t_test[j,i*2-1]<0.05){
          Comparativo_t_test[j,i*2]=0.05
        }
        if (Comparativo_t_test[j,i*2-1]<0.01){
          Comparativo_t_test[j,i*2]=0.01
        }

      }

    }
  }
  Comparativo_t_test[is.nan(Comparativo_t_test)]=NA
  Comparativo_t_test[Comparativo_t_test=="NaN"]=NA
  options(scipen=999)
  Comparativo_t_test=format(round(Comparativo_t_test,3), nsmall=3)
  for (i in (1:(ncol(Dados_teste)))){
    for (j in (1:ncol(Dados_teste))){
      if (Comparativo_t_test[j,i*2-1]<0.1){
        Comparativo_t_test[j,i*2]="*"
      }
      if (Comparativo_t_test[j,i*2-1]<0.05){
        Comparativo_t_test[j,i*2]="**"
      }
      if (Comparativo_t_test[j,i*2-1]<0.01){
        Comparativo_t_test[j,i*2]="***"
      }
      #if (Comparativo_t_test[j,i*2-1]<0.00001){
      #Comparativo_t_test[j,i*2-1]=round(0.0000,4)
      #}
    }}
  Comparativo_t_test=as.data.frame(Comparativo_t_test)
  Comparativo_t_test[is.na(Comparativo_t_test)]=as.character('')
  View(Comparativo_t_test)
  nome_ttest=paste("~/Comparativo_t_test_", Ratio,"_vs_", Ratio2,".rda", sep="")
  nome_ttest_Tab=paste("~/Comparativo_t_test_", Ratio,"_vs_", Ratio2,".xlsx", sep="")
  save(Comparativo_t_test, file=nome_ttest)
  t_test= colnames(as.data.frame(Dados_teste))
  Comparativo_t_test_Tabela=as.data.frame(cbind(t_test,Comparativo_t_test))
  write_xlsx(Comparativo_t_test_Tabela, nome_ttest_Tab)
}
  ################################################################################
}
