#' Turnovers
#' It measures portfolio turnover
#' @param Portfolios: Specify the portfolios or type 'Alls' for all.
#' Portfolios:'MF_EQ', 'MF_MKW','MKW','ANNt_EQ','ANNt_MKW','Sharpe','MF_Sharpe','ANNt_Sharpe',
#' @examples
#' Turnovers('Alls')

#' @export

Turnovers<-function(Portfolio){

library(dplyr)
  if (length(Portfolio)==1){
  if (Portfolio=='Alls'){
      Portfolio=c('MF_EQ', 'MF_MKW','MKW','ANNt_EQ','ANNt_MKW','Sharpe','MF_Sharpe',
                  'ANNt_Sharpe')
      load('~/Weights_MF_EQ_Horizon.rda')
  }}
if (length(Portfolio)==1){
    options(warn=-1)
    if(Portfolio=='MF_EQ'){
    load('~/Weights_MF_EQ_Horizon.rda')
    dados=(Weights_MF_EQ_Horizon)
    View(Weights_MF_EQ_Horizon)
    }
  if(Portfolio=='MF_MKW'){
    load('~/Weights_MF_MKW_Horizon.rda')
    dados=Weights_MF_MKW_Horizon
    View(Weights_MF_MKW_Horizon)
  }
  if(Portfolio=='MKW'){
    load('~/Weights_MKW_Horizon.rda')
    dados=Weights_MKW_Horizon
    View(Weights_MKW_Horizon)
  }
  if(Portfolio=='ANNt_EQ'){
    load('~/Weights_ANNt_EQ_Horizon.rda')
    dados=Weights_ANNt_EQ_Horizon
    View(Weights_ANNt_EQ_Horizon)
  }
  if(Portfolio=='ANNt_MKW'){
    load('~/Weights_ANNt_MKW_Horizon.rda')
    dados=Weights_ANNt_MKW_Horizon
    View(Weights_ANNt_MKW_Horizon)
  }
  if(Portfolio=='Sharpe'){
    load('~/Weights_Sharpe_Horizon.rda')
    dados=Weights_Sharpe_Horizon
    View(Weights_Sharpe_Horizon)
  }
  if(Portfolio=='MF_Sharpe'){
    load('~/Weights_MF_Sharpe_Horizon.rda')
    dados=Weights_MF_Sharpe_Horizon
    View(Weights_MF_Sharpe_Horizon)
  }
  if(Portfolio=='ANNt_Sharpe'){
    load('~/Weights_ANNt_Sharpe_Horizon.rda')
    dados=Weights_ANNt_Sharpe_Horizon
    View(Weights_ANNt_Sharpe_Horizon)
  }

  Linhas_CV=(nrow(dados)-2)
  Linhas=seq(from=3, to=nrow(dados)-1, by=2)
  Colunas = 50
  Compras=matrix(nrow=Linhas_CV, ncol=Colunas)
  Vendas=matrix(nrow=Linhas_CV, ncol=Colunas)
  ordem=0
  n_buys=0
  Buy=NULL
  Sell=NULL
    for (i in Linhas){
        for (j in 3:Colunas){
          if (is.na(dados[i,j])==FALSE && is.na(dados[i+2,1])==FALSE){
            test=dados[i+2,3:ncol(dados)]
            excluir=names(test)[colSums(is.na(test)) > 0]
            test <- test %>%
              select(-all_of(excluir))
            if (any(unlist(test)==dados[i,j])){
                ordem=which(dados[i+2,]==dados[i,j])
                if(as.numeric(dados[i+1,j])>as.numeric(dados[i+3,ordem])){
                   Compras[i+1,j] = as.numeric(dados[i+1,j])-as.numeric(dados[i+3,ordem])
                  }else{
                     Vendas[i+1,j] = as.numeric(dados[i+3,ordem])-as.numeric(dados[i+1,j])
                    }
            }
            teste=dados[i+2,3:ncol(dados)]
            excluir=names(teste)[colSums(is.na(teste)) > 0]
            teste <- teste %>%
              select(-all_of(excluir))
            if (is.na(dados[i,j])==FALSE && any(unlist(teste)==as.character(dados[i,j]))==FALSE){
              Compras[i+1,j] = as.numeric(dados[i+1,j])
            }
            teste2=dados[i,3:ncol(dados)]
            excluir=names(teste2)[colSums(is.na(teste2)) > 0]
            teste2 <- teste2 %>%
              select(-all_of(excluir))
            if (is.na(dados[i+2,j])==FALSE && any(unlist(teste2)==as.character(dados[i+2,j]))==FALSE){
              Vendas[i+1,j] = as.numeric(dados[i+1,j])
            }
          }
        }
      if(is.na(dados[i+2,1])==FALSE){
      Compras2=Compras%>% replace(is.na(.), 0)
      Vendas2=Vendas%>% replace(is.na(.), 0)
      #Compras2[i+1,][is.na(Compras2[i+1])] <-0
      n_buys=n_buys+1
      Buy[n_buys]=sum(Compras2[i+1,])
      Sell[n_buys]=sum(Vendas2[i+1,])
      }
    }
  if(sum(Buy)>sum(Sell)){
  Turn=Buy
  }else{
    Turn=Sell
  }
  Turn[Turn>1]=1
  Media=round(mean(Turn),2)
  if(Media>1){
    Media=1
  }
  Turnover=dados[,1:2]
  posicao_=seq(from=1, to=(2*length(Turn)-1), by=2)
  elemento=0
  Lista=Turn
  for(i in 1:length(posicao_)){
    posicao=posicao_[i]
  Lista= append(Lista, elemento, after = posicao)
  }
  elemento2=c('Turnover',0)
  Lista=append(Lista, elemento2, after = 0)
  Posicao3=length(Lista)+2
  elemento3=NA
  for(i in length(Lista):(nrow(Turnover)-1)){
    posicao=posicao[i]
    Lista= append(Lista, elemento3)
  }
  #Lista[Posicao3-1]=0
  Lista[Posicao3-1]='Mean'
  Lista[Posicao3]=Media
  Turnover[,3]=Lista
  Turnover=na.omit(Turnover)
  View(Turnover)
  print(Turnover)
}

if (length(Portfolio)>1){
   Port=Portfolio
   load('~/Weights_MF_EQ_Horizon.rda')
dados=Weights_MF_EQ_Horizon
Turnover_Alls=dados[,1:2]
Turnover_Alls[1,1]='Portfolios'
Turnover_Alls=na.omit(Turnover_Alls)


for (m in 1:length(Port)){
  Portfolio=Port[m]

  library(dplyr)
  options(warn=-1)
  if(Portfolio=='MF_EQ'){
    load('~/Weights_MF_EQ_Horizon.rda')
    dados=(Weights_MF_EQ_Horizon)
    View(Weights_MF_EQ_Horizon)
  }
  if(Portfolio=='MF_MKW'){
    load('~/Weights_MF_MKW_Horizon.rda')
    dados=Weights_MF_MKW_Horizon
    View(Weights_MF_MKW_Horizon)
  }
  if(Portfolio=='MKW'){
    load('~/Weights_MKW_Horizon.rda')
    dados=Weights_MKW_Horizon
    View(Weights_MKW_Horizon)
  }
  if(Portfolio=='ANNt_EQ'){
    load('~/Weights_ANNt_EQ_Horizon.rda')
    dados=Weights_ANNt_EQ_Horizon
    View(Weights_ANNt_EQ_Horizon)
  }
  if(Portfolio=='ANNt_MKW'){
    load('~/Weights_ANNt_MKW_Horizon.rda')
    dados=Weights_ANNt_MKW_Horizon
    View(Weights_ANNt_MKW_Horizon)
  }
  if(Portfolio=='Sharpe'){
    load('~/Weights_Sharpe_Horizon.rda')
    dados=Weights_Sharpe_Horizon
    View(Weights_Sharpe_Horizon)
  }
  if(Portfolio=='MF_Sharpe'){
    load('~/Weights_MF_Sharpe_Horizon.rda')
    dados=Weights_MF_Sharpe_Horizon
    View(Weights_MF_Sharpe_Horizon)
  }
  if(Portfolio=='ANNt_Sharpe'){
    load('~/Weights_ANNt_Sharpe_Horizon.rda')
    dados=Weights_ANNt_Sharpe_Horizon
    View(Weights_ANNt_Sharpe_Horizon)
  }


  Linhas_CV=(nrow(dados)-2)
  Linhas=seq(from=3, to=nrow(dados)-1, by=2)
  Colunas = 50
  Compras=matrix(nrow=Linhas_CV, ncol=Colunas)
  Vendas=matrix(nrow=Linhas_CV, ncol=Colunas)
  ordem=0
  n_buys=0
  Buy=NULL
  Sell=NULL
  for (i in Linhas){
    for (j in 3:Colunas){
      if (is.na(dados[i,j])==FALSE && is.na(dados[i+2,1])==FALSE){
        test=dados[i+2,3:ncol(dados)]
        excluir=names(test)[colSums(is.na(test)) > 0]
        test <- test %>%
          select(-all_of(excluir))
        if (any(unlist(test)==dados[i,j])){
          ordem=which(dados[i+2,]==dados[i,j])
          if(as.numeric(dados[i+1,j])>as.numeric(dados[i+3,ordem])){
            Compras[i+1,j] = as.numeric(dados[i+1,j])-as.numeric(dados[i+3,ordem])
          }else{
            Vendas[i+1,j] = as.numeric(dados[i+3,ordem])-as.numeric(dados[i+1,j])
          }
        }
        teste=dados[i+2,3:ncol(dados)]
        excluir=names(teste)[colSums(is.na(teste)) > 0]
        teste <- teste %>%
          select(-all_of(excluir))
        if (is.na(dados[i,j])==FALSE && any(unlist(teste)==as.character(dados[i,j]))==FALSE){
          Compras[i+1,j] = as.numeric(dados[i+1,j])
        }
        teste2=dados[i,3:ncol(dados)]
        excluir=names(teste2)[colSums(is.na(teste2)) > 0]
        teste2 <- teste2 %>%
          select(-all_of(excluir))
        if (is.na(dados[i+2,j])==FALSE && any(unlist(teste2)==as.character(dados[i+2,j]))==FALSE){
          Vendas[i+1,j] = as.numeric(dados[i+1,j])
        }
      }
    }
    if(is.na(dados[i+2,1])==FALSE){
      Compras2=Compras%>% replace(is.na(.), 0)
      Vendas2=Vendas%>% replace(is.na(.), 0)
      #Compras2[i+1,][is.na(Compras2[i+1])] <-0
      n_buys=n_buys+1
      Buy[n_buys]=sum(Compras2[i+1,])
      Sell[n_buys]=sum(Vendas2[i+1,])
    }
  }
  if(sum(Buy)>sum(Sell)){
    Turn=Buy
  }else{
    Turn=Sell
  }
  Turn[Turn>1]=1
  Media=round(mean(Turn),2)
  if(Media>1){
    Media=1
  }
  Turnover=dados[,1:2]
  posicao_=seq(from=1, to=(2*length(Turn)-1), by=2)
  elemento=0
  Lista=Turn
  for(i in 1:length(posicao_)){
    posicao=posicao_[i]
    Lista= append(Lista, elemento, after = posicao)
  }
  elemento2=c(Portfolio,'Turnover')
  Lista=append(Lista, elemento2, after = 0)
  Posicao3=length(Lista)+2
  elemento3=NA
  for(i in length(Lista):(nrow(Turnover)-1)){
    posicao=posicao[i]
    Lista= append(Lista, elemento3)
  }
  #Lista[Posicao3-1]=0
  Lista[Posicao3-1]='Mean'
  Lista[Posicao3]=Media
  Turnover[,3]=Lista
  Turnover=na.omit(Turnover)
  View(Turnover)
  print(Turnover)

  Turnover_Alls[,2+m]=Turnover[,3]
  }
  View(Turnover_Alls)
  print(Turnover_Alls)
 }
}
