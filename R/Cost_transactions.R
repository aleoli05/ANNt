#' Cost_transactions
#'@description
#' Penalize the return of portfolios with the cost transactions

#'@param Cost Percentual of cost transactions consider. This number will be multiplied for 2 because we need sell and buy.
#' @examples
#' Const_transactions(Cost=0.05)
#' @export

Cost_transactions<-function(Cost=0.05){
  library(stringr)
  library(writexl)
  load('~/Comparativo_RCum_Horizon_Anual.rda')
  Dados_teste=Comparativo_RCum_Horizon_Anual[nrow(Comparativo_RCum_Horizon_Anual):1,]
  Turnovers('Alls')
  load('~/Turnovers_Alls.rda')
  Cost_Medio_matrix=matrix(nrow=3,ncol=ncol(Dados_teste))
  colnames(Cost_Medio_matrix)=colnames(Dados_teste)
  rownames(Cost_Medio_matrix)=c('RCum_without_Cost', 'Turnover', 'RCum_with_Cost')
  Cost_matrix=Dados_teste

  for (i in (2:ncol(Cost_matrix))){
    h=1
    for (j in (2:nrow(Cost_matrix))){
      Cost_matrix[j,i]=Dados_teste[j,i]*(1-as.numeric(Turnover_Alls[j+h,i+1])*2*Cost)
      h=h+1
    }
  }
  Cost_Medio_matrix[1,1]=mean(Dados_teste[,1])
  Cost_Medio_matrix[2,1]=0
  Cost_Medio_matrix[3,1]=mean(Dados_teste[,1])
  for (i in (2:ncol(Cost_matrix))){
    Cost_Medio_matrix[1,i]=round(mean(Dados_teste[,i]),2)
    Cost_Medio_matrix[2,i]=Turnover_Alls[nrow(Turnover_Alls),i+1]
    Cost_Medio_matrix[3,i]=round(mean(Cost_matrix[,i]),2)
  }
  Cost_matrix=as.data.frame(round(Cost_matrix,2))
  Cost_Medio_matrix=as.data.frame(Cost_Medio_matrix)
  save(Cost_matrix,file="~/Cost_matrix.rda")
  t_test= colnames(as.data.frame(Cost_matrix))
  Cost_matrix_Tabela=as.data.frame(cbind(t_test,Cost_matrix))
  write_xlsx(Cost_matrix, "~/Cost_matrix_Tabela.xlsx")
  save(Cost_Medio_matrix,file="~/Cost_Medio_matrix.rda")
  write_xlsx(Cost_Medio_matrix, "~/Cost_Medio_matrix.xlsx")
  View(Cost_matrix)
  print(Cost_Medio_matrix)
}
