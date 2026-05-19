#' Portfolio_backtesting_Inv
#' Calculate the mean portfolio backtest to the investment perod.
#'
#' @param Date_Initial_Backtesting Date initial of the backtest
#' @param Date_Final_Backtesting Date final of the backtest
#' @examples
#' Date_Initial_Backtesting =''
#' Date_Final_Backtesting =''
#' Portfolio_backtesting_Inv('', '')
#'
#' @export
Portfolio_backtesting_Inv <- function(Date_Initial_Backtesting,Date_Final_Backtesting) {

  library(dplyr)
  library(writexl)
  options(warn=-1)
  load('~/Comparativo_Rm_Horizon_Anual.rda')
  load('~/Comparativo_RETORNOS_Horizon_Anual.rda')
  load('~/Comparativo_RCum_Horizon_Anual.rda')
  load('~/Comparativo_Alpha_Horizon_Anual.rda')
  load('~/Comparativo_Sharpe_Horizon_Anual.rda')
  load('~/Comparativo_Sortino_Horizon_Anual.rda')
  load('~/Comparativo_Treynor_Horizon_Anual.rda')
  load('~/Comparativo_Beta_Horizon_Anual.rda')
  load('~/Comparativo_Volatility_Horizon_Anual.rda')
  load('~/Comparativo_Var_Horizon_Anual.rda')
  load('~/Comparativo_CVar_Horizon_Anual.rda')





  load('~/RM.rda')
  load('~/Rf.rda')

  sumbacktest_Inv <- matrix(nrow=11, ncol=9)
  colnames(sumbacktest_Inv)= c( RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                            "ANNt_EQ", "ANNt_MKW","ANNt_SHARPE")
  rownames(sumbacktest_Inv) = c("Average Return (% p.d.)","Annualized Return (% p.a.)",
                            "Cumulative Return (% p.p.)", "Jensen´s Alpha (% p.d.)",
                            "Sharpe Ratio (Dimensionless)", "Sortino Ratio",
                            "Treynor (% p.d.)","CAPM Beta",
                            "Annualized Volatility (% p.a.)",
                            "VaR 95% (% p.d.)", "CVaR 95% (% p.d.)")


  Parameters= list(  Comparativo_Rm_Horizon_Anual,
                     Comparativo_RETORNOS_Horizon_Anual,
                     Comparativo_RCum_Horizon_Anual,
                     Comparativo_Alpha_Horizon_Anual,
                     Comparativo_Sharpe_Horizon_Anual,
                     Comparativo_Sortino_Horizon_Anual,
                     Comparativo_Treynor_Horizon_Anual,
                     Comparativo_Beta_Horizon_Anual,
                     Comparativo_Volatility_Horizon_Anual,
                     Comparativo_Var_Horizon_Anual,
                     Comparativo_CVar_Horizon_Anual
                  )

  for (i in (1:length(Parameters))){

      Parameter=data.frame(Parameters[i])


      if(Date_Initial_Backtesting==''){
        Corte1=1
      } else{

        if(length(which(rownames(Parameter)==Date_Initial_Backtesting))==0){
          while(length(which(rownames(Parameter)==Date_Initial_Backtesting))==0){
            dia=as.Date(Date_Initial_Backtesting)
            new_day=dia+1
            Date_Initial_Backtesting = as.character(new_day)
          }
        }


        Corte1=which(rownames(as.data.frame(Parameter))==Date_Initial_Backtesting)
      }


      if(Date_Final_Backtesting==''){
        Corte2=nrow(Parameter)
      } else{


        if(length(which(rownames(Parameter)==Date_Final_Backtesting))==0){
          while(length(which(rownames(Parameter)==Date_Final_Backtesting))==0){
            dia=as.Date(Date_Final_Backtesting)
            new_day=dia-1
            Date_Final_Backtesting = as.character(new_day)
          }
        }

        Corte2=which(rownames(as.data.frame(Parameter))==Date_Final_Backtesting)
      }
      Parameter=Parameter[Corte1:Corte2,]
      attach(as.data.frame(Parameter))

      sumbacktest_Inv[i,]= round(colMeans(Parameter),3)

  }
  Summary_Backtest_Inv = sumbacktest_Inv
  View(Summary_Backtest_Inv)
  save(Summary_Backtest_Inv, file="~/Summary_Backtest_Inv.rda")
  save(Summary_Backtest_Inv, file="Summary_Backtest_Inv.rda")
  Portfolio = rownames(as.data.frame(sumbacktest_Inv))
  SUMBACKTEST_Inv_Df = mutate(as.data.frame(Portfolio),
                          as.data.frame(sumbacktest_Inv))
  write_xlsx(SUMBACKTEST_Inv_Df, "~/Summary_Backtest_Inv.xlsx")

  x="All backtest realized!"
  print(x)

}
