#'ANNt_Oliveira_Ceretta
#'Command that realize all operations of the package (except Out-of-sample) and save all in a specific past into user past
#'@param Tickers Name of the assets or "Current_SP500_Tickers" for all S&P 500 assets
#'@param RM Proxy of the market
#'@param Rf Risk free rate
#'@param Initial_Date Series start Date, format ('Year-Month-Day'). Assets with values not observed in the series are excluded
#'@param Initial_Date_Training Training series start Date
#'@param Final_Date End date of the treatment series
#'@param Periodicity should be one of “daily”, “weekly”, “monthly”
#'@param Hidden Number of hidden neurons (If ” is the length series). For a good performance use '' to form a square input x hidden matrix of neurons
#'@param Stepmax Number of replications per asset to train the ANN. For a good performance, use 7500
#' @param Asymmetry "Negative" or "Positive". Shifts the probability of the return being greater than the proxy to the right or left, "Negative" or "Positive". Default is to the right, "Negative"
#'@param Type_ANNt Select type ANNt: "T1"= NNet_Signal_Traning; "T2"= NNet_t_Training; "T3"= MC_Signal_Training; "T4"= MC_t_Training; "T5"= NNet_Signal_Test; "T6"= NNet_t_Test; "T7"= MC_Signal_Test; "T8"= Type_ANNt: MC_t_Test
#'@param N_Assets Limit of asset numbers in the portfolio
#'@param Base Database to use: "yahoo" or "Rus"
#'@param Import Import dates from external data base after first import. "Yes"
#'or "No". "Yes" is the standard.
#'@param Exclude_ticket Deletes any ticket from the ticket list that you want to remove for some reason
#'@param Type_ANN Select the network type: 'ANNt' or 'LSTMt' in RNN from ANNt
#' @param Order If "Yes" processes the asset selection, if "No" uses the already processed assets available in the database
#'
#'@examples
#'Tickers <-c('AAPL','XOM','TSLA','KO', 'F')
#'RM <-c('^GSPC') #RM the S&P500
#'Rf <- 0
#'Initial_Date <-c('2018-01-03')
#'Final_Date_Training <- c('2022-12-29')
#'Final_Date <-c('')
#'Periodicity <- c('daily')
#'Hidden <- 5
#'Stepmax <- 7500
#'Type_ANNt <- 'T8'
#'N_Assets <- 3
#'ANNt_Oliveira_Ceretta(c('AAPL','XOM','TSLA','KO', 'F'), '^GSPC', 0, '2018-01-03', '2022-12-29', '', 'daily',5,7500,'T8',3)
#'@export
ANNt_Oliveira_Ceretta <- function(Tickers, RM, Rf, Initial_Date, Final_Date_Training,
                                  Final_Date, Periodicity, Hidden, Stepmax, Asymmetry='Negative', Type_ANNt='T4',
                                  N_Assets, Base='yahoo', Import='Yes', Exclude_ticket='', Type_ANN='ANNt',
                                  Order='Yes'){
#Tickers <-c('AAPL','XOM','TSLA','KO', 'F')
#RM <-c('^GSPC') #RM the S&P500


  Exclude=Exclude_ticket
  Periodo= c('daily','monthly','weekly')
  if(length(which(Periodo==Periodicity))==0){
    stop('This periodicity is not implementedy in this command. Use step by step process starting with the "Assets_series" command!')
  }


Signal_Sharpe=0
save(Signal_Sharpe, file='~/Signal_Sharpe.rda')
Initial_Date <-Initial_Date
x0 = Final_Date
save(x0, file='~/x0.rda')
Final_Date <-Final_Date
Periodicity <- Periodicity
Initial_Date_Training <-''
# Indicate that the command ANNt_Oliveira_Ceretta is used
x1 = Final_Date_Training
save(x1, file='~/x1.rda')
Final_Date_Testing <-c('')
x2 = Hidden
save(x2, file='~/x2.rda')
x3 = Stepmax
save(x3, file='~/x3.rda')
x4 = N_Assets
save(x4, file='~/x4.rda')
Initial_Date_Testing <- c('')
Final_Date_Testing <- c('')
Rf <- Rf
save(Rf, file='~/Rf.rda')
x5 = Rf
save(x5, file='~/x5.rda')
Initial_Analysis_Date <- c('')
Final_Analysis_Date <- c('')

#load('~/Horizon.rda')
if(Order=='Yes'){
if (Import =='Yes'){
  if (Base=='yahoo'){
    Assets_series (Tickers,RM, Initial_Date, Final_Date,'daily', Exclude_ticket=Exclude)
  }
  if(Base=='Rus'){
    Assets_series_Rus (Tickers,RM, Initial_Date, Final_Date,'daily', Exclude_ticket=Exclude)
  }
  if(Base=='Rus_2'){
    Assets_series_Rus_2 (Tickers,RM, Initial_Date, Final_Date,'daily', Exclude_ticket=Exclude)
  }
}}

Final_Date_Training <- Final_Date_Training
X11 = Asymmetry
save(X11,file='~/X11.rda')

if(Order=='Yes'){
if(Type_ANN=='ANNt'){
  ANNt_order ('', '', '', 'hidden', 'stepmax', Asymmetry=Asymmetry)
} else {
  if(Type_ANN=='LSTMt'){
    LSTMt_order ('', '', '', 'hidden', 'stepmax', Asymmetry=Asymmetry,Plot='No')
  }}}
Gen_portfolios('n_Assets',Initial_Date_Testing,'',Rf, Type_ANNt)
Portfolio_backtesting('','')
Plot_Cumulative_Returns('')
Gen_efficient_frontier('','')
Plot_efficient_frontier()
Sys.sleep((15))
Plot_New_efficient_frontier()
Sys.sleep((15))
Plot_CUSUM('','')
save(Final_Date, file='~/Final_Date.rda')
Backup_Inv()
}
