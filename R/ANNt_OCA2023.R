#'ANNt OCA 2023
#'reproduce Oliveira, Ceretta, & Albrecht (2023)
#'@description For more details see:
#'Oliveira, A. S., Ceretta, P. S., & Albrecht, P. (2023). Performance comparison of multifractal techniques and artificial neural networks in the construction of investment portfolios. Finance Research Letters, 55, 103814.
#'@param Tickers Current SP500 Tickers on 2022-07-12
#'@param RM Proxy of the market: SP500
#'@param Initial_Date Series start Date: 2018-01-10
#'@param Final_Date_Training Training series start Date: 2020-01-20
#'@param Final_Date End date of the treatment series: 2022-07-12
#'@param Periodicity In the study should be one of “daily”
#'@param Hidden Number of hidden neurons. In the study use '' to form a square input x hidden matrix of neurons
#'@param Stepmax Number of replications per asset to train the ANN. In the study use 10000
#'@param N_Assets Limit of asset numbers in the portfolio: Use 5, 10, 15 or 20 to replicate the study
#'@author Alexandre Silva de Oliveira
#'@references Oliveira, A. S., Ceretta, P. S., & Albrecht, P. (2023). Performance comparison of multifractal techniques and artificial neural networks in the construction of investment portfolios. Finance Research Letters, 55, 103814.
#'@examples
#'Tickers <-'Current_SP500_Tickers'
#'RM <-c('^GSPC') #RM the S&P500
#'Rf <- 0.0311
#'Initial_Date <-c('2018-01-11')
#'Final_Date_Training <- c('2020-01-17')
#'Final_Date <-c('2022-07-12')
#'Periodicity <- c('daily')
#'Hidden <- ''
#'Stepmax <- 10000
#'N_Assets <- 5
#'ANNt:::ANNt_OCA2023('Current_SP500_Tickers', '^GSPC', 0.0311, '2018-01-11', '2020-01-17', '', 'daily','',10000,5)
#'@export
ANNt_OCA2023 <- function(Tickers, RM, Rf, Initial_Date, Final_Date_Training, Final_Date, Periodicity, Hidden, Stepmax, N_Assets){
  Tickers <- 'Current_SP500_Tickers'

  #RM <-c('^GSPC') #RM the S&P500
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
  Initial_Analysis_Date <- c('')
  Final_Analysis_Date <- c('')

  Assets_series (Tickers,RM, Initial_Date, Final_Date,'daily')
  Final_Date_Training <- Final_Date_Training
  ANNt_order ('', '', '2022-07-12', 'hidden', 'stepmax')
  Gen_portfolios('n_Assets',Initial_Date_Testing,'2022-07-12',0,'T8')
  Portfolio_backtesting()
  Plot_Cumulative_Returns('')
  #Gen_efficient_frontier('','')
  #Plot_efficient_frontier()
  #Sys.sleep((15))
  #Plot_New_efficient_frontier()
  #Sys.sleep((15))
  #Plot_CUSUM('','')
  save(Final_Date, file='~/Final_Date.rda')
  Backup_ANNt()
}
