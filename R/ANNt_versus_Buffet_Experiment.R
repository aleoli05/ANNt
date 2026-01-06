#' ANNt_versus_Buffet_Experiment
#' Realize the 1st time series research, Validation: Long Term Testing, defined in "Value investing or quantitative financing: portfolio decision based on a new efficient frontier concept" paper.
#'
#' @param Data_Base import from Yahoo Finance or if "GitHub" to be specified, from GitHub/aleoli05
#' @param Serie "First_Serie" or 'Rebalanced_Serie" for the first or rebalanced sample
#' @param Initial_Date Series start Date (Must be 7 periods greater than the analyzed series)
#' @param Final_Date_Training Series finish training date
#' @param Final_Date Series end Date (If '' is the System Date)
#' @param Hidden Number of hidden neurons (If '' is the length series)
#' @param Stepmax Number of replications per asset to train the ANN
#' @param Loss Function: "MSE" for Mean Square Error, "MAE" for Mean Absolute Error,
#' "MADL" for Mean Absolute Directional Loss, and "GMADL" for Generalized Mean Absolute Directional Loss
#' @param Early_Stopping = 'No' or 'Yes'. Default is 'No'. If 'Yes' is necessary inform the value
#' @param Learning_Rate is the Artificial Neural Network learning rate
#' @param Decay L2 regularization or weight decay, add a penalty term to the loss function. "Yes" or "No.
#' No" is default. If  "Yes" is necessary inform the lambda or rate of regularization
#' @param Asymmetry "Negative" or "Positive". Shifts the probability of the return being greater than the proxy to the right or left, "Negative" or "Positive". Default is to the right, "Negative"
#' @param Skew_t Incorporate skew parameter in the probability: "Yes" or "No". Default is "Yes".
#' @param Parameters was defined in paper
#'
#' @author Alexandre Silva de Oliveira

#' @examples
#' ANNt_versus_Buffet_Experiment(Data_Base="GitHub",
#' Serie="First_Serie",
#' Initial_Date='2018-01-11',
#' Final_Date_Training='2021-12-30',
#' Final_Date='2022-08-04 ',
#' Hidden= 7,
#' Stepmax= 300,
#' Loss="GMADL",
#' Learning_Rate=0.3,
#' Decay=c('Yes',0.1),
#' Early_Stopping = c('Yes', 0.002),
#' Asymmetry='Negative',
#' Skew_t='Yes')
#'
#' @export
ANNt_versus_Buffet_Experiment <- function(Data_Base='GitHub', Serie="First_Serie",Initial_Date='2018-01-03',
                               Final_Date_Training='2021-12-30',
                               Final_Date='2022-08-04 ',
                               Hidden= '', Stepmax= 20000,
                               Loss="MSE", Learning_Rate=0.3, Decay='No',
                               Early_Stopping = 'No', Asymmetry='Negative',
                               Skew_t='Yes') {

  options(warn=-1)
  # library(webinar.cpom)
  library(quantmod)
  library(PerformanceAnalytics)
  library(magrittr)
  library(fBasics)
  library(tidyverse)
  library(stringr)
  library(dplyr)
  library(neuralnet)
  library(zoo)
  library(forecast)
  library(timetk)
  library(moments)
  library(data.table)
  library(ggplot2)
  library(rvest)
  library(caret)
  library (readxl)
  library(writexl)
  library(portfolio.optimization)
  library(PortfolioAnalytics)
  library(ROI)
  library(fPortfolio)
  library(timeSeries)
  library(gridExtra)
  library(cowplot)
  library(portfolioBacktest)
  library(CVXR)
  library(MFDFA)
  library(DEoptim)
# 1) Import the assets series, example:
  if(Data_Base=='GitHub'){
  library(readxl)
  library(readr)

  download.file("https://github.com/aleoli05/ANNt/raw/main/Data_/Assets_Prices_Buffet.rda",destfile ="~/Assets_Prices_Buffet.rda")
  #Assets_series (Tickers='Current_SP500_Tickers','^GSPC', '2018-01-03', '','daily')
  load("~/Assets_Prices_Buffet.rda")

  portfolioPrices_Teste = portfolio_observed
  portfolioPrices=portfolio_observed[1:1310,]

  View(portfolioPrices)

  Datas_portfolio = rownames(as.data.frame(portfolioPrices))


  # BENCHMARK
  BENCHMARK <- c("SP500")

  #Renames Columns
  Initial_Date=Initial_Date
  Final_Date_Training=Final_Date_Training
  Final_Date=Final_Date
  Hidden= Hidden
  Stepmax= Stepmax
  Loss=Loss
  Learning_Rate=Learning_Rate
  Decay=Decay
  Early_Stopping = Early_Stopping
  Asymmetry=Asymmetry
  Skew_t=Skew_t
  RM='Buffet'
  save(RM, file='~/RM.rda')
  save(Initial_Date, file='~/Initial_Date.rda')
  save(Final_Date_Training, file='~/Final_Date_Training.rda')
  save(Final_Date, file='~/Final_Date.rda')

  # Calculate Returns: Daily RoC
  portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))

  scenario.set <- portfolioReturns
  scenario.set <- scenario.set[apply(scenario.set,1,
                                     function(x) all(!0)),]
  #View(scenario.set)
  save(scenario.set,file='~/scenario.set.rda')
  tickers=colnames(scenario.set)
  save(tickers, file='~/tickers.rda')
  Tickers=colnames(scenario.set[-1])
  save(Tickers, file='~/Tickers.rda')
  assets <- ncol(scenario.set)
  scenarios <- nrow(scenario.set)
  #scenario.set=as.zoo(scenario.set)
  #rownames(scenario.set)=rownames(portfolioReturns)

  # Plot Charts
  cat("\n", paste0(names(scenario.set), "\n"))

  #chart.Bar(scenario.set$SP500)
  #charts.PerformanceSummary(scenario.set$SP500)
  chart.Bar(scenario.set[,1])
  charts.PerformanceSummary(scenario.set[,1])

  #########################################
  } else {
Assets_series (Tickers='Current_SP500_Tickers','^GSPC', Initial_Date = Initial_Date,
                  Final_Date=Final_Date,'daily')
}
# 2) Specify Buffet´s Portfolio, example:
Name = c("Buffet")


if (Serie=='First_Serie'){
  Portfolio=c("AAPL", "BAC", "KO", "AXP", "CVX", "KHC", "OXY") # PORTFOLIO´s Buffet 2022
  Weights=c( 0.414, 0.102, 0.073, 0.068, 0.068, 0.037, 0.033) # PORTFOLIO weights 2022
} else {
  #Portfolio=c("AAPL", "BAC", "CVX", "KO", "AXP", "KHC", "OXY", "MCO") # PORTFOLIO´s Buffet 2023
  #Weights=c( 0.441, 0.089, 0.079, 0.075, 0.074, 0.038, 0.038, 0.023) # PORTFOLIO weights 2023
  Portfolio=c("AAPL", "BAC", "CVX", "KO", "AXP", "KHC", "OXY") # PORTFOLIO´s Buffet 2023
  Weights=c( 0.441, 0.089, 0.079, 0.075, 0.074, 0.038, 0.038) # PORTFOLIO weights 2023
}

Specify_Pf_RM(Name,Portfolio,Weights)

# 3) ANNt order generate, example:
Type_ANN = 'ANNt'
save(Type_ANN, file='~/Type_ANN.rda')

ANNt_order (Initial_Date_Training = Initial_Date, Final_Date_Training = Final_Date_Training,
            Final_Date_Testing = Final_Date, Hidden= Hidden, Stepmax= Stepmax,
            Loss=Loss, Learning_Rate=Learning_Rate, Decay=Decay,
            Early_Stopping = Early_Stopping, Asymmetry='Negative', Skew_t=Skew_t)


# 4) Generate portfolios, example:
Initial_Date_Testing=as.character(as.Date(Final_Date_Training)+1)
Gen_portfolios(7,Initial_Date_Testing = Initial_Date_Testing,
               Final_Date_Testing = Final_Date, Rf=0.0426, 'T8')

# 5) Portfolios Backtesting, example:
Portfolio_backtesting(Date_Initial_Backtesting = Initial_Date_Testing,
                      Date_Final_Backtesting = Final_Date)

# 6) Plot Cumulative Portfolio Returns, example:
Plot_Cumulative_Returns('')

# 7) Generate Efficient Frontier of Markowitz:
Initial_Analysis_Date=as.character(as.Date(Initial_Date_Testing)-500)
Gen_efficient_frontier(Initial_Analysis_Date = Initial_Analysis_Date,Final_Analysis_Date = Final_Date)
#Gen_efficient_frontier('2020-01-21',Final_Analysis_Date = Final_Date)

# 8) Plot the Efficient Frontier graphic:
Plot_efficient_frontier()

# 9) Plot the New Efficient Frontier:
Plot_New_efficient_frontier()

# 10) Plot the Cumulative Sum of Returns (CUSUM Graphic):
Plot_CUSUM(1,5)

# 11) Save copy of alls processed data, example:
Backup_ANNt()

}
