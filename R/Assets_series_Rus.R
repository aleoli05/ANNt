#' Assets series Rus
#' Import the series with the length specified of the Moscow Stock Exchange

#' @param Tickers Name of the assets
#' @param RM Proxy of the Russia market - Example: IMOEX - Moscow Exchange Index
#' @param Initial_Date Series start Date, format ('Year-Month-Day')
#'
#' Assets with values not observed in the series are excluded
#' @param Final_Date Series end Date ('Year-Month-Day'). If '' is the System Date
#' @param Periodicity should be one of “daily”, “weekly”, “monthly”, “hourly”, “1minutes”, “2minutes”, “5minutes”, “15minutes”, “30minutes”, “60minutes”, “90minutes”. (Intraday maximum 7 days)
#' @param Exclude_ticket Deletes any ticket from the ticket list that you want to remove for some reason


#' @examples
#' # Specify the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' Tickers <-c('AFKS', 'AFLT', 'FEES', 'GMKN','GAZP')
#' RM <-c('^GSPC') #RM the S&P500
#' Initial_Date <-c('2018-01-03')
#' Final_Date <-c('2023-09-07')
#' Periodicity <- c('daily')
#'
#' # Generates the Adjusted Daily Prices Series from Yahoo Finance
#' Assets_series_Rus (Tickers=c('AFKS', 'AFLT', 'FEES', 'GMKN','GAZP'),'IMOEX', '2018-01-03', '','daily')
#'
#' @export
Assets_series_Rus <- function(Tickers, RM, Initial_Date, Final_Date, Periodicity, Exclude_ticket='') {

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
  inst_pacote <- require(rusquant)
  if (inst_pacote == FALSE) {
    install.packages("rusquant")
  }
  library(rusquant)

  ydev=dev.list()
  if(class(ydev)!="NULL"){
    dev.off()
  }else{print('Starting Asset_series Command')}

  ################# Create Returns Times Series ###########################

 if(Periodicity=='daily'){
   #coluna=4
   Periodicity= 'day'
   #stop('This periodicity is not implementedy in this verson. Please, use daily periodicity!')
 }


  if(Final_Date==('')){
    Final_Date=Sys.Date()
  }


  # RM

  RM <- RM
  Tickers_1=Tickers
  Condicao=Tickers
  ################################################################################

  ###############################

  #######

  ###############Exclude_ticket
  Exclude=NULL
  for (i in 1:length(Exclude_ticket)) {
    Exclude[i]<-which(Tickers_n==Exclude_ticket[i])
    #Exclude<-Tickers[!Exclude_ticket]
  }
  if(length(Exclude)!=0){
    Tickers_n=Tickers_n[-Exclude]
  }
  ###############

  Tickers_MOEX = Tickers
  Date_1="Date"
  portfolio<- getSymbols.Alor(RM, env = globalenv(), from = Initial_Date, to = Final_Date, adjust = FALSE, api.key = NULL, period = Periodicity, verbose = TRUE, board = "MOEX", auto.assign = FALSE)

  portfolio<- as.data.frame(portfolio)
  Date_1<-portfolio$Date
  rownames(portfolio) <- Date_1
  portfolio <- data.frame(portfolio[,2], portfolio[,7])
  rownames(portfolio) <- as.character(Date_1)
  colnames(portfolio) <- c("IMOEX", "Date")

  for (Ticker in Tickers_MOEX){
    test <- getSymbols.Alor(Ticker, env = globalenv(), from = Initial_Date, to = Final_Date, adjust = FALSE, api.key = NULL, period = Periodicity, verbose = TRUE, board = "MOEX", auto.assign = FALSE)
    #test <- cbind(portfolioPrices,
    #               getSymbols.Alor(Ticker, env = globalenv(),
    #                              from = "2021-12-31",
    #                             to = Sys.Date(),
    #                            adjust = FALSE,
    #                           api.key = NULL,
    #                          period = "day",
    #                         verbose = TRUE,
    #                        board = "MOEX",
    #                       auto.assign = FALSE))

    test <- as.data.frame(test)
    Date<-test$Date
    rownames(test) <- Date
    Teste <- data.frame(test[,2], test[,7])
    rownames(Teste) <- as.character(Date)
    colnames(Teste) <- c(Ticker, "Date")

    if(Ticker==Tickers_MOEX[1]){
      Test = left_join(portfolio,Teste)
    } else {
      Test = left_join(Test,Teste)
    }
  }
  rownames(Test) <- as.character(Date_1)
  portfolioPrices<- Test[,-2]
  View(portfolioPrices)

  #################

  Condicao = Ticker
  Tickers=Ticker




  Datas_portfolio = rownames(as.data.frame(portfolioPrices))
  portfolioPrices_Df = mutate(as.data.frame(Datas_portfolio),
                              as.data.frame(portfolioPrices))

  #Renames Columns
  tickers <- colnames(portfolioPrices)
  portfolioPrices_Teste = portfolioPrices


  Datas_portfolio = rownames(as.data.frame(portfolioPrices))
  portfolioPrices_Df = mutate(as.data.frame(Datas_portfolio),
                              as.data.frame(portfolioPrices))
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                           function(x) all(!is.na(x))),]
  portfolioPrices<- portfolioPrices[apply(portfolioPrices,1,
                                          function(x) all(!0)),]

  portfolio_observed=portfolioPrices
  #portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
  portfolioReturns <- as.matrix(na.omit(ROC(portfolioPrices), type="discrete"))


  scenario.set <- portfolioReturns

  scenario.set <- scenario.set[apply(scenario.set,1,
                                     function(x) all(!0)),]
  #View(scenario.set)
  Final_Date = rownames(as.data.frame(scenario.set)[nrow(scenario.set),])
  assets <- ncol(scenario.set)
  scenarios <- nrow(scenario.set)
  saveRDS(scenario.set,file='scenario.set')
  save(Initial_Date, file="~/Initial_Date.rda")
  save(Final_Date, file="~/Final_Date.rda")
  save(RM, file="~/RM.rda")
  save(Tickers, file="~/Tickers.rda")
  save(tickers,file='~/tickers.rda')
  save(scenario.set,file='~/scenario.set.rda')
  save(scenario.set,file='~/Assets_Returns.rda')
  save(portfolioPrices,file='~/Assets_Prices.rda')
  save(portfolio_observed,file='~/Assets_Prices_observed.rda')

  Assets_Prices=portfolioPrices

  Assets_Returns=scenario.set
  Asset_Prices_Observed=portfolio_observed
  Assets_Returns=scenario.set


  write.zoo(scenario.set, file='scenario.set')
  View(Assets_Prices)

  View(Assets_Returns)
  #write_xlsx(tickers,file='~/tickers.xlsx')
  write_xlsx(portfolioPrices_Df, "~/Assets_Prices.xlsx")
  portfolio_observed2=data.frame(portfolio_observed)
  write_xlsx(portfolio_observed2, "~/Assets_Prices_Observed.xlsx")
  scenario.set2=data.frame(rownames(as.data.frame(scenario.set)),as.data.frame(scenario.set))
  write_xlsx(scenario.set2, "~/Assets_Returns.xlsx")
  write_xlsx(as.data.frame(Tickers_MOEX), "~/Current_Tickers_MOEX.xlsx")

  # Plot Charts
  cat("\n", paste0(names(scenario.set), "\n"))

  chart.Bar(scenario.set[,1])
  #charts.PerformanceSummary(scenario.set[,1])

  #ng(file="~/Chart_RM.png", width=1920, height=1200, res=296)
  png(file="~/Chart_RM.png")
  op <- par(new = TRUE)
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A")
  cex=0.5
  Chart_RM=(charts.PerformanceSummary(scenario.set[,1],main=paste("Performance of", RM)))
  dev.off()
  op <- par(new = TRUE)
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A")
  cex=0.5
  charts.PerformanceSummary(scenario.set[,1],main=paste("Performance of", RM))
  #########################################
}
