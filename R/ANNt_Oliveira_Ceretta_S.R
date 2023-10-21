#'ANNt_Oliveira_Ceretta_Superior
#'Create a portfolio to outperform the Sharpe portfolio of reported assets.
#'@description
#'Command that realize all operations of the package and save all in a specific past into user past
#'@param Tickers Name of the assets or "Current_SP500_Tickers" for all S&P 500 assets
#'@param RM Proxy of the market
#'@param Rf Risk free rate
#'@param Initial_Date Series start Date, format ('Year-Month-Day'). Assets with values not observed in the series are excluded
#'@param Initial_Date_Training Training series start Date
#'@param Final_Date End date of the treatment series
#'@param Periodicity should be one of “daily”, “weekly”, “monthly”, “hourly”, “1minutes”, “2minutes”, “5minutes”, “15minutes”, “30minutes”, “60minutes”, “90minutes”. (Intraday maximum 7 days)
#'@param Hidden Number of hidden neurons (If ” is the length series). For a good performance use '' to form a square input x hidden matrix of neurons
#'@param Stepmax Number of replications per asset to train the ANN. For a good performance, use 7500
#'@param Type_ANNt Select type ANNt: "T1"= NNet_Signal_Traning; "T2"= NNet_t_Training; "T3"= MC_Signal_Training; "T4"= MC_t_Training; "T5"= NNet_Signal_Test; "T6"= NNet_t_Test; "T7"= MC_Signal_Test; "T8"= Type_ANNt: MC_t_Test
#'@param N_Assets Limit of asset numbers in the portfolio
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
#'ANNt_Oliveira_Ceretta_S(c('AAPL','XOM','TSLA','KO', 'F'), '^GSPC', 0, '2018-01-03', '2022-12-29', '', 'daily',5,7500,'T8',3)
#'@export
ANNt_Oliveira_Ceretta_S <- function(Tickers, RM, Rf, Initial_Date, Final_Date_Training, Final_Date, Periodicity, Hidden, Stepmax, Type_ANNt, N_Assets){
#Tickers <-c('AAPL','XOM','TSLA','KO', 'F')
#RM <-c('^GSPC') #RM the S&P500

  library(quantmod)
  library(PortfolioAnalytics)
  library(PerformanceAnalytics)
  #library(nse2r)
  library(MFDFA)
  library(xts)
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
  library(IntroCompFinR)
  Signal_Sharpe=1
  save(Signal_Sharpe,file="~/Signal_Sharpe.rda")
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
################################################################################
load('~/scenario.set.rda')
load('~/Datas1Predict.rda')
load('~/RM.rda')
load("~/Initial_Date.rda") # Carrega objeto scenario.set


# Duração do processamento 1720/length(dados)=1.2 min)
#load("~/scenario.set.rda") # Carrega objeto scenario.set
load("~/T8.rda") # Carrega objeto scenario.set
load("~/I_dataPredict.rda") # Carrega objeto scenario.set
load("~/F_dataPredict.rda") # Carrega objeto scenario.set
scenario.set=data.frame(scenario.set)
# h is the number of assets, case the ANNt_Oliveira_Ceretta went used
if(N_Assets=='n_Assets'){
  load('~/x4.rda')
  N_Assets=x4
}
dados<-scenario.set
nAtivos = ncol(dados)
datas=rownames(scenario.set)

Fator_Tempo = 1808/(nrow(dados))
Unidade=' minute(s)'
Tempo= round(Fator_Tempo*(ncol((dados))-1),2)
if (Tempo>120){
  Unidade=' hour(s)'
  Tempo=round(Tempo/60,2)
}
dados2=data.frame(dados)
cat(paste("
 Estimating Sharpe Portfolio, total processing time: ", Tempo, Unidade,"
____________________________________________________________________
", sep=""))

n_assets=N_Assets

if(Initial_Date_Testing==('')){
  D = which(rownames(scenario.set)==Final_Date_Training)
  Initial_Date_Testing=Initial_Date_Testing = rownames(as.data.frame(scenario.set)[D+1,])
}
if(Final_Date_Testing==('')){
  Final_Date_Testing=rownames(dados2[nrow(dados2),])
  #Final_Date_Testing=Sys.Date()
}

Rf=Rf/100

if(class(Initial_Date_Testing)!=('numeric')){
  Datas1Predict = rownames(scenario.set)[
  (which(rownames(scenario.set)==Initial_Date_Testing)):(which(rownames(scenario.set)==Final_Date_Testing))]

  }else{
  Datas1Predict = rownames(scenario.set)[(Initial_Date_Testing+6):(which(rownames(scenario.set)==Final_Date_Testing))]
}
save(Datas1Predict,file='~/Datas1Predict.rda')
PosCovidSP500 = as.matrix(scenario.set[Datas1Predict,1])
colnames(PosCovidSP500)=colnames(scenario.set[1])
rownames(PosCovidSP500)=Datas1Predict
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))

options(warn=-1)



all.returns <- TodosAtivosPredict
while (nrow(all.returns)<ncol(all.returns)){
  Inicio=as.character(as.Date(rownames(all.returns)[1])-(ncol(all.returns)-nrow(all.returns)))
  Fim=as.Date(rownames(all.returns)[nrow(all.returns)])

  while(length(which(rownames(scenario.set)==Inicio))==0){
    dia=as.Date(Inicio)
    new_day=dia+1
    Inicio = as.character(new_day)}

    while(length(which(rownames(scenario.set)==Fim))==0){
      dia=as.Date(Fim)
      new_day=dia+1
      Fim = as.character(new_day)}


  all.returns=scenario.set[(which(rownames(scenario.set)==as.character(Inicio))-10):which(rownames(scenario.set)==Fim),-1]
}

Contador=round(nrow(all.returns),-1)
#if(nrow(all.returns)-Contador<0){
Contador=Contador-10
#}
Remover= nrow(all.returns)-Contador
if(ncol(all.returns)>10){
  all.returns <- all.returns[1:(nrow(all.returns)-Remover),]

  if (nrow(all.returns)-ncol(all.returns)<10){
    Inicio=as.Date(rownames(all.returns)[1])
    Fim=as.Date(rownames(all.returns)[nrow(all.returns)])
    all.returns=scenario.set[(which(rownames(scenario.set)==Inicio)-20):which(rownames(scenario.set)==Fim),-1]
  }
}

####### set up portfolio with objetive and constraints
n.assets <- length(colnames(all.returns))
port.sec <- portfolio.spec(assets = colnames(all.returns))
port.sec <- add.objective(portfolio = port.sec, type = "risk", name = "StdDev")
port.sec <- add.objective(portfolio = port.sec, type = "return", name = "mean")
port.sec <- add.constraint(portfolio = port.sec, type = "full_investiment")
port.sec <- add.constraint(portfolio = port.sec, type = "box", min = 0, max = 1)


# map off efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R = all.returns, portfolio = port.sec,
                                         n.portfolio = 2000, type = "mean-StdDev")

# Daily Sharpe ratio
rf=(1+Rf)^(1/252)-1
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios==max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 1

#print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
#print(sprintf("Optimal E(port return): %f", optimal.mean*sqrt(n.trading.days.per.year)))
mean_sharpe = optimal.mean*sqrt(n.trading.days.per.year)
#print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
sd_sharpe <- optimal.sd*sqrt(n.trading.days.per.year)

#print("Optimal weights")
weight_test <- eff.frontier$frontier[optimal.port.name,(1:n.assets)+3]

save(mean_sharpe,file="~/mean_sharpe.rda")
save(sd_sharpe,file="~/sd_sharpe.rda")
save(weight_test,file="~/weight_test.rda")

#########################################


weight_test <- round(weight_test,4)
weight_Sharpe= weight_test[which(weight_test !=0)]
weight_Sharpe

# Weight extract
Weight_Sharpe_1 <- t(as.data.frame(weight_Sharpe))
colnames(Weight_Sharpe_1)<-str_replace(colnames(Weight_Sharpe_1),'w.','')
rownames(Weight_Sharpe_1)<-'Weight'

print(paste('Weights of the SHARPE Portfolio:'))
print(Weight_Sharpe_1)
#weight
### Retornos carteira Sharpe todos os ativos
#RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% maxSR.weight.rp
RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% weight_test

##############################################################################

#View(PosCovidBuffet2)
Specific_RM = as.matrix(scenario.set[,-1])%*% weight_test
Portfolio_with_RM_Original=scenario.set
RM_Original=RM
RM='Sharpe'
scenario.set[,1]=as.data.frame(Specific_RM)
colnames(scenario.set)[1]=RM
#rownames(scenario.set)=datas

save(RM_Original,file='~/RM_Original.rda')
save(Specific_RM,file='~/Specific_RM.rda')
save(scenario.set,file='~/scenario.set.rda')
#################################################################################



Final_Date_Training <- Final_Date_Training
ANNt_order ('', '', '', 'hidden', 'stepmax')

Signal_Sharpe=0
save(Signal_Sharpe,file="~/Signal_Sharpe.rda")
RM=RM_Original
save(RM, file="~/RM.rda")
load('~/scenario.set.rda')
scenario.set=Portfolio_with_RM_Original
save(scenario.set, file="~/scenario.set.rda")

Gen_portfolios('n_Assets',Initial_Date_Testing,'',0, Type_ANNt)
Portfolio_backtesting('','')
Plot_Cumulative_Returns('')
Gen_efficient_frontier('','')
Plot_efficient_frontier()
Sys.sleep((15))
Plot_New_efficient_frontier()
Sys.sleep((15))
Plot_CUSUM('','')
save(Final_Date, file='~/Final_Date.rda')
Signal_Sharpe=1
save(Signal_Sharpe,file="~/Signal_Sharpe.rda")
Backup_ANNt()
Signal_Sharpe=0
save(Signal_Sharpe,file="~/Signal_Sharpe.rda")
}
