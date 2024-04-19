#' ANNt_versus_Buffet_Article
#' Realize the 2st time series research, Validation: Long Term Testing, defined in "Value investing or quantitative financing: portfolio decision based on a new efficient frontier concept" paper.
#'
#' @param Parameters was defined in paper
#' @examples
#' ANNt_versus_Buffet_Article()
#'
#' @export
ANNt_versus_Buffet_Article <- function() {



################################################################################
### ALEXANDRE SILVA DE OLIVEIRA - MY OPTMIZATION MODELING WITH R
################################################################################
###
### PRINCIPAL REFERENCE:
### ----------------------------------------------------------------------------
### Ronald Hochreiter - http://www.finance-r.com/cpom/
### Morris e Comeau (2020) - Financial Markets and Portfolio Management
################################################################################
# 8296 Grafico das carteiras
# 6665 Tamanho das carteiras
# 7455 Tamanho MF-DFA
# 7476 Tamanho RNAt
################################################################################
##### 1. Setup & Data


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


################# Create Returns Times Series ###########################
# BENCHMARK
BENCHMARK <- c("SP500")

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


# Calculate Returns: Daily RoC
portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))

scenario.set <- portfolioReturns
scenario.set <- scenario.set[apply(scenario.set,1,
                                   function(x) all(!0)),]
#View(scenario.set)
save(scenario.set,file='~/scenario.set.rda')

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

Name = c("Buffet")
Portfolio=c("AAPL", "BAC", "CVX", "KO", "AXP", "KHC", "OXY", "MCO") # PORTFOLIO´s Buffet 2023
Weights=c( 0.441, 0.089, 0.079, 0.075, 0.074, 0.038, 0.038, 0.023) # PORTFOLIO weights 2023
Specify_Pf_RM(Name,Portfolio,Weights)

scenario.set=as.matrix(scenario.set)
save(scenario.set,file='~/scenario.set.rda')

# 3) ANNt order generate, example:
## 1° Time Series
#ANNt_order ('2018-01-11', '2021-12-30','2022-08-04 ', '', 7000)
## 2° Time Series
#ANNt_order ('2018-01-21', '2022-08-11','2023-03-17 ', '', 7000)


load("~/T8.rda") # Carrega objeto scenario.set
load("~/RM.rda")
#load("~/portfolioReturns.rda")


################################################################################

###############################



















################################################################################
# Comparativo com a Carteira de Warren Buffet
# Carteira de Buffet - 7 ativos - 80%

load("~/scenario.set.rda")
scenario.set = data.frame(scenario.set)

########################### 1° Serie Times #####################################
### Short term Analysis
Datas1Predict = rownames(scenario.set)[
  (which(rownames(scenario.set)=="2021-12-31")):
    (which(rownames(scenario.set)=="2022-08-04"))]
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))

### Long Term Analysis
Datas1Predict = rownames(scenario.set)[
  (which(rownames(scenario.set)=="2020-01-21")):
    (which(rownames(scenario.set)=="2022-08-04"))]
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,]))

########################### 2° Serie Times Rebalanced###########################
### Short term Analysis
Datas1Predict = rownames(scenario.set)[
  (which(rownames(scenario.set)=="2022-08-12")):
    (which(rownames(scenario.set)=="2023-03-17"))]
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))

### Long Term Analysis
Datas1Predict = rownames(scenario.set)[
  (which(rownames(scenario.set)=="2020-09-01")):
    (which(rownames(scenario.set)=="2023-03-17"))]
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,]))

################################################################################
all.returns <- TodosAtivosPredict
library(dplyr)
PosCovidSP500 = as.matrix(portfolioReturns[Datas1Predict,1])
Buffet = c("AAPL", "BAC", "KO", "AXP", "CVX", "KHC", "OXY") # PORTFOLIO_2022
PesosBuffet = c( 0.414, 0.102, 0.073, 0.068, 0.068, 0.037, 0.033) # PORTFOLIO_2022
Buffet = c("AAPL", "BAC", "CVX", "KO", "AXP", "KHC", "OXY", "MCO") # PORTFOLIO_2023
PesosBuffet = c( 0.441, 0.089, 0.079, 0.075, 0.074, 0.038, 0.038, 0.023) # PORTFOLIO_2023
sum(PesosBuffet)
PesosBuffetNormalizado = PesosBuffet/sum(PesosBuffet)
CarteiraBuffet = as.data.frame(scenario.set) %>%
  dplyr::select(which((colnames(scenario.set) %in% Buffet)))
PosCovidBuffet = CarteiraBuffet[Datas1Predict,]
View(PosCovidBuffet)
#AAPL <- getSymbols.yahoo("AAPL", from="2010-01-01",
#                                         auto.assign=FALSE)[,4]
#AAPLretornos<- na.omit(ROC(AAPL, type="discrete"))
#colnames(AAPLretornos)="AAPL"
#PosCovidAAPLRetornos = AAPLretornos[Datas1Predict]
#PosCovidBuffet2 = cbind(PosCovidBuffet, PosCovidAAPLRetornos)
#View(PosCovidBuffet2)

#AAPL <- getSymbols.yahoo("AAPL", from="2018-01-09",
#       auto.assign=FALSE)[,4]
#AAPLretornos<- na.omit(ROC(AAPL, type="discrete"))
#colnames(AAPLretornos)="AAPL"
#PosCovidAAPLRetornos = AAPLretornos[Datas1Predict]

TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))
#TodosAtivosPredict1 = cbind(as.data.frame(PosCovidAAPLRetornos),TodosAtivosPredict)
#TodosAtivosPredict = TodosAtivosPredict1

#View(PosCovidBuffet2)
RetornosMediosBuffet = as.matrix(PosCovidBuffet) %*% PesosBuffetNormalizado

# Carteira RNA NNet dist t com pesos iguais para Comparação
#T76_Carteira = c("SNPS", "BIIB", "GNRC", "TPR", "DG", "VTRS", "COST")

#Carteira_T6 = as.data.frame(scenario.set) %>%
#  dplyr::select(which((colnames(scenario.set) %in% T6_Carteira)))
#T6 = Carteira_T6[Datas1Predict,]


CarteiraComparativa = colnames(T8[1:8])
C_Net_T_comparativa = as.data.frame(scenario.set) %>%
  dplyr::select(which((colnames(scenario.set) %in% CarteiraComparativa)))
C_Net_T_comparativa = C_Net_T_comparativa[Datas1Predict,]
PesosComparativos = c(rep(1/8,8))
Media_C_Net_T_Comparativa = as.matrix(C_Net_T_comparativa) %*% PesosComparativos

# Carteira de Markovitz de Minima Variância obtida a partir de todos ativos
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))
pesos_todosPredict <- round(tseries::portfolio.optim(TodosAtivosPredict)$pw, 2)
RetornoMedioMArkovitz = TodosAtivosPredict%*% pesos_todosPredict


###############
symbols = colnames(TodosAtivosPredict)


# Carteira RNA NNet dist T com pesos de Markovitz  para Comparação
pesos_MarkovitzNNet_T <- round(tseries::portfolio.optim(
  as.matrix(C_Net_T_comparativa))$pw, 2)
Ret_Medio_RNA_T_Mkv = as.matrix(C_Net_T_comparativa) %*% pesos_MarkovitzNNet_T


##### Carteira de Sharpe para todos os Ativos
## Optmization

symbols2 = colnames(TodosAtivosPredict)
init.portf <- portfolio.spec(assets = symbols)
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
init.portf

init.portf <- add.constraint(portfolio = init.portf, type = "risk",
                             name = "StdDev", multiplier = 0)
port1 <- add.constraint(portfolio = init.portf,
                        type = "diversification", min=0, max=1,
                        indexnum=2)
port1 <- add.constraint(portfolio = init.portf, type = "risk", name = "StdDev")

print("Test 1")

### Carteira Sharpe todos os ativos
maxSRport.rp <- optimize.portfolio(R=TodosAtivosPredict[1:630,],
                                   portfolio = port1,
                                   optimize_method = "random",
                                   search_size = 2000,
                                   maxSR=TRUE, trace = TRUE)
maxSRport.rp

maxSR.weight.rp <- extractWeights(maxSRport.rp)

### Retornos carteira Sharpe todos os ativos
RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% maxSR.weight.rp

print("Test 2")


### Carteira Sharpe RNAt
## Optmization
symbols_RNAt = colnames(C_Net_T_comparativa)
init.portf <- portfolio.spec(assets = symbols_RNAt)
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
init.portf

init.portf <- add.constraint(portfolio = init.portf, type = "risk",
                             name = "StdDev", multiplier = 0)
port1 <- add.constraint(portfolio = init.portf,
                        type = "diversification", min=0, max=1,
                        indexnum=2)
port1 <- add.constraint(portfolio = init.portf, type = "risk", name = "StdDev")



maxSRport.rp.RNAt<- optimize.portfolio(R=C_Net_T_comparativa,
                                       portfolio = port1,
                                       optimize_method = "random",
                                       search_size = 2000,
                                       maxSR=TRUE, trace = TRUE)
maxSRport.rp.RNAt

maxSR.weight.rp.RNAt <- extractWeights(maxSRport.rp.RNAt)

### Retornos carteira Sharpe RNAt
RetornoMedioMaxIS_RNAt = as.matrix(C_Net_T_comparativa)%*% maxSR.weight.rp.RNAt



all.returns <- TodosAtivosPredict
## set up portfolio with objetive and constraints
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
rf=(1+0.0571)^(1/252)-1
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios==max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 1

print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("Optimal E(port return): %f", optimal.mean*sqrt(n.trading.days.per.year)))
mean_sharpe = optimal.mean*sqrt(n.trading.days.per.year)
print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
sd_sharpe <- optimal.sd*sqrt(n.trading.days.per.year)
print("Optimal weights")
weight_test <- eff.frontier$frontier[optimal.port.name,(1:n.assets)+3]
weight_test <- round(weight_test,4)
weight_Sharpe= weight_test[which(weight_test !=0)]
weight_Sharpe

### Retornos carteira Sharpe todos os ativos
#RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% maxSR.weight.rp
RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% weight_test

################################################################################
### Retornos carteira Sharpe RNAt
all.returns_RNA_t <- as.matrix(C_Net_T_comparativa)
## set up portfolio with objetive and constraints
n.assets.RNAt <- length(colnames(all.returns_RNA_t))

port.sec.RNAt <- portfolio.spec(assets = colnames(all.returns_RNA_t))
port.sec.RNAt <- add.objective(portfolio = port.sec.RNAt, type = "risk", name = "StdDev")
port.sec.RNAt <- add.objective(portfolio = port.sec.RNAt, type = "return", name = "mean")
port.sec.RNAt <- add.constraint(portfolio = port.sec.RNAt, type = "full_investiment")
port.sec.RNAt <- add.constraint(portfolio = port.sec.RNAt, type = "box", min = 0, max = 1)

# map off efficient frontier (for variance risk)
eff.frontier_RNA_t <- create.EfficientFrontier(R = all.returns_RNA_t, portfolio = port.sec.RNAt,
                                               n.portfolio = 2000, type = "mean-StdDev")

# Daily Sharpe ratio
rf=(1+0.0571)^(1/252)-1
sharpe.ratios_RNA_t <- (eff.frontier_RNA_t$frontier[,"mean"]-rf)/eff.frontier_RNA_t$frontier[,"StdDev"]
max.sharpe.ratio_RNA_t <- sharpe.ratios_RNA_t[sharpe.ratios_RNA_t==max(sharpe.ratios_RNA_t)]
optimal.port.name.RNAt <- names(max.sharpe.ratio_RNA_t)
optimal.mean.RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,"mean"]
optimal.sd.RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,"StdDev"]

n.trading.days.per.year.RNAt <- 1

print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio_RNA_t*sqrt(n.trading.days.per.year.RNAt)))
print(sprintf("Optimal E(port return): %f", optimal.mean.RNAt*sqrt(n.trading.days.per.year.RNAt)))
mean_sharpe_RNA_t = optimal.mean.RNAt*sqrt(n.trading.days.per.year.RNAt)
print(sprintf("Optimal sd(port return): %f", optimal.sd.RNAt*sqrt(n.trading.days.per.year.RNAt)))
sd_sharpe_RNA_t <- optimal.sd.RNAt*sqrt(n.trading.days.per.year.RNAt)
print("Optimal weights")
weight_test_RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,(1:n.assets.RNAt)+3]
weight_test_RNAt <- round(weight_test_RNAt,4)
weight_Sharpe_RNA_t= weight_test_RNAt[which(weight_test_RNAt !=0)]
weight_Sharpe_RNA_t

RetornoMedioMaxIS_RNAt = as.matrix(C_Net_T_comparativa)%*% weight_test_RNAt


##

# Geração da Matriz de comparação dos Retornos
Comparativo_RETORNOS = matrix(nrow=length(RetornosMediosBuffet), ncol=7)
Comparativo_RETORNOS[,1] = RetornosMediosBuffet
Comparativo_RETORNOS[,2] = PosCovidSP500
Comparativo_RETORNOS[,3] = Media_C_Net_T_Comparativa
Comparativo_RETORNOS[,4] = RetornoMedioMArkovitz
Comparativo_RETORNOS[,5] = Ret_Medio_RNA_T_Mkv
Comparativo_RETORNOS[,6] = RetornoMedioMaxIS
Comparativo_RETORNOS[,7] = RetornoMedioMaxIS_RNAt
#Comparativo_RETORNOS[,6] = RetornoMedioMean_Variance_Mkv

colnames(Comparativo_RETORNOS)= c("Buffet","SP500", "RNAt_Eq", "Markowitz",
                                  "RNAt_Mkv", "MaxSharpe", "RNAt_Sharpe")
rownames(Comparativo_RETORNOS) = rownames(PosCovidBuffet)
Datas_Comparativo_RETORNOS = rownames(as.data.frame(Comparativo_RETORNOS))
Comparativos_RETORNOS_Df = mutate(as.data.frame(Datas_Comparativo_RETORNOS),
                                  as.data.frame(Comparativo_RETORNOS))
write_xlsx(Comparativos_RETORNOS_Df, "RetornosRNAxBUFFET_Sharpe_Curto_Prazo.xlsx")
write_xlsx(Comparativos_RETORNOS_Df, "RetornosRNAxBUFFET_Sharpe_Longo_Prazo.xlsx")

################################################################################





#### Backtesting

## Medias dos retornos
Media_RetornosMediosBuffet = sum(RetornosMediosBuffet)/length((RetornosMediosBuffet))
Media_SP500 <- sum(PosCovidSP500)/length((PosCovidSP500))
Media_Media_C_Net_T_Comparativa = sum(Media_C_Net_T_Comparativa)/length(Media_C_Net_T_Comparativa)
Media_RetornoMedioMArkovitz = sum(RetornoMedioMArkovitz)/length(RetornoMedioMArkovitz)
Media_Ret_Medio_RNA_Mkv = sum(Ret_Medio_RNA_T_Mkv)/length(Ret_Medio_RNA_T_Mkv)
#Media_Ret_Mean_Var = mean(RetornoMedioMean_Variance_Mkv)
Media_RetornoMedioMaxIS = mean(RetornoMedioMaxIS)
Media_RetornoMedioMaxIS_RNAt = mean(RetornoMedioMaxIS_RNAt)

# Vari?ncias
AVERAGE_Return = function(X) {
  T=length(X)
  RC=NULL
  RC[1]=1+X[1]
  for(i in 2:T){
    RC[i]=RC[i-1]*(1+X[i])
  }
  AR = RC[T]^(252/T)-1
  return(AR)
}

CUMULATIVE_Return = function(X) {
  T=length(X)
  RC=NULL
  RC[1]=1+X[1]
  for(i in 2:T){
    RC[i]=RC[i-1]*(1+X[i])
  }
  AR = RC[T]^(252/T)-1
  #return(AR)
  return(RC[T]-1)
}

ANNUALIZED_VOLATILITY = function(X) {
  AV=252^0.5*sd(X)
  return(AV)
}

Var_95 = function(X) {
  Var_95=mean(X)+sd(X)*qnorm(0.05)
  return(Var_95)
}

Var_95_ = function(X) {
  media=mean(X)
  desvio = sd(X)
  Var_95=cvar::VaR(qnorm, 0.05, mean=media, sd=desvio)
  return(Var_95)
}

CVar_95 = function(X) {
  media=mean(X)
  desvio = sd(X)
  CVar_95=cvar::ES(qnorm, 0.05, mean=media, sd=desvio)
  return(CVar_95)
}

#### Indice de sharpe
Rf = 0.0426
IS = function(X) {
  media=mean(X)
  desvio = sd(X)
  T=length(X)
  RC=NULL
  RC[1]=1+X[1]
  for(i in 2:T){
    RC[i]=RC[i-1]*(1+X[i])
  }
  AR = RC[T]^(252/T)-1
  AV=252^0.5*sd(X)
  IS = (AR - Rf)/AV
  return(IS)
}


#### Sortino Rate
MAR = 0.0
SortinoRatio_my = function(X) {
  media=mean(X)
  T=length(X)
  d=NULL
  d2 = NULL
  for(i in 1:T){
    d[i] = if (X[i]>MAR){0}else{-X[i]}
    d2[i] = d[i]^2
  }
  desvio = (sum(d2)/T)^0.5
  SR_My = (media - MAR)/desvio
  return(SR_My)
}

#### Sortino Rate
MAR = 0.0
SortinoRatio_ = function(X) {
  media=mean(X)
  desvio = sd(X)
  T=length(X)
  RC=NULL
  RC[1]=1+X[1]
  for(i in 2:T){
    RC[i]=RC[i-1]*(1+X[i])
  }
  AR = RC[T]^(252/T)-1
  AV=252^0.5*sd(X)
  SR = SortinoRatio(X)
  return(SR)
}

#### Beta CAPM
Beta = function(X,Y) {
  media_X=mean(X)
  desvio_X = sd(X)
  T=length(X)
  media_y = mean(Y)
  desvio_Y = sd(Y)
  Beta = cov(X,Y)/var(Y)
  return(Beta)
}

#### Alfa de Jensen

Alfa = function(X,Y,Rf) {
  media_X=mean(X)
  desvio_X = sd(X)
  T=length(X)
  media_y = mean(Y)
  desvio_Y = sd(Y)
  Beta = cov(X,Y)/var(Y)
  Rf_diario = (1+Rf)^(1/252)-1
  a=media_X-Beta*media_y
  Alfa = a-(Rf_diario*(1-Beta))
  return(Alfa)
}


#### Indice de Treynor
Rf = 0.0426
ITreynor = function(X,Y,Rf) {
  media_X=mean(X)
  desvio_X = sd(X)
  T=length(X)
  media_y = mean(Y)
  desvio_Y = sd(Y)
  Beta = cov(X,Y)/var(Y)
  Rf_diario = (1+Rf)^(1/252)-1
  ITreynor = (media_X - Rf_diario)/Beta
  return(ITreynor)
}

# Tabela de resultados
sumbacktest <- matrix(nrow=7, ncol=11)
sumbacktest[1,1]= round(Media_RetornosMediosBuffet,4)*100
sumbacktest[2,1]= round(Media_SP500,4)*100
sumbacktest[3,1]= round(Media_Media_C_Net_T_Comparativa,4)*100
sumbacktest[4,1]= round(Media_RetornoMedioMArkovitz,4)*100
sumbacktest[5,1]= round(Media_Ret_Medio_RNA_Mkv,4)*100
sumbacktest[6,1]= round(Media_RetornoMedioMaxIS,4)*100
sumbacktest[7,1]= round(Media_RetornoMedioMaxIS_RNAt,4)*100
#sumbacktest[8,1]= round(Media_Ret_Mean_Var,4)*100

sumbacktest[1,2]= round(AVERAGE_Return(RetornosMediosBuffet),4)*100
sumbacktest[2,2]= round(AVERAGE_Return(PosCovidSP500),4)*100
sumbacktest[3,2]= round(AVERAGE_Return(Media_C_Net_T_Comparativa),4)*100
sumbacktest[4,2]= round(AVERAGE_Return(RetornoMedioMArkovitz),4)*100
sumbacktest[5,2]= round(AVERAGE_Return(Ret_Medio_RNA_T_Mkv),4)*100
sumbacktest[6,2]= round(AVERAGE_Return(RetornoMedioMaxIS),4)*100
sumbacktest[7,2]= round(AVERAGE_Return(RetornoMedioMaxIS_RNAt),4)*100
#sumbacktest[8,2]= round(AVERAGE_Return(RetornoMedioMean_Variance_Mkv),4)*100

sumbacktest[1,3]= round(CUMULATIVE_Return(RetornosMediosBuffet),4)*100
sumbacktest[2,3]= round(CUMULATIVE_Return(PosCovidSP500),4)*100
sumbacktest[3,3]= round(CUMULATIVE_Return(Media_C_Net_T_Comparativa),4)*100
sumbacktest[4,3]= round(CUMULATIVE_Return(RetornoMedioMArkovitz),4)*100
sumbacktest[5,3]= round(CUMULATIVE_Return(Ret_Medio_RNA_T_Mkv),4)*100
sumbacktest[6,3]= round(CUMULATIVE_Return(RetornoMedioMaxIS),4)*100
sumbacktest[7,3]= round(CUMULATIVE_Return(RetornoMedioMaxIS_RNAt),4)*100
#sumbacktest[8,3]= round(CUMULATIVE_Return(RetornoMedioMean_Variance_Mkv),4)*100

sumbacktest[1,4]= round(ANNUALIZED_VOLATILITY(RetornosMediosBuffet),4)*100
sumbacktest[2,4]= round(ANNUALIZED_VOLATILITY(PosCovidSP500),4)*100
sumbacktest[3,4]= round(ANNUALIZED_VOLATILITY(Media_C_Net_T_Comparativa),4)*100
sumbacktest[4,4]= round(ANNUALIZED_VOLATILITY(RetornoMedioMArkovitz),4)*100
sumbacktest[5,4]= round(ANNUALIZED_VOLATILITY(Ret_Medio_RNA_T_Mkv),4)*100
sumbacktest[6,4]= round(ANNUALIZED_VOLATILITY(RetornoMedioMaxIS),4)*100
sumbacktest[7,4]= round(ANNUALIZED_VOLATILITY(RetornoMedioMaxIS_RNAt),4)*100
#sumbacktest[8,4]= round(ANNUALIZED_VOLATILITY(RetornoMedioMean_Variance_Mkv),4)*100

sumbacktest[1,5]= round(Var_95_(RetornosMediosBuffet),4)*100
sumbacktest[2,5]= round(Var_95_(PosCovidSP500),4)*100
sumbacktest[3,5]= round(Var_95_(Media_C_Net_T_Comparativa),4)*100
sumbacktest[4,5]= round(Var_95_(RetornoMedioMArkovitz),4)*100
sumbacktest[5,5]= round(Var_95_(Ret_Medio_RNA_T_Mkv),4)*100
sumbacktest[6,5]= round(Var_95_(RetornoMedioMaxIS),4)*100
sumbacktest[7,5]= round(Var_95_(RetornoMedioMaxIS_RNAt),4)*100
#sumbacktest[8,5]= round(Var_95_(RetornoMedioMean_Variance_Mkv),4)*100

sumbacktest[1,6]= round(CVar_95(RetornosMediosBuffet),4)*100
sumbacktest[2,6]= round(CVar_95(PosCovidSP500),4)*100
sumbacktest[3,6]= round(CVar_95(Media_C_Net_T_Comparativa),4)*100
sumbacktest[4,6]= round(CVar_95(RetornoMedioMArkovitz),4)*100
sumbacktest[5,6]= round(CVar_95(Ret_Medio_RNA_T_Mkv),4)*100
sumbacktest[6,6]= round(CVar_95(RetornoMedioMaxIS),4)*100
sumbacktest[7,6]= round(CVar_95(RetornoMedioMaxIS_RNAt),4)*100
#sumbacktest[8,6]= round(CVar_95(RetornoMedioMean_Variance_Mkv),4)*100

sumbacktest[1,7]= round(IS(RetornosMediosBuffet),2)
sumbacktest[2,7]= round(IS(PosCovidSP500),2)
sumbacktest[3,7]= round(IS(Media_C_Net_T_Comparativa),2)
sumbacktest[4,7]= round(IS(RetornoMedioMArkovitz),2)
sumbacktest[5,7]= round(IS(Ret_Medio_RNA_T_Mkv),2)
sumbacktest[6,7]= round(IS(RetornoMedioMaxIS),2)
sumbacktest[7,7]= round(IS(RetornoMedioMaxIS_RNAt),2)
#sumbacktest[8,7]= round(IS(RetornoMedioMean_Variance_Mkv),4)

sumbacktest[1,8]= round(SortinoRatio_my(RetornosMediosBuffet),2)
sumbacktest[2,8]= round(SortinoRatio_my(PosCovidSP500),2)
sumbacktest[3,8]= round(SortinoRatio_my(Media_C_Net_T_Comparativa),2)
sumbacktest[4,8]= round(SortinoRatio_my(RetornoMedioMArkovitz),2)
sumbacktest[5,8]= round(SortinoRatio_my(Ret_Medio_RNA_T_Mkv),2)
sumbacktest[6,8]= round(SortinoRatio_my(RetornoMedioMaxIS),2)
sumbacktest[7,8]= round(SortinoRatio_my(RetornoMedioMaxIS_RNAt),2)
#sumbacktest[8,8]= round(SortinoRatio_my(RetornoMedioMean_Variance_Mkv),4)

sumbacktest[1,9]= round(Beta(RetornosMediosBuffet, PosCovidSP500),2)
sumbacktest[2,9]= round(Beta(PosCovidSP500, PosCovidSP500),2)
sumbacktest[3,9]= round(Beta(Media_C_Net_T_Comparativa, PosCovidSP500),2)
sumbacktest[4,9]= round(Beta(RetornoMedioMArkovitz, PosCovidSP500),2)
sumbacktest[5,9]= round(Beta(Ret_Medio_RNA_T_Mkv, PosCovidSP500),2)
sumbacktest[6,9]= round(Beta(RetornoMedioMaxIS, PosCovidSP500),2)
sumbacktest[7,9]= round(Beta(RetornoMedioMaxIS_RNAt, PosCovidSP500),2)
sumbacktest[7,9]= round(Beta(RetornoMedioMaxIS_RNAt, PosCovidSP500),2)
#sumbacktest[8,9]= round(SortinoRatio_my(RetornoMedioMean_Variance_Mkv),4)

sumbacktest[1,10]= round(Alfa(RetornosMediosBuffet, PosCovidSP500, 0.0426)*100,2)
sumbacktest[2,10]= round(Alfa(PosCovidSP500, PosCovidSP500, 0.0426)*100,2)
sumbacktest[3,10]= round(Alfa(Media_C_Net_T_Comparativa, PosCovidSP500, 0.0426)*100,2)
sumbacktest[4,10]= round(Alfa(RetornoMedioMArkovitz, PosCovidSP500, 0.0426)*100,2)
sumbacktest[5,10]= round(Alfa(Ret_Medio_RNA_T_Mkv, PosCovidSP500, 0.0426)*100,2)
sumbacktest[6,10]= round(Alfa(RetornoMedioMaxIS, PosCovidSP500, 0.0426)*100,2)
sumbacktest[7,10]= round(Alfa(RetornoMedioMaxIS_RNAt, PosCovidSP500, 0.0426)*100,2)
#sumbacktest[8,10]= round(Alfa(RetornoMedioMean_Variance_Mkv, PosCovidSP500, 0.0426)*100,2)


sumbacktest[1,11]= round(ITreynor(RetornosMediosBuffet, PosCovidSP500, 0.0426)*100,2)
sumbacktest[2,11]= round(ITreynor(PosCovidSP500, PosCovidSP500, 0.0426)*100,2)
sumbacktest[3,11]= round(ITreynor(Media_C_Net_T_Comparativa, PosCovidSP500, 0.0426)*100,2)
sumbacktest[4,11]= round(ITreynor(RetornoMedioMArkovitz, PosCovidSP500, 0.0426)*100,2)
sumbacktest[5,11]= round(ITreynor(Ret_Medio_RNA_T_Mkv, PosCovidSP500, 0.0426)*100,2)
sumbacktest[6,11]= round(ITreynor(RetornoMedioMaxIS, PosCovidSP500, 0.0426)*100,2)
sumbacktest[7,11]= round(ITreynor(RetornoMedioMaxIS_RNAt, PosCovidSP500, 0.0426)*100,2)
#sumbacktest[8,11]= round(ITreynor(RetornoMedioMean_Variance_Mkv, PosCovidSP500, 0.0426)*100,2)

rownames(sumbacktest)= c("Buffet","SP500", "ANNt_Eq", "Markowitz", "ANNt_Mkv",
                         "MaxSharpe", "ANNt_Sharpe")
colnames(sumbacktest) = c("Average Return (% a.d.)","Annualized Return (% a.a.)",
                          "Cumulative Return (% a.p.)", "Annualized Volatility (% a.a.)",
                          "VaR 95% (% a.d.)", "CVaR 95% (% a.d.)",
                          "Sharpe Ratio (Dimensionless)", "Sortino Ratio",
                          "CAPM Beta", "Alfa Jensen (% a.d.)", "Treynor (%a.d.)")
View(sumbacktest)
Portfolio = rownames(as.data.frame(sumbacktest))
SUMBACKTEST_Df = mutate(as.data.frame(Portfolio),
                        as.data.frame(sumbacktest))
write_xlsx(SUMBACKTEST_Df, "SUMBACKTEST_Buffet_Sharpe.xlsx")

# Geração da Matriz de comparação dos Retornos Acumulados
Comparativo = matrix(nrow=length(RetornosMediosBuffet), ncol=7)
Comparativo[1,1] = RetornosMediosBuffet[1,]
Comparativo[1,2] = PosCovidSP500[1,]
Comparativo[1,3] = Media_C_Net_T_Comparativa[1,]
Comparativo[1,4] = RetornoMedioMArkovitz[1,]
Comparativo[1,5] = Ret_Medio_RNA_T_Mkv [1,]
Comparativo[1,6] = RetornoMedioMaxIS [1,]
Comparativo[1,7] = RetornoMedioMaxIS_RNAt [1,]

for(i in 2:length(RetornosMediosBuffet)) {
  Comparativo[i,1] = (Comparativo[i-1,1]+1)*(RetornosMediosBuffet[i,]+1)-1
  Comparativo[i,2] = (as.matrix(Comparativo[i-1,2])+1)*
    (as.matrix(PosCovidSP500[i,])+1)-1
  Comparativo[i,3] = (as.matrix(Comparativo[i-1,3])+1)*
    (as.matrix(Media_C_Net_T_Comparativa [i,])+1)-1
  Comparativo[i,4] = (as.matrix(Comparativo[i-1,4])+1)*
    (as.matrix(RetornoMedioMArkovitz [i,])+1)-1
  Comparativo[i,5] = (as.matrix(Comparativo[i-1,5])+1)*
    (as.matrix(Ret_Medio_RNA_T_Mkv[i,])+1)-1
  Comparativo[i,6] = (as.matrix(Comparativo[i-1,6])+1)*
    (as.matrix(RetornoMedioMaxIS[i,])+1)-1
  Comparativo[i,7] = (as.matrix(Comparativo[i-1,7])+1)*
    (as.matrix(RetornoMedioMaxIS_RNAt[i,])+1)-1
}
colnames(Comparativo)= c("Buffet","SP500", "ANNt_Eq", "Markowitz", "ANNt_Mkv",
                         "MaxSharpe", "ANNt_Sharpe")
rownames(Comparativo) = rownames(PosCovidBuffet)
#################################################################################

















##Gráfico Comparativo dos Retornos Acumulados das Carteiras

Eixo_X = rownames(PosCovidBuffet)
nline = nrow(Comparativo)
Comparativo = as.data.frame(Comparativo)
nline = nrow(Comparativo)
pdf("Comparativo.pdf")
cores <- c("black", "red", "blue", "green", "darkgreen", "brown", "gray")
matplot(cbind(Comparativo$SP500,
              Comparativo$Buffet,
              Comparativo$ANNt_Eq,
              Comparativo$ANNt_Mkv,
              Comparativo$Markovitz,
              Comparativo$MaxSharpe,
              Comparativo$ANNt_Sharpe),
        type = "l", lwd = 1, xaxt = "n", col = cores,
        xlab = paste("Periodo"),
        ylab = "Retornos Acumulados",
        main = paste("Comparativo dos Retornos Acumulados"))
legend("bottomright", cex= 0.7, legend = c("SP500",
                                           "Buffet",
                                           "ANNt_Eq",
                                           "ANNt_Mkw",
                                           "ANNt_Sharpe",
                                           "Markowitz",
                                           "Sharpe"),
       lty = 2,
       lwd = 3,
       bty = "o",
       col = cores)
axis(1, 1:nline, Eixo_X)
#axis(1, 1:nline, las=1)
#axis(1, paste(rownames(Comparativo)))
#text(nline/2, min(Comparativo), labels=(paste("
#                   Período de teste:", xlab= Inicio_dataPredict, "a",
#                                         xlab = Fim_dataPredict)), cex= 0.8)
dev.off()


RetornosMediosBuffet = as.xts(RetornosMediosBuffet)
colnames(PosCovidSP500)="SP500"
rownames(PosCovidSP500)= Eixo_X
PosCovidSP500 = as.xts(PosCovidSP500)
Media_C_Net_T_Comparativa = as.xts(Media_C_Net_T_Comparativa)

pdf("Cartas_Comparativas.pdf")
Comparativo = as.xts(Comparativo)
#charts.PerformanceSummary(Comparativo$SP500)
#charts.PerformanceSummary(Comparativo$Buffet)
#charts.PerformanceSummary(Comparativo$RNAt_Eq)
#charts.PerformanceSummary(Comparativo$Markovitz)
#charts.PerformanceSummary(Comparativo$RNAt_Mkv)
#Legenda=colnames(Comparativo)
#plot(Comparativo)


dev.off()

##### Cumulative Returns Graphic

par(#mfrow=c(2,2),
  #mar=c(2,2,2,2),
  oma=c(1,2,1,1))

library("ggplot2")
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A", cex=0.8)

Eixo = c(1:nrow(Comparativo))
Eixo_X = rownames(as.data.frame(Comparativo))
Comparativo2 = as.data.frame(Comparativo)
#Eixo_X2 = c(1,
#            round(nrow(Comparativo)/4,0),
#            round(nrow(Comparativo)/2,0),
#            round(nrow(Comparativo)*3/4,0),
#            nrow(Comparativo))
if(nrow(Comparativo)>200) {Eixo_X2 = c(1, 100, 200, 300, 400, 500, 600)
} else {Eixo_X2 = c(1, 50, 100, 149, 200, 250, 300)}
Eixo_X3 = rownames(Comparativo2[Eixo_X2,])
Inicio_data = rownames(Comparativo2[1,])
Fim_data = rownames(Comparativo2[nrow(Comparativo2),])
TestComparativo = cbind(as.data.frame(Comparativo), Eixo)
Retornos=TestComparativo$SP500
Periodos=TestComparativo$Eixo
z = TestComparativo$Buffet
w = TestComparativo$ANNt_Eq
t = TestComparativo$ANNt_Mkv
v = TestComparativo$ANNt_Sharpe
s = TestComparativo$Markowitz
u = TestComparativo$MaxSharpe
cores <- c("black", "red", "blue", "green", "darkgreen", "brown", "gray")
plot(Periodos, Retornos,
     type ="l",
     xaxt = "n",
     ylab = "Cumulative Returns",
     xlab = "Period",
     las =1,
     #xaxp = c(1,nline, 5),
     ylim = c(min(Comparativo), max(Comparativo)))
lines(z, col = c("red"))
lines(w, col = c("blue"))
lines(t, col = c("green"))
lines(v, col = c("darkgreen"))
lines(s, col = c("brown"))
lines(u, col = c("gray"))
axis(1, at=(Eixo_X2), label = Eixo_X3)
axis(4, las=1)
#abline(h=-0.4, lty=3)
#abline(h=-0.2, lty=3)
#abline(h= 0.0, lty=3)
#abline(h= 0.2, lty=3)
#abline(h= 0.4, lty=3)
#abline(h= 0.6, lty=3)
#abline(h= 0.8, lty=3)
#abline(v=nline/1, lty=3)
#abline(v=nline/2, lty=3)
#abline(v=nline*3/4, lty=3)
#abline(v=nline/4, lty=3)
#abline(v=1, lty=3)
grid(nx = NULL, ny = NULL, lty =3, lwd = 1, col = "gray")
#title(main = "Comparativo", font.main = 1, line = 1.5)
#title(main = paste("Comparativo           ",
#                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
title("Comparative")
title(main = paste(
  xlab= Inicio_data,"/", xlab= Fim_data),
  line = 0.5,
  cex = 0.5,
  font.main = 1)
legend("topleft", cex= 1.0, legend = c(    "SP500",
                                           "Buffet",
                                           "ANNt_Eq",
                                           "ANNt_Mkw",
                                           "ANNt_Sharpe",
                                           "Markowitz",
                                           "Sharpe"),
       lty = 2,
       lwd = 3,
       #bty = "o",
       bty = "n",
       col = cores)
#box.col = "white")


### New Grafic

pdf("New_grafic_Comparativo.pdf")
png(file="New_grafic_Comparativo_Buffet_Sharpe.png", width=1920, height=1920, res=296, family = "A")
par(#mfrow=c(2,2),
  #mar=c(2,2,2,2),
  oma=c(1,2,1,1))

library("ggplot2")
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A", cex=0.8)

Eixo = c(1:nrow(Comparativo))
Eixo_X = rownames(as.data.frame(Comparativo))
Comparativo2 = as.data.frame(Comparativo)
#Eixo_X2 = c(1,
#            round(nrow(Comparativo)/4,0),
#            round(nrow(Comparativo)/2,0),
#            round(nrow(Comparativo)*3/4,0),
#            nrow(Comparativo))
if(nrow(Comparativo)>200) {Eixo_X2 = c(1, 100, 200, 300, 400, 500, 600)
} else {Eixo_X2 = c(1, 50, 100, 149, 200, 250, 300)}
Eixo_X3 = rownames(Comparativo2[Eixo_X2,])
Inicio_data = rownames(Comparativo2[1,])
Fim_data = rownames(Comparativo2[nrow(Comparativo2),])
TestComparativo = cbind(as.data.frame(Comparativo), Eixo)
Retornos=TestComparativo$SP500
Periodos=TestComparativo$Eixo
z = TestComparativo$Buffet
w = TestComparativo$ANNt_Eq
t = TestComparativo$ANNt_Mkv
v = TestComparativo$ANNt_Sharpe
s = TestComparativo$Markowitz
u = TestComparativo$MaxSharpe
cores <- c("black", "red", "blue", "green", "darkgreen", "brown", "gray")
plot(Periodos, Retornos,
     type ="l",
     xaxt = "n",
     ylab = "Cumulative Returns",
     xlab = "Period",
     las =1,
     #xaxp = c(1,nline, 5),
     ylim = c(min(Comparativo), max(Comparativo)))
lines(z, col = c("red"))
lines(w, col = c("blue"))
lines(t, col = c("green"))
lines(v, col = c("darkgreen"))
lines(s, col = c("brown"))
lines(u, col = c("gray"))
axis(1, at=(Eixo_X2), label = Eixo_X3)
axis(4, las=1)
#abline(h=-0.4, lty=3)
#abline(h=-0.2, lty=3)
#abline(h= 0.0, lty=3)
#abline(h= 0.2, lty=3)
#abline(h= 0.4, lty=3)
#abline(h= 0.6, lty=3)
#abline(h= 0.8, lty=3)
#abline(v=nline/1, lty=3)
#abline(v=nline/2, lty=3)
#abline(v=nline*3/4, lty=3)
#abline(v=nline/4, lty=3)
#abline(v=1, lty=3)
grid(nx = NULL, ny = NULL, lty =3, lwd = 1, col = "gray")
#title(main = "Comparativo", font.main = 1, line = 1.5)
#title(main = paste("Comparativo           ",
#                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
title("Comparative")
title(main = paste(
  xlab= Inicio_data,"/", xlab= Fim_data),
  line = 0.5,
  cex = 0.5,
  font.main = 1)
legend("topleft", cex= 1.0, legend = c(    "SP500",
                                           "Buffet",
                                           "ANNt_Eq",
                                           "ANNt_Mkw",
                                           "ANNt_Sharpe",
                                           "Markowitz",
                                           "Sharpe"),
       lty = 2,
       lwd = 3,
       #bty = "o",
       bty = "n",
       col = cores)
#box.col = "white")

dev.off()




}









