#' Investment Horizon
#'@description
#' Analyzes the return of portfolios with different investment horizons

#'@param Tickers Name of the assets or "Current_SP500_Tickers" for all S&P 500 assets
#'@param RM Proxy of the market
#'@param Rf Risk free rate
#'@param Initial_Date Series start Date, format ('Year-Month-Day'). Assets with values not observed in the series are excluded
#'@param Initial_Date_Training Training series start Date
#'@param Final_Date End date of the treatment series
#'@param Frequency How many times simulate the investiment horizon
#'@param Periodicity should be one of “daily”, “weekly”, “monthly”
#'@param Hidden Number of hidden neurons (If ” is the length series). For a good performance use '' to form a square input x hidden matrix of neurons
#'@param Stepmax Number of replications per asset to train the ANN. For a good performance, use 7500
#'@param Type_ANNt Select type ANNt: "T1"= NNet_Signal_Traning; "T2"= NNet_t_Training; "T3"= MC_Signal_Training; "T4"= MC_t_Training; "T5"= NNet_Signal_Test; "T6"= NNet_t_Test; "T7"= MC_Signal_Test; "T8"= Type_ANNt: MC_t_Test
#'@param N_Assets Limit of asset numbers in the portfolio
#'@param Base Database to use: "yahoo" or "Rus"
#'@param Fun Which technique to apply to generate portfolios:
#' 'S_Out' uses the ANNt_Oliveira_Ceretta_S_Out function, this is the standard
#' 'Out' uses the ANNt_Oliveira_Cereta_Out function
#' 'S' uses the ANNt_Oliveira_Cereta_S function
#' 'Original' uses the ANNt_Oliveira_Ceretta function

#' @examples
#' # Specify the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' Tickers <-c('AAPL','XOM','TSLA','KO', 'F')
#' RM <-c('^GSPC') #RM the S&P500
#' Rf <- 0
#' Initial_Date <-c('2024-01-03')
#' Final_Date_Training <-c('2024-06-03')
#' Final_Date <-c('2024-11-20')
#' Frequency <- 2
#' Periodicity <- c('daily')
#' Hidden <- 5
#' Stepmax <- 7500
#' Type_ANNt <- 'T8'
#' N_Assets <- 3
#' Base <- 'yahoo'
#' Fun <- 'S_Out'
#' #' # Generates the Adjusted Daily Prices Series from Yahoo Finance
#' Investiment_Horizon (c('AAPL','XOM','TSLA','KO', 'F'), '^GSPC', Rf, '2024-01-03', '2024-06-03', '', 2,'daily', Hidden= 5, Stepmax = 7500, Type_ANNt='T8', N_Assets = 3)
#'

#' @export
Investiment_Horizon <- function(Tickers, RM, Rf, Initial_Date, Final_Date_Training, Final_Date, Frequency, Periodicity, Hidden, Stepmax, Type_ANNt, N_Assets,Base='yahoo', Fun='S_Out'){
X = Final_Date
RM=RM
 if(X==as.character('')){
   Final_Date=as.character(Sys.Date())
   }

data0 = as.Date.character(Initial_Date)
data1 = as.Date.character(Final_Date_Training)
treino = as.numeric(data1-data0)
data2 = as.Date.character(Final_Date)
diferenca_dias = as.numeric(data2-data1)
Interval = round(diferenca_dias/Frequency,0)


# Geração da Matriz de comparação dos Retornos
Comparativo_RETORNOS_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Tempo = c(1:Frequency)

for (i in (1:Frequency)){
Fim_Train= (data2-Interval*i)
Inicio = as.character(Fim_Train-treino)
Inicio_Test = as.character(Fim_Train+1)
Fim_Train= as.character(Fim_Train)



if(Fun=='S_Out'){
  ANNt_Oliveira_Ceretta_S_Out(Tickers, RM, Rf, Initial_Date=Inicio, Fim_Train, Final_Date, Periodicity, Hidden, Stepmax, Type_ANNt, N_Assets, Base='yahoo')
}
if(Fun=='Out'){
  ANNt_Oliveira_Ceretta_Out(Tickers, RM, Rf, Initial_Date=Inicio, Fim_Train, Final_Date, Periodicity, Hidden, Stepmax, Type_ANNt, N_Assets, Base='yahoo')
}
if(Fun=='S'){
  ANNt_Oliveira_Ceretta_S(Tickers, RM, Rf, Initial_Date=Inicio, Fim_Train, Final_Date, Periodicity, Hidden, Stepmax, Type_ANNt, N_Assets, Base='yahoo')
}
if(Fun=='Original'){
  ANNt_Oliveira_Ceretta(Tickers, RM, Rf, Initial_Date=Inicio, Fim_Train, Final_Date, Periodicity, Hidden, Stepmax, Type_ANNt, N_Assets, Base='yahoo')
}

load('~/Summary_Backtest.rda')
Comparativo_RETORNOS_Horizon_Anual[i,] = Summary_Backtest[,2]
Tempo[i]=Inicio_Test

}
Inicio_Teste_Datas = Tempo
rownames(Comparativo_RETORNOS_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_RETORNOS_Horizon_Anual)= rownames(Summary_Backtest)
View(Comparativo_RETORNOS_Horizon_Anual)

}
