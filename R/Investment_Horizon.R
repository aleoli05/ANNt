#' Investment Horizon
#'@description
#' Analyzes the return of portfolios with different investment horizons

#'@param Tickers Name of the assets or "Current_SP500_Tickers" for all S&P 500 assets.
#'@param RM Proxy of the market.
#'@param Rf Risk free rate.
#'@param Initial_Date Series start Date, format ('Year-Month-Day'). Assets with values not observed in the series are excluded.
#'@param Initial_Date_Training Training series start Date.
#'@param Final_Date End date of the treatment series.
#'@param Frequency How many times simulate the investiment horizon.
#'@param Periodicity should be one of “daily”, “weekly”, “monthly”.
#'@param Hidden Number of hidden neurons (If ” is the length series). For a good performance use '' to form a square input x hidden matrix of neurons.
#'@param Stepmax Number of replications per asset to train the ANN. For a good performance, use 7500.
#' @param Asymmetry "Negative" or "Positive". Shifts the probability of the return being greater than the proxy to the right or left, "Negative" or "Positive". Default is to the right, "Negative"
#'@param Type_ANNt Select type ANNt: "T1"= NNet_Signal_Traning; "T2"= NNet_t_Training; "T3"= MC_Signal_Training; "T4"= MC_t_Training; "T5"= NNet_Signal_Test; "T6"= NNet_t_Test; "T7"= MC_Signal_Test; "T8"= Type_ANNt: MC_t_Test.
#'@param N_Assets Limit of asset numbers in the portfolio.
#'@param Base Database to use: "yahoo" or "Rus".
#'@param Fun Which technique to apply to generate portfolios:
#' 'S_Out' uses the ANNt_Oliveira_Ceretta_S_Out function, this is the standard;
#' 'Out' uses the ANNt_Oliveira_Cereta_Out function;
#' 'S' uses the ANNt_Oliveira_Cereta_S function;
#' 'Original' uses the ANNt_Oliveira_Ceretta function.
#'@param Specifies_Date Specific dates for the end of training. Used to define
#'the investment horizon of portfolios from specific dates.
#'@param Import Import dates from external data base after first import. "Yes"
#'or "No". "Yes" is the standard.
#' @param Exclude_ticket Deletes any ticket from the ticket list that you want to remove for some reason
#' @param Type_ANN Select the network type: 'ANNt' or 'LSTMt' in RNN from ANNt
#' @param Order If "Yes" processes the asset selection, if "No" uses the already processed assets available in the database
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
#' Specific_Dates <- c(Sys.Date())
#' Investment_Horizon (c('AAPL','XOM','TSLA','KO', 'F'), '^GSPC', Rf, '2024-01-03', '2024-06-03', '', 2,'daily', Hidden= 5, Stepmax = 7500, Type_ANNt='T8', N_Assets = 3)
#'

#' @export
Investment_Horizon <- function(Tickers, RM, Rf, Initial_Date, Final_Date_Training,
                                Final_Date, Frequency, Periodicity, Hidden,
                                Stepmax, Asymmetry='Negative', Type_ANNt,
                                N_Assets,Base='yahoo', Fun='S_Out',
                                Specific_Dates=Sys.Date(),
                                Import='Yes',Exclude_ticket='', Type_ANN='ANNt',
                                Order='Yes'){
  ydev=dev.list()
  if(class(ydev)!="NULL"){
    dev.off()
  }else{print('Starting Investment_Horizon Command')}
  dev.capabilities()

Horizon='Yes'
Exclude = Exclude_ticket
RM_Nome_Backup=RM
save(RM_Nome_Backup,file='~/RM_Nome_Backup.rda')
#load('~/Horizon.rda')

if (Import =='No'){
  if(Order=='Yes'){

  if (Base=='yahoo'){
    Assets_series (Tickers,RM, Initial_Date, Final_Date,'daily', Exclude_ticket = Exclude)
  }
  if(Base=='Rus'){
    Assets_series_Rus (Tickers,RM, Initial_Date, Final_Date,'daily', Exclude_ticket = Exclude)
  }
  if(Base=='Rus_2'){
    Assets_series_Rus_2 (Tickers,RM, Initial_Date, Final_Date,'daily', Exclude_ticket = Exclude)
  }
  }
  load('~/scenario.set.rda')
  scenario_ajustado=scenario.set
}

if(Import=='No'){
  Horizon='No'
  save(Horizon,file='~/Horizon.rda')
}
save(Type_ANN, file='~/Rype_ANN.rda')
save(Horizon,file='~/Horizon.rda')
Tickers_1=Tickers
save(Tickers_1, file='~/Tickers_1.rda')
X = Final_Date
BS= Base
Specifies_Dates=Specific_Dates
RM=RM
if(length(Specific_Dates)!=1){
  Frequency=length((Specific_Dates))
    }

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
Comparativo_Rm_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Comparativo_RETORNOS_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Comparativo_RCum_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Comparativo_Volatility_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Comparativo_Var_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Comparativo_CVar_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Comparativo_Sharpe_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Comparativo_Sortino_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Comparativo_Beta_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Comparativo_Alpha_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Comparativo_Treynor_Horizon_Anual = matrix(nrow=Frequency, ncol=9)
Tempo = c(1:Frequency)

#### Matrix of weights

Weights_MF_EQ_Horizon <- matrix(ncol=60, nrow=(Frequency*2+1))
Weights_MF_EQ_Horizon <- as.data.frame((Weights_MF_EQ_Horizon))
Weights_MF_EQ_Horizon [1,1] <- 'MF_EQ PORTFOLIOS'
Weights_MF_EQ_Horizon [1,2] <- 'ASSETS'
Weights_MF_EQ_Horizon [2,1] <- 'Initial_Date_Testing'
Weights_MF_EQ_Horizon [2,2] <- 'Final_Date_Testing'
Weights_MF_EQ_Horizon [3,2] <- 'Days'

Weights_MF_MKW_Horizon <- matrix(ncol=60, nrow=(Frequency*2+1))
Weights_MF_MKW_Horizon <- as.data.frame((Weights_MF_MKW_Horizon))
Weights_MF_MKW_Horizon [1,1] <- 'MF_MKW PORTFOLIOS'
Weights_MF_MKW_Horizon [1,2] <- 'ASSETS'
Weights_MF_MKW_Horizon [2,1] <- 'Initial_Date_Testing'
Weights_MF_MKW_Horizon [2,2] <- 'Final_Date_Testing'
Weights_MF_MKW_Horizon [3,2] <- 'Days'

Weights_MKW_Horizon <- matrix(ncol=60, nrow=(Frequency*2+1))
Weights_MKW_Horizon <- as.data.frame((Weights_MKW_Horizon))
Weights_MKW_Horizon [1,1] <- 'MARKOWITZ PORTFOLIOS'
Weights_MKW_Horizon [1,2] <- 'ASSETS'
Weights_MKW_Horizon [2,1] <- 'Initial_Date_Testing'
Weights_MKW_Horizon [2,2] <- 'Final_Date_Testing'
Weights_MKW_Horizon [3,2] <- 'Days'

Weights_ANNt_EQ_Horizon <- matrix(ncol=60, nrow=(Frequency*2+1))
Weights_ANNt_EQ_Horizon <- as.data.frame((Weights_ANNt_EQ_Horizon))
Weights_ANNt_EQ_Horizon [1,1] <- 'ANNt_EQ PORTFOLIOS'
Weights_ANNt_EQ_Horizon [1,2] <- 'ASSETS'
Weights_ANNt_EQ_Horizon [2,1] <- 'Initial_Date_Testing'
Weights_ANNt_EQ_Horizon [2,2] <- 'Final_Date_Testing'
Weights_ANNt_EQ_Horizon [3,2] <- 'Days'

Weights_ANNt_MKW_Horizon <- matrix(ncol=60, nrow=(Frequency*2+1))
Weights_ANNt_MKW_Horizon <- as.data.frame((Weights_ANNt_MKW_Horizon))
Weights_ANNt_MKW_Horizon [1,1] <- 'ANNt_MKW PORTFOLIOS'
Weights_ANNt_MKW_Horizon [1,2] <- 'ASSETS'
Weights_ANNt_MKW_Horizon [2,1] <- 'Initial_Date_Testing'
Weights_ANNt_MKW_Horizon [2,2] <- 'Final_Date_Testing'
Weights_ANNt_MKW_Horizon [3,2] <- 'Days'

Weights_Sharpe_Horizon <- matrix(ncol=60, nrow=(Frequency*2+1))
Weights_Sharpe_Horizon <- as.data.frame((Weights_Sharpe_Horizon))
Weights_Sharpe_Horizon [1,1] <- 'SHARPE PORTFOLIOS'
Weights_Sharpe_Horizon [1,2] <- 'ASSETS'
Weights_Sharpe_Horizon [2,1] <- 'Initial_Date_Testing'
Weights_Sharpe_Horizon [2,2] <- 'Final_Date_Testing'
Weights_Sharpe_Horizon [3,2] <- 'Days'

Weights_MF_Sharpe_Horizon <- matrix(ncol=60, nrow=(Frequency*2+1))
Weights_MF_Sharpe_Horizon <- as.data.frame((Weights_MF_Sharpe_Horizon))
Weights_MF_Sharpe_Horizon [1,1] <- 'MF_SHARPE PORTFOLIOS'
Weights_MF_Sharpe_Horizon [1,2] <- 'ASSETS'
Weights_MF_Sharpe_Horizon [2,1] <- 'Initial_Date_Testing'
Weights_MF_Sharpe_Horizon [2,2] <- 'Final_Date_Testing'
Weights_MF_Sharpe_Horizon [3,2] <- 'Days'

Weights_ANNt_Sharpe_Horizon <- matrix(ncol=60, nrow=(Frequency*2+1))
Weights_ANNt_Sharpe_Horizon <- as.data.frame((Weights_ANNt_Sharpe_Horizon))
Weights_ANNt_Sharpe_Horizon [1,1] <- 'ANNt_SHARPE PORTFOLIOS'
Weights_ANNt_Sharpe_Horizon [1,2] <- 'ASSETS'
Weights_ANNt_Sharpe_Horizon [2,1] <- 'Initial_Date_Testing'
Weights_ANNt_Sharpe_Horizon [2,2] <- 'Final_Date_Testing'
Weights_ANNt_Sharpe_Horizon [3,2] <- 'Days'

######################################
for (i in (1:Frequency)){
  if (length(Specific_Dates)!=1){
    Fim_Train= as.Date.character(Specific_Dates[i])
    Inicio = as.character(Fim_Train-treino)
    Inicio_Test = as.character(Fim_Train+1)

    if(length(which(rownames(as.data.frame(scenario.set))==Inicio_Test))==0){
      while(length(which(rownames(as.data.frame(scenario.set))==Inicio_Test))==0){
        dia=as.Date(Inicio_Test)
        new_day=dia+1
        Inicio_Test = as.character(new_day)
      }}

    Fim_Train= as.character(Fim_Train)
     }
  if (length(Specific_Dates)==1){
        Fim_Train= (data2-Interval*i)
        Inicio = as.character(Fim_Train-treino)
        Inicio_Test = as.character(Fim_Train+1)

        if(length(which(rownames(as.data.frame(scenario.set))==Inicio_Test))==0){
          while(length(which(rownames(as.data.frame(scenario.set))==Inicio_Test))==0){
            dia=as.Date(Inicio_Test)
            new_day=dia+1
            Inicio_Test = as.character(new_day)
          }}

        Fim_Train= as.character(Fim_Train)
        }

  if (Import =='No'){
    if(length(which(rownames(scenario_ajustado)==Inicio))==0){
      while(length(which(rownames(scenario_ajustado)==Inicio))==0){
        dia=as.Date(Inicio)
        new_day=dia+1
        Inicio = as.character(new_day)
      }}
    D = 1+ which(rownames(scenario_ajustado)==Inicio)
    D1 = nrow(scenario_ajustado)
  scenario.set = scenario_ajustado[D:D1,]
  save(scenario.set,file='~/scenario.set.rda')
  }

load('~/Tickers_1.rda')
if(Fun=='S_Out'){
  ANNt_Oliveira_Ceretta_S_Out(Tickers=Tickers_1, RM, Rf, Initial_Date=Inicio, Fim_Train,
                              Final_Date, Periodicity, Hidden=Hidden, Stepmax, Asymmetry=Asymmetry, Type_ANNt,
                              N_Assets, Base=BS, Import=Horizon, Exclude_ticket=Exclude,
                              Type_ANN=Type_ANN, Order=Order)
  {
    load('~/Initial_Date_Out.rda')
    load('~/Final_Date_Out.rda')
    Initial_Date_Testing=as.Date(Initial_Date_Out)
    Initial_Date_Testing=as.character(Initial_Date_Testing)
    Final_Date_Testing=Final_Date_Out
    data3 = as.Date.character(Initial_Date_Out)
    data4 = as.Date.character(Final_Date_Out)
    teste_dias = as.numeric(data4-data3)
    Weights_MF_EQ_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_MF_EQ_Horizon [2,2] <- 'Final_Date_Out'
    Weights_MF_MKW_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_MF_MKW_Horizon [2,2] <- 'Final_Date_Out'
    Weights_MKW_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_MKW_Horizon [2,2] <- 'Final_Date_Out'
    Weights_ANNt_EQ_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_ANNt_EQ_Horizon [2,2] <- 'Final_Date_Out'
    Weights_ANNt_MKW_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_ANNt_MKW_Horizon [2,2] <- 'Final_Date_Out'
    Weights_Sharpe_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_Sharpe_Horizon [2,2] <- 'Final_Date_Out'
    Weights_MF_Sharpe_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_MF_Sharpe_Horizon [2,2] <- 'Final_Date_Out'
    Weights_ANNt_Sharpe_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_ANNt_Sharpe_Horizon [2,2] <- 'Final_Date_Out'
  }
}
if(Fun=='Out'){
  ANNt_Oliveira_Ceretta_Out(Tickers=Tickers_1, RM, Rf, Initial_Date=Inicio, Fim_Train,
                            Final_Date, Periodicity, Hidden=Hidden, Stepmax, Asymmetry=Asymmetry, Type_ANNt,
                            N_Assets, Base=BS, Import=Horizon, Exclude_ticket=Exclude,
                            Type_ANN=Type_ANN, Order=Order)
  {
    load('~/Initial_Date_Out.rda')
    load('~/Final_Date_Out.rda')
    Initial_Date_Testing=as.Date(Initial_Date_Out)
    Initial_Date_Testing=as.character(Initial_Date_Testing)
    Final_Date_Testing=Final_Date_Out
    data3 = as.Date.character(Initial_Date_Out)
    data4 = as.Date.character(Final_Date_Out)
    teste_dias = as.numeric(data4-data3)
    Weights_MF_EQ_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_MF_EQ_Horizon [2,2] <- 'Final_Date_Out'
    Weights_MF_MKW_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_MF_MKW_Horizon [2,2] <- 'Final_Date_Out'
    Weights_MKW_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_MKW_Horizon [2,2] <- 'Final_Date_Out'
    Weights_ANNt_EQ_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_ANNt_EQ_Horizon [2,2] <- 'Final_Date_Out'
    Weights_ANNt_MKW_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_ANNt_MKW_Horizon [2,2] <- 'Final_Date_Out'
    Weights_Sharpe_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_Sharpe_Horizon [2,2] <- 'Final_Date_Out'
    Weights_MF_Sharpe_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_MF_Sharpe_Horizon [2,2] <- 'Final_Date_Out'
    Weights_ANNt_Sharpe_Horizon [2,1] <- 'Initial_Date_Out'
    Weights_ANNt_Sharpe_Horizon [2,2] <- 'Final_Date_Out'
  }
}
if(Fun=='S'){
  ANNt_Oliveira_Ceretta_S(Tickers=Tickers_1, RM, Rf, Initial_Date=Inicio, Fim_Train,
                          Final_Date, Periodicity, Hidden=Hidenn, Stepmax, Asymmetry=Asymmetry, Type_ANNt,
                          N_Assets, Base=BS, Import=Horizon, Exclude_ticket=Exclude,
                          Type_ANN=Type_ANN, Order=Order)
  load('~/Initial_Date_Testing.rda')
  load('~/Final_Date_Testing.rda')
  data3 = as.Date.character(Initial_Date_Testing)
  data4 = as.Date.character(Final_Date_Testing)
  teste_dias = as.numeric(data4-data3)
}
if(Fun=='Original'){
  ANNt_Oliveira_Ceretta(Tickers=Tickers_1, RM, Rf, Initial_Date=Inicio, Fim_Train,
                        Final_Date, Periodicity, Hidden=Hidden, Stepmax, Asymmetry=Asymmetry, Type_ANNt,
                        N_Assets, Base=BS, Import=Horizon, Exclude_ticket=Exclude,
                        Type_ANN=Type_ANN, Order=Order)
  load('~/Initial_Date_Testing.rda')
  load('~/Final_Date_Testing.rda')
  data3 = as.Date.character(Initial_Date_Testing)
  data4 = as.Date.character(Final_Date_Testing)
  teste_dias = as.numeric(data4-data3)
}


load('~/Pesos_MFractal_2.rda')
load('~/Pesos_MFractal_Mkv2.rda')
load('~/Pesos_C_Markov2.rda')
load('~/Pesos_ANNt_Eq2.rda')
load('~/Pesos_ANNt_Mkv2.rda')
load('~/Weight_Sharpe_1.rda')
load('~/Weight_Sharpe_MF.rda')
load('~/Weight_ANNt_Sharpe.rda')
load('~/Summary_Backtest.rda')

Comparativo_Rm_Horizon_Anual[i,] = Summary_Backtest[,1]
Comparativo_RETORNOS_Horizon_Anual[i,] = Summary_Backtest[,2]
Comparativo_RCum_Horizon_Anual[i,] = Summary_Backtest[,3]
Comparativo_Volatility_Horizon_Anual[i,] = Summary_Backtest[,4]
Comparativo_Var_Horizon_Anual[i,] = Summary_Backtest[,5]
Comparativo_CVar_Horizon_Anual[i,] = Summary_Backtest[,6]
Comparativo_Sharpe_Horizon_Anual[i,] = Summary_Backtest[,7]
Comparativo_Sortino_Horizon_Anual[i,] = Summary_Backtest[,8]
Comparativo_Beta_Horizon_Anual[i,] = Summary_Backtest[,9]
Comparativo_Alpha_Horizon_Anual[i,] = Summary_Backtest[,10]
Comparativo_Treynor_Horizon_Anual[i,] = Summary_Backtest[,11]
Tempo[i]=Inicio_Test



#######
if (i==1){
  x=2
}

for(k in (1:ncol(Pesos_MFractal_2))){
  Weights_MF_EQ_Horizon[x+i,1]=Initial_Date_Testing
  Weights_MF_EQ_Horizon[x+i,2]=Final_Date_Testing
  Weights_MF_EQ_Horizon[1+x+i,1]=teste_dias
  Weights_MF_EQ_Horizon[1+x+i,2]='Days'
  Weights_MF_EQ_Horizon[x+i,k+2]=data.frame(colnames(Pesos_MFractal_2))[k,]
  Weights_MF_EQ_Horizon[1+x+i,k+2]=round(data.frame(Pesos_MFractal_2)[k],2)

}

for(k in (1:ncol(Pesos_MFractal_Mkv2))){
  Weights_MF_MKW_Horizon[x+i,1]=Initial_Date_Testing
  Weights_MF_MKW_Horizon[x+i,2]=Final_Date_Testing
  Weights_MF_MKW_Horizon[1+x+i,1]=teste_dias
  Weights_MF_MKW_Horizon[1+x+i,2]='Days'
  Weights_MF_MKW_Horizon[x+i,k+2]=data.frame(colnames(Pesos_MFractal_Mkv2))[k,]
  Weights_MF_MKW_Horizon[1+x+i,k+2]=round(data.frame(Pesos_MFractal_Mkv2)[k],2)
}

for(k in (1:ncol(Pesos_C_Markov2))){
  Weights_MKW_Horizon[x+i,1]=Initial_Date_Testing
  Weights_MKW_Horizon[x+i,2]=Final_Date_Testing
  Weights_MKW_Horizon[1+x+i,1]=teste_dias
  Weights_MKW_Horizon[1+x+i,2]='Days'
  Weights_MKW_Horizon[x+i,k+2]=data.frame(colnames(Pesos_C_Markov2))[k,]
  Weights_MKW_Horizon[1+x+i,k+2]=round(data.frame(Pesos_C_Markov2)[k],2)
}

for(k in (1:ncol(Pesos_ANNt_Eq2))){
  Weights_ANNt_EQ_Horizon[x+i,1]=Initial_Date_Testing
  Weights_ANNt_EQ_Horizon[x+i,2]=Final_Date_Testing
  Weights_ANNt_EQ_Horizon[1+x+i,1]=teste_dias
  Weights_ANNt_EQ_Horizon[1+x+i,2]='Days'
  Weights_ANNt_EQ_Horizon[x+i,k+2]=data.frame(colnames(Pesos_ANNt_Eq2))[k,]
  Weights_ANNt_EQ_Horizon[1+x+i,k+2]=round(data.frame(Pesos_ANNt_Eq2)[k],2)
}

for(k in (1:ncol(Pesos_ANNt_Mkv2))){
  Weights_ANNt_MKW_Horizon[x+i,1]=Initial_Date_Testing
  Weights_ANNt_MKW_Horizon[x+i,2]=Final_Date_Testing
  Weights_ANNt_MKW_Horizon[1+x+i,1]=teste_dias
  Weights_ANNt_MKW_Horizon[1+x+i,2]='Days'
  Weights_ANNt_MKW_Horizon[x+i,k+2]=data.frame(colnames(Pesos_ANNt_Mkv2))[k,]
  Weights_ANNt_MKW_Horizon[1+x+i,k+2]=round(data.frame(Pesos_ANNt_Mkv2)[k],2)
}

for(k in (1:ncol(Weight_Sharpe_1))){
  Weights_Sharpe_Horizon[x+i,1]=Initial_Date_Testing
  Weights_Sharpe_Horizon[x+i,2]=Final_Date_Testing
  Weights_Sharpe_Horizon[1+x+i,1]=teste_dias
  Weights_Sharpe_Horizon[1+x+i,2]='Days'
  Weights_Sharpe_Horizon[x+i,k+2]=data.frame(colnames(Weight_Sharpe_1))[k,]
  Weights_Sharpe_Horizon[1+x+i,k+2]=round(data.frame(Weight_Sharpe_1)[k],2)
}

for(k in (1:ncol(Weight_Sharpe_MF))){
  Weights_MF_Sharpe_Horizon[x+i,1]=Initial_Date_Testing
  Weights_MF_Sharpe_Horizon[x+i,2]=Final_Date_Testing
  Weights_MF_Sharpe_Horizon[1+x+i,1]=teste_dias
  Weights_MF_Sharpe_Horizon[1+x+i,2]='Days'
  Weights_MF_Sharpe_Horizon[x+i,k+2]=data.frame(colnames(Weight_Sharpe_MF))[k,]
  Weights_MF_Sharpe_Horizon[1+x+i,k+2]=round(data.frame(Weight_Sharpe_MF)[k],2)
}

for(k in (1:ncol(Weight_ANNt_Sharpe))){
  Weights_ANNt_Sharpe_Horizon[x+i,1]=Initial_Date_Testing
  Weights_ANNt_Sharpe_Horizon[x+i,2]=Final_Date_Testing
  Weights_ANNt_Sharpe_Horizon[1+x+i,1]=teste_dias
  Weights_ANNt_Sharpe_Horizon[1+x+i,2]='Days'
  Weights_ANNt_Sharpe_Horizon[x+i,k+2]=data.frame(colnames(Weight_ANNt_Sharpe))[k,]
  Weights_ANNt_Sharpe_Horizon[1+x+i,k+2]=round(data.frame(Weight_ANNt_Sharpe)[k],2)
}
x=x+1


save(Weights_MF_EQ_Horizon,file='~/Weights_MF_EQ_Horizon.rda')
write_xlsx(as.data.frame(Weights_MF_EQ_Horizon), "~/Weights_MF_EQ_Horizon.xlsx")

save(Weights_MF_MKW_Horizon,file='~/Weights_MF_MKW_Horizon.rda')
write_xlsx(as.data.frame(Weights_MF_MKW_Horizon), "~/Weights_MF_MKW_Horizon.xlsx")

save(Weights_MKW_Horizon,file='~/Weights_MKW_Horizon.rda')
write_xlsx(as.data.frame(Weights_MKW_Horizon), "~/Weights_MKW_Horizon.xlsx")

save(Weights_ANNt_EQ_Horizon,file='~/Weights_ANNt_EQ_Horizon.rda')
write_xlsx(as.data.frame(Weights_ANNt_EQ_Horizon), "~/Weights_ANNt_EQ_Horizon.xlsx")

save(Weights_ANNt_MKW_Horizon,file='~/Weights_ANNt_MKW_Horizon.rda')
write_xlsx(as.data.frame(Weights_ANNt_MKW_Horizon), "~/Weights_ANNt_MKW_Horizon.xlsx")

save(Weights_Sharpe_Horizon,file='~/Weights_Sharpe_Horizon.rda')
write_xlsx(as.data.frame(Weights_Sharpe_Horizon), "~/Weights_Sharpe_Horizon.xlsx")

save(Weights_MF_Sharpe_Horizon,file='~/Weights_MF_Sharpe_Horizon.rda')
write_xlsx(as.data.frame(Weights_MF_Sharpe_Horizon), "~/Weights_MF_Sharpe_Horizon.xlsx")

save(Weights_ANNt_Sharpe_Horizon,file='~/Weights_ANNt_Sharpe_Horizon.rda')
write_xlsx(as.data.frame(Weights_ANNt_Sharpe_Horizon), "~/Weights_ANNt_Sharpe_Horizon.xlsx")
#############################################

Inicio_Teste_Datas = Tempo
rownames(Comparativo_Rm_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_Rm_Horizon_Anual)= rownames(Summary_Backtest)
rownames(Comparativo_RETORNOS_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_RETORNOS_Horizon_Anual)= rownames(Summary_Backtest)
rownames(Comparativo_RCum_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_RCum_Horizon_Anual)= rownames(Summary_Backtest)
rownames(Comparativo_Volatility_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_Volatility_Horizon_Anual)= rownames(Summary_Backtest)
rownames(Comparativo_Var_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_Var_Horizon_Anual)= rownames(Summary_Backtest)
rownames(Comparativo_CVar_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_CVar_Horizon_Anual)= rownames(Summary_Backtest)
rownames(Comparativo_Sharpe_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_Sharpe_Horizon_Anual)= rownames(Summary_Backtest)
rownames(Comparativo_Sortino_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_Sortino_Horizon_Anual)= rownames(Summary_Backtest)
rownames(Comparativo_Beta_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_Beta_Horizon_Anual)= rownames(Summary_Backtest)
rownames(Comparativo_Alpha_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_Alpha_Horizon_Anual)= rownames(Summary_Backtest)
rownames(Comparativo_Treynor_Horizon_Anual)=Inicio_Teste_Datas
colnames(Comparativo_Treynor_Horizon_Anual)= rownames(Summary_Backtest)

save(Comparativo_Rm_Horizon_Anual, file='~/Comparativo_Rm_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_Rm_Horizon_Anual), "~/Comparativo_Rm_Horizon_Anual.xlsx")
save(Comparativo_RETORNOS_Horizon_Anual, file='~/Comparativo_RETORNOS_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_RETORNOS_Horizon_Anual), "~/Comparativo_RETORNOS_Horizon_Anual.xlsx")
save(Comparativo_RCum_Horizon_Anual, file='~/Comparativo_RCum_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_RCum_Horizon_Anual), "~/Comparativo_RCum_Horizon_Anual.xlsx")
save(Comparativo_Volatility_Horizon_Anual, file='~/Comparativo_Volatility_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_Volatility_Horizon_Anual), "~/Comparativo_Volatility_Horizon_Anual.xlsx")
save(Comparativo_Var_Horizon_Anual, file='~/Comparativo_Var_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_Var_Horizon_Anual), "~/Comparativo_Var_Horizon_Anual.xlsx")
save(Comparativo_CVar_Horizon_Anual, file='~/Comparativo_CVar_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_CVar_Horizon_Anual), "~/Comparativo_CVar_Horizon_Anual.xlsx")
save(Comparativo_Sharpe_Horizon_Anual, file='~/Comparativo_Sharpe_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_Sharpe_Horizon_Anual), "~/Comparativo_Sharpe_Horizon_Anual.xlsx")
save(Comparativo_Sortino_Horizon_Anual, file='~/Comparativo_Sortino_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_Sortino_Horizon_Anual), "~/Comparativo_Sortino_Horizon_Anual.xlsx")
save(Comparativo_Beta_Horizon_Anual, file='~/Comparativo_Beta_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_Beta_Horizon_Anual), "~/Comparativo_Beta_Horizon_Anual.xlsx")
save(Comparativo_Alpha_Horizon_Anual, file='~/Comparativo_Alpha_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_Alpha_Horizon_Anual), "~/Comparativo_Alpha_Horizon_Anual.xlsx")
save(Comparativo_Treynor_Horizon_Anual, file='~/Comparativo_Treynor_Horizon_Anual.rda')
write_xlsx(as.data.frame(Comparativo_Treynor_Horizon_Anual), "~/Comparativo_Treynor_Horizon_Anual.xlsx")
}
View(Comparativo_RETORNOS_Horizon_Anual)
################################################################################
###Gráfico Comparativo dos Retornos Acumulados das Carteiras

Plot_Returns_Annualized_Horizon <-function(){
load('~/Comparativo_RETORNOS_Horizon_Anual.rda')

options(warn=-1)
Eixo_X = rownames(Comparativo_RETORNOS_Horizon_Anual[,1])
nline = nrow(Comparativo_RETORNOS_Horizon_Anual)
Comparativo_RETORNOS_Horizon_Anual = Comparativo_RETORNOS_Horizon_Anual[rev(seq_len(nrow(Comparativo_RETORNOS_Horizon_Anual))),]
Comparativo_RETORNOS_Horizon_Anual = as.data.frame(Comparativo_RETORNOS_Horizon_Anual)
nline = nrow(Comparativo_RETORNOS_Horizon_Anual)


#########################
Weights_Investment_Horizon(Portfolio='MF_EQ')
Weights_Investment_Horizon(Portfolio='MF_MKW')
Weights_Investment_Horizon(Portfolio='MKW')
Weights_Investment_Horizon(Portfolio='ANNt_EQ')
Weights_Investment_Horizon(Portfolio='ANNt_MKW')
Weights_Investment_Horizon(Portfolio='MF_Sharpe')
Weights_Investment_Horizon(Portfolio='ANNt_Sharpe')
Weights_Investment_Horizon(Portfolio='Sharpe')
Plot_Ratio_Horizon(Ratio="Annualized_Returns")
Plot_Ratio_Horizon(Ratio="Annualized_Volatility")
Plot_Ratio_Horizon(Ratio="Rm")
Plot_Ratio_Horizon(Ratio="RCum")
Plot_Ratio_Horizon(Ratio="CVar")
Plot_Ratio_Horizon(Ratio="Var")
Plot_Ratio_Horizon(Ratio="Beta")
Plot_Ratio_Horizon(Ratio="Alpha")
Plot_Ratio_Horizon(Ratio="Sharpe")
Plot_Ratio_Horizon(Ratio="Sortino")
Plot_Ratio_Horizon(Ratio="Treynor")


#########################
Until_Date=rownames(Comparativo_RETORNOS_Horizon_Anual)[nrow(Comparativo_RETORNOS_Horizon_Anual)]
#Comparativo_RETORNOS_Horizon_Anual=Comparativo_RETORNOS_Horizon_Anual
#  Corte=as.numeric(nrow(as.data.frame(Comparativo_RETORNOS_Horizon_Anual)))

if(Until_Date ==('')){
  #Until_Date = Final_Date_Testing
  Until_Date = rownames(Comparativo_RETORNOS_Horizon_Anual[nrow(Comparativo_RETORNOS_Horizon_Anual),])
}

if(length(which(rownames(Comparativo_RETORNOS_Horizon_Anual)==Until_Date))==0){
  while(length(which(rownames(Comparativo_RETORNOS_Horizon_Anual)==Until_Date))==0){
    dia=as.Date(Until_Date)
    new_day=dia-1
    Until_Date = as.character(new_day)
  }
}

Corte= which(rownames(as.data.frame(Comparativo_RETORNOS_Horizon_Anual))==as.Date(Until_Date))
Coparativo_Backup = Comparativo_RETORNOS_Horizon_Anual
Comparativo_RETORNOS_Horizon_Anual=Comparativo_RETORNOS_Horizon_Anual[1:Corte,]

load('~/RM.rda')

png(file="~/Graphic_Annualized_Returns_Horizon.png", width=1920, height=1920, res=296, family = "A")
par(#mfrow=c(2,2),
  #mar=c(2,2,2,2),
  oma=c(1,2,1,1))

library("ggplot2")
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A", cex=0.8)

Eixo = c(1:nrow(Comparativo_RETORNOS_Horizon_Anual))
Eixo_X = rownames(as.data.frame(Comparativo_RETORNOS_Horizon_Anual))
Comparativo_RETORNOS_Horizon_Anual2 = as.data.frame(Comparativo_RETORNOS_Horizon_Anual)
#Eixo_X2 = c(1,
#            round(nrow(Comparativo_RETORNOS_Horizon_Anual)/4,0),
#            round(nrow(Comparativo_RETORNOS_Horizon_Anual)/2,0),
#            round(nrow(Comparativo_RETORNOS_Horizon_Anual)*3/4,0),
#            nrow(Comparativo_RETORNOS_Horizon_Anual))
if(nrow(Comparativo_RETORNOS_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
#} else{Eixo_X2 = c(1, 50, 100, 149)}
} else{
  if(nrow(Comparativo_RETORNOS_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
  }else{Eixo_X2 = c(1:nrow(Comparativo_RETORNOS_Horizon_Anual))}}
Eixo_X3 = rownames(Comparativo_RETORNOS_Horizon_Anual2[Eixo_X2,])
Eixo_X3 = str_replace(Eixo_X3,"NA","")
Inicio_data = rownames(Comparativo_RETORNOS_Horizon_Anual2[1,])
Fim_data = rownames(Comparativo_RETORNOS_Horizon_Anual2[nrow(Comparativo_RETORNOS_Horizon_Anual2),])
#Fim_data = "2023-03-16"
TestComparativo_RETORNOS_Horizon_Anual = cbind(as.data.frame(Comparativo_RETORNOS_Horizon_Anual), Eixo)
Retornos=TestComparativo_RETORNOS_Horizon_Anual[,1]
Periodos=TestComparativo_RETORNOS_Horizon_Anual$Eixo
s = TestComparativo_RETORNOS_Horizon_Anual$MARKOWITZ
u = TestComparativo_RETORNOS_Horizon_Anual$SHARPE
h = TestComparativo_RETORNOS_Horizon_Anual$MF_EQ
z = TestComparativo_RETORNOS_Horizon_Anual$MF_MKW
p = TestComparativo_RETORNOS_Horizon_Anual$MF_SHARPE
w = TestComparativo_RETORNOS_Horizon_Anual$ANNt_EQ
t = TestComparativo_RETORNOS_Horizon_Anual$ANNt_MKW
q = TestComparativo_RETORNOS_Horizon_Anual$ANNt_SHARPE
plot(Periodos, Retornos,
     type ="l",
     xaxt = "n",
     ylab = "Annualized Returns",
     xlab = "Period",
     las =1,
     #xaxp = c(1,nline, 5),
     ylim = c(min(Comparativo_RETORNOS_Horizon_Anual), max(Comparativo_RETORNOS_Horizon_Anual)))
lines(s, col = c("brown"))
lines(u, col = c("gray"))
lines(h, col = c("yellow"))
lines(z, col = c("red"))
lines(p, col = c("purple"))
lines(w, col = c("blue"))
lines(t, col = c("green"))
lines(q, col = c("darkgreen"))
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
#title(main = "Carteiras RNAt e MF-DFA com 5 Ativos", font.main = 1, line = 1.5)
#title(main = paste("Comparativo_RETORNOS_Horizon_Anual           ",
#                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
title(paste("Annualized Returns over the ",RM," Investment Horizon:", N_Assets, "Assets"))
#title(main = paste(
# xlab= Inicio_data,"/", xlab= Fim_data),
#line = 0.5,
#cex = 0.5,
#font.main = 1)


## Contador de vit?rias Buffet
Contador_MF_DFA = matrix(nrow=149)
legend("topleft",
       #"bottomright",
       legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                  "ANNt_EQ",
                  "ANNt_MKW", "ANNt_SHARPE"),
       cex = 0.8,
       lty = 1,
       #bty = "o",
       bty = "n",
       lwd = 3,
       col = c("black", "brown", "gray", "yellow", "red",
               "purple","blue",
               "green",
               "darkgreen"))


dev.off()
################################################################################
#Presentation
par(#mfrow=c(2,2),
  #mar=c(2,2,2,2),
  oma=c(1,1,1,1))

library("ggplot2")
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A", cex=0.8)

Eixo = c(1:nrow(Comparativo_RETORNOS_Horizon_Anual))
Eixo_X = rownames(as.data.frame(Comparativo_RETORNOS_Horizon_Anual))
Comparativo_RETORNOS_Horizon_Anual2 = as.data.frame(Comparativo_RETORNOS_Horizon_Anual)
#Eixo_X2 = c(1,
#            round(nrow(Comparativo_RETORNOS_Horizon_Anual)/4,0),
#            round(nrow(Comparativo_RETORNOS_Horizon_Anual)/2,0),
#            round(nrow(Comparativo_RETORNOS_Horizon_Anual)*3/4,0),
#            nrow(Comparativo_RETORNOS_Horizon_Anual))
if(nrow(Comparativo_RETORNOS_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
#} else{Eixo_X2 = c(1, 50, 100, 149)}
} else{
  if(nrow(Comparativo_RETORNOS_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
  }else{Eixo_X2 = c(1:nrow(Comparativo_RETORNOS_Horizon_Anual))}}
Eixo_X3 = rownames(Comparativo_RETORNOS_Horizon_Anual2[Eixo_X2,])
Eixo_X3 = str_replace(Eixo_X3,"NA","")
Inicio_data = rownames(Comparativo_RETORNOS_Horizon_Anual2[1,])
Fim_data = rownames(Comparativo_RETORNOS_Horizon_Anual2[nrow(Comparativo_RETORNOS_Horizon_Anual2),])
#Fim_data = "2023-03-16"
TestComparativo_RETORNOS_Horizon_Anual = cbind(as.data.frame(Comparativo_RETORNOS_Horizon_Anual), Eixo)
Retornos=TestComparativo_RETORNOS_Horizon_Anual[,1]
Periodos=TestComparativo_RETORNOS_Horizon_Anual$Eixo
s = TestComparativo_RETORNOS_Horizon_Anual$MARKOWITZ
u = TestComparativo_RETORNOS_Horizon_Anual$SHARPE
h = TestComparativo_RETORNOS_Horizon_Anual$MF_EQ
z = TestComparativo_RETORNOS_Horizon_Anual$MF_MKW
p = TestComparativo_RETORNOS_Horizon_Anual$MF_SHARPE
w = TestComparativo_RETORNOS_Horizon_Anual$ANNt_EQ
t = TestComparativo_RETORNOS_Horizon_Anual$ANNt_MKW
q = TestComparativo_RETORNOS_Horizon_Anual$ANNt_SHARPE
plot(Periodos, Retornos,
     type ="l",
     xaxt = "n",
     ylab = "Annualized Returns",
     xlab = "Period",
     las =1,
     lwd =2,
     #xaxp = c(1,nline, 5),
     ylim = c(min(Comparativo_RETORNOS_Horizon_Anual), max(Comparativo_RETORNOS_Horizon_Anual)))
lines(s, col = c("brown"), lwd=2)
lines(u, col = c("gray"), lwd=2)
lines(h, col = c("yellow"), lwd=2)
lines(z, col = c("red"), lwd=2)
lines(p, col = c("purple"), lwd=2)
lines(w, col = c("blue"), lwd=2)
lines(t, col = c("green"), lwd=2)
lines(q, col = c("darkgreen"), lwd=2)
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
#title(main = "Carteiras RNAt e MF-DFA com 5 Ativos", font.main = 1, line = 1.5)
#title(main = paste("Comparativo_RETORNOS_Horizon_Anual           ",
#                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
title(paste("Annualized Returns over the ",RM," Investment Horizon:", N_Assets, "Assets"))
#title(main = paste(
# xlab= Inicio_data,"/", xlab= Fim_data),
#line = 0.5,
#cex = 0.5,
#font.main = 1)


## Contador de vit?rias Buffet
Contador_MF_DFA = matrix(nrow=149)
legend("topleft",
       #"bottomright",
       legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                  "ANNt_EQ",
                  "ANNt_MKW", "ANNt_SHARPE"),
       cex = 0.6,
       lty = 1,
       #bty = "o",
       bty = "n",
       lwd = 3,
       col = c("black", "brown", "gray", "yellow", "red",
               "purple",
               "blue",
               "green",
               "darkgreen"))
save(Until_Date, file="~/Until_Date.rda")
}

Plot_Returns_Annualized_Horizon()
################################################################################
Backup_ANNt(Investment='Yes')

}
