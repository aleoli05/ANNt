#' Select_Architecture
#'@description
#' Analyzes the return of portfolios with different model architecture of ANN

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
#' @param Loss Function: "MSE" for Mean Square Error, "MAE" for Mean Absolute Error,
#' "MADL" for Mean Absolute Directional Loss, and "GMADL" for Generalized Mean Absolute Directional Loss
#' @param Early_Stopping = 'No' or 'Yes'. Default is 'No'. If 'Yes' is necessary inform the value
#' @param Learning_Rate is the Artificial Neural Network learning rate
#' @param Decay L2 regularization or weight decay, add a penalty term to the loss function. "Yes" or "No.
#' No" is default. If  "Yes" is necessary inform the lambda or rate of regularization
#'@param Asymmetry "Negative" or "Positive". Shifts the probability of the return being greater than the proxy to the right or left, "Negative" or "Positive". Default is to the right, "Negative"
#'@param Type_ANNt Select type ANNt: "T1"= NNet_Signal_Traning; "T2"= NNet_t_Training; "T3"= MC_Signal_Training; "T4"= MC_t_Training; "T5"= NNet_Signal_Test; "T6"= NNet_t_Test; "T7"= MC_Signal_Test; "T8"= Type_ANNt: MC_t_Test.
#'@param N_Assets Limit of asset numbers in the portfolio.
#'@param Base Database to use: "yahoo" or "Rus".
#'@param Fun Which technique to apply to generate portfolios:
#' 'S_Out' uses the ANNt_Oliveira_Ceretta_S_Out function, this is the standard;
#' 'Out' uses the ANNt_Oliveira_Cereta_Out function;
#' 'S' uses the ANNt_Oliveira_Cereta_S function;
#' 'Original' uses the ANNt_Oliveira_Ceretta function.
#'@param Specific_Date Specific dates for the end of training. Used to define
#'the investment horizon of portfolios from specific dates.
#'@param Download Download asset prices for external data set or local data set. "Yes" or "No". "Yes" is the standard.
#'@param Import Import dates from external data base after first import in each re balance. "Yes"
#'or "No". "No" is the standard.
#' @param Exclude_ticket Deletes any ticket from the ticket list that you want to remove for some reason
#' @param Type_ANN Select the network type: 'ANNt', 'LSTMt' in RNN from ANNt, or 'SKEWt' to raw excess return data
#' @param Order If "Yes" processes the asset selection, if "No" uses the already processed assets available in the database
#' @param Continue_from Determine if continue from a Specific_Date in the data
#' @param Skew_t Incorporate skew parameter in the probability: "Yes" or "No". Default is "No".
#' @param Initial_Arch Define the Architecture that will initiated the analysis: 1, 2, 3,... Standard is 1.
#' @param Bias include Bias, Yes or No, with auto learning
#' @param Return_Cumulative 'Total' for all period off investment or "Rebalanced" if estimated only within the rebalancing.
#' @examples
#' # Specify the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' ####### Example 1 #######
#' Specific_Dates=c('2025-09-30','2025-06-30',
#' '2025-03-31','2024-12-31',
#' '2024-09-30','2024-06-30',
#' '2024-03-31','2023-12-31',
#' '2023-09-30','2023-06-30',
#' '2023-03-31','2022-12-31'
#' )
#' #######
#' Select_Architecture(
#' Tickers =c('Current_SP500_Tickers'),
#' RM =c('^GSPC'),
#' Rf = 0,
#' Initial_Date =c('2018-01-03'),
#' Final_Date_Training =c('2019-12-31'),
#' Final_Date ='',
#' Frequency = 2,
#' Periodicity = c('daily'),
#' Hidden = c('',5, 5, 5, 5),
#' Stepmax = c(2000, 300, 300, 300, 32),
#' Loss=c("MSE","MAE", "MADL", "GMADL", 'GMADL'),
#' Learning_Rate=c(0.3, 0.3, 0.3, 0.3, 0.3),
#' Decay=c('No', c('Yes',0.05), c('Yes',0.5), c('Yes',0.5), c('Yes',0.5)),
#' Early_Stopping = c('No','Yes', 'Yes', 'Yes', 'Yes'),
#' Asymmetry=c('Positive','Positive','Positive','Positive','Positive'),
#' Type_ANNt = c('T8','T8', 'T8', 'T8', 'T8'),
#' N_Assets = c(20,20,20,20,20),
#' Base = 'yahoo',
#' Fun = 'Original',
#' Specific_Dates = Specific_Dates,
#' Import = 'No',
#' Type_ANN = c('ANNt','ANNt', 'ANNt', 'ANNt', 'LSTMt'),
#' Order = 'Yes',
#' Download = 'Yes',
#' Skew_t=c('No','Yes', 'Yes', 'Yes', 'Yes'),
#' Initial_Arch=1,
#' Bias=c('No','No','No','No','No'),
#' Return_Cumulative =c('Total','Total','Total','Total','Total')
#' )


#' @export

Select_Architecture<-function(
  Tickers =c('Current_SP500_Tickers'),
  RM =c('^GSPC'),
  Rf = 0,
  Initial_Date =c('2018-01-03'),
  Final_Date_Training =c('2019-12-31'),
  Final_Date ='',
  Frequency = 2,
  Periodicity = c('daily'),
  Hidden = c('',5, 5, 5, 5),
  Stepmax = c(2000, 300, 300, 300, 300),
  Loss=c("MSE","MAE", "MADL", "GMADL", 'GMADL'),
  Learning_Rate=c(0.3, 0.3, 0.3, 0.3, 0.3),
  Decay=c('No', c('Yes',0.05), c('Yes',0.5), c('Yes',0.5), c('Yes',0.5)),
  Early_Stopping = c('No','Yes', 'Yes', 'Yes', 'Yes'),
  Asymmetry=c('Positive','Positive','Positive','Positive','Positive'),
  Type_ANNt = c('T8','T8', 'T8', 'T8', 'T8'),
  N_Assets = c(20,20,20,20,20),
  Base = 'yahoo',
  Fun = 'Original' ,
  Specific_Dates = Specific_Dates,
  Import = 'No',
  Exclude_ticket='',
  Type_ANN = c('ANNt','ANNt', 'ANNt', 'ANNt', 'LSTMt'),
  Order='Yes',
  Continue_from='1900-01-01',
  Download='Yes',
  Skew_t=c('No','Yes', 'Yes', 'Yes', 'Yes'),
  Initial_Arch=1,
  Bias=c('No', 'No', 'No', 'No', 'No'),
  Return_Cumulative ='Total'
)

{
  library(stringr)
  library(writexl)
  nrows=length(Specific_Dates)
  ncols=length(Type_ANN)
  save(Initial_Arch, file="~/Initial_Arch.rda")

  if (Initial_Arch==1){
  Select_Arch_RCum_ANNt_Sharpe=matrix(nrow=nrows, ncol=ncols)
  colnames(Select_Arch_RCum_ANNt_Sharpe)=paste('Type',"",1:ncols)
  rownames(Select_Arch_RCum_ANNt_Sharpe)=Specific_Dates

  Select_Arch_Volatility_ANNt_Sharpe=matrix(nrow=nrows, ncol=ncols)
  colnames(Select_Arch_Volatility_ANNt_Sharpe)=paste('Type',1:ncols)
  rownames(Select_Arch_Volatility_ANNt_Sharpe)=Specific_Dates
  }else{
  load('~/Select_Arch_RCum_ANNt_Sharpe.rda')
  load('~/Select_Arch_Volatility_ANNt_Sharpe.rda')
  }


  for (i_arch in (Initial_Arch:length(Type_ANN))){
    if(Hidden[i_arch]!=''){
      Hidden_select=as.numeric(Hidden[i_arch])
    }else{
      Hidden_select=''
    }
    Investment_Horizon(
      Tickers =Tickers,
      RM =RM,
      Rf = Rf,
      Initial_Date =Initial_Date,
      Final_Date_Training =Final_Date_Training,
      Final_Date =Final_Date,
      Frequency = Frequency,
      Periodicity = Periodicity,
      Hidden = Hidden_select,
      Stepmax = Stepmax[i_arch],
      Asymmetry=Asymmetry[i_arch],
      Type_ANNt = Type_ANNt[i_arch],
      N_Assets = N_Assets[i_arch],
      Base = Base,
      Fun = Fun,
      Specific_Dates = Specific_Dates,
      Import = Import,
      Exclude_ticket=Exclude_ticket,
      Type_ANN = Type_ANN[i_arch],
      Order=Order,
      Continue_from=Continue_from,
      Download=Download,
      Skew_t=Skew_t[i_arch],
      Bias=Bias[i_arch],
      Return_Cumulative =Return_Cumulative
    )
    load('~/Comparativo_RCum_Horizon_Anual.rda')
    load('~/Comparativo_Volatility_Horizon_Anual.rda')
    Continue_from='1900-01-01'
    Download='No'
    Select_Arch_RCum_ANNt_Sharpe[,i_arch]=Comparativo_RCum_Horizon_Anual[,9]
    Select_Arch_Volatility_ANNt_Sharpe[,i_arch]=Comparativo_Volatility_Horizon_Anual[,9]
    Initial_Arch=i_arch
    save(Initial_Arch, file="~/Initial_Arch.rda")
    save(Select_Arch_RCum_ANNt_Sharpe,file="~/Select_Arch_RCum_ANNt_Sharpe.rda")
    save(Select_Arch_Volatility_ANNt_Sharpe,file="~/Select_Arch_Volatility_ANNt_Sharpe.rda")
  }
  #save(Initial_Arch,file="~/Initial_Arch.rda")
  #save(Select_Arch_RCum_ANNt_Sharpe,file="~/Select_Arch_RCum_ANNt_Sharpe.rda")
  Medias=round(colMeans(Select_Arch_RCum_ANNt_Sharpe),2)
  Desvios=round(apply(Select_Arch_RCum_ANNt_Sharpe,2,sd),2)
  Sharpe_R=round(Medias/Desvios,2)
  Select_Arch_RCum_ANNt_Sharpe=rbind(Select_Arch_RCum_ANNt_Sharpe,Means=Medias)
  Select_Arch_RCum_ANNt_Sharpe=rbind(Select_Arch_RCum_ANNt_Sharpe,Std_Dev=Desvios)
  Select_Arch_RCum_ANNt_Sharpe=rbind(Select_Arch_RCum_ANNt_Sharpe,Return_per_Risk=Sharpe_R)
  View(Select_Arch_RCum_ANNt_Sharpe)
  t_test= rownames(as.data.frame(Select_Arch_RCum_ANNt_Sharpe))
  Select_Arch_RCum_ANNt_Sharpe_Tabela=as.data.frame(cbind(t_test,Select_Arch_RCum_ANNt_Sharpe))
  write_xlsx(Select_Arch_RCum_ANNt_Sharpe_Tabela, "~/Select_Arch_RCum_ANNt_Sharpe.xlsx")
  #save(Select_Arch_Volatility_ANNt_Sharpe,file="~/Select_Arch_Volatility_ANNt_Sharpe.rda")
  t_test2= rownames(as.data.frame(Select_Arch_Volatility_ANNt_Sharpe))
  Select_Arch_Volatility_ANNt_Sharpe_Tabela=as.data.frame(cbind(t_test2,Select_Arch_Volatility_ANNt_Sharpe))
  write_xlsx(Select_Arch_Volatility_ANNt_Sharpe_Tabela, "~/Select_Arch_Volatility_ANNt_Sharpe.xlsx")

  #t_Test_Diff_Mean_Portfolios('Select_Arch_RCum','Select_Arch_Volatility')
}


