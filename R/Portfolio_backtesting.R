#' Portfolio_backtesting
#' Realize the portfolio backtest.
#'
#' @param Date_Initial_Backtesting Date initial of the backtest
#' @param Date_Final_Backtesting Date final of the backtest
#' @examples
#' Date_Initial_Backtesting =''
#' Date_Final_Backtesting =''
#' Portfolio_backtesting('', '')
#'
#' @export
Portfolio_backtesting <- function(Date_Initial_Backtesting,Date_Final_Backtesting) {

  library(dplyr)
  library(writexl)
  options(warn=-1)
  load('~/Comparativo_RETORNOS.rda')
  load('~/RM.rda')
  load('~/Rf.rda')
  Comparativo_RETORNOS=data.frame(Comparativo_RETORNOS)


  if(Date_Initial_Backtesting==''){
    Corte1=1
  } else{

    if(length(which(rownames(Comparativo_RETORNOS)==Date_Initial_Backtesting))==0){
      while(length(which(rownames(Comparativo_RETORNOS)==Date_Initial_Backtesting))==0){
        dia=as.Date(Date_Initial_Backtesting)
        new_day=dia+1
        Date_Initial_Backtesting = as.character(new_day)
      }
    }


  Corte1=which(rownames(as.data.frame(Comparativo_RETORNOS))==Date_Initial_Backtesting)
  }


  if(Date_Final_Backtesting==''){
    Corte2=nrow(Comparativo_RETORNOS)
  } else{


    if(length(which(rownames(Comparativo_RETORNOS)==Date_Final_Backtesting))==0){
      while(length(which(rownames(Comparativo_RETORNOS)==Date_Final_Backtesting))==0){
        dia=as.Date(Date_Final_Backtesting)
        new_day=dia-1
        Date_Final_Backtesting = as.character(new_day)
      }
    }

  Corte2=which(rownames(as.data.frame(Comparativo_RETORNOS))==Date_Final_Backtesting)
  }
  Comparativo_RETORNOS=Comparativo_RETORNOS[Corte1:Corte2,]
  attach(as.data.frame(Comparativo_RETORNOS))


  ## Medias dos retornos
  Media_MF_EQ = mean(MF_EQ)
  Media_MF_MKW = sum(MF_MKW)/length((MF_MKW))
  Media_SP500 <- sum(Comparativo_RETORNOS[,1])/length((Comparativo_RETORNOS[,1]))
  Media_ANNt_EQ = sum(ANNt_EQ)/length(ANNt_EQ)
  Media_MARKOWITZ = sum(MARKOWITZ)/length(MARKOWITZ)
  Media_Ret_Medio_RNA_Mkv = sum(ANNt_MKW)/length(ANNt_MKW)
  #Media_Ret_Mean_Var = mean(RetornoMedioMean_Variance_Mkv)
  Media_SHARPE = mean(SHARPE)
  Media_MF_SHARPE = mean(MF_SHARPE)
  Media_ANNt_SHARPE = mean(ANNt_SHARPE)

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
  #Rf = 0.0311
  IS = function(X,Rf=0) {
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

  Alfa = function(X,Y,Rf=0) {
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
  #Rf = 0.0311
  ITreynor = function(X,Y,Rf=0) {
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
  sumbacktest <- matrix(nrow=9, ncol=11)
  sumbacktest[1,1]= round(Media_SP500,4)*100
  sumbacktest[2,1]= round(Media_MARKOWITZ,4)*100
  sumbacktest[3,1]= round(Media_SHARPE,4)*100
  sumbacktest[4,1]= round(Media_MF_EQ,4)*100
  sumbacktest[5,1]= round(Media_MF_MKW,4)*100
  sumbacktest[6,1]= round(Media_MF_SHARPE,4)*100
  sumbacktest[7,1]= round(Media_ANNt_EQ,4)*100
  sumbacktest[8,1]= round(Media_Ret_Medio_RNA_Mkv,4)*100
  sumbacktest[9,1]= round(Media_ANNt_SHARPE,4)*100
  #sumbacktest[6,1]= round(Media_Ret_Mean_Var,4)*100


  sumbacktest[1,2]= round(AVERAGE_Return(Comparativo_RETORNOS[,1]),4)*100
  sumbacktest[2,2]= round(AVERAGE_Return(MARKOWITZ),4)*100
  sumbacktest[3,2]= round(AVERAGE_Return(SHARPE),4)*100
  sumbacktest[4,2]= round(AVERAGE_Return(MF_EQ),4)*100
  sumbacktest[5,2]= round(AVERAGE_Return(MF_MKW),4)*100
  sumbacktest[6,2]= round(AVERAGE_Return(MF_SHARPE),4)*100
  sumbacktest[7,2]= round(AVERAGE_Return(ANNt_EQ),4)*100
  sumbacktest[8,2]= round(AVERAGE_Return(ANNt_MKW),4)*100
  sumbacktest[9,2]= round(AVERAGE_Return(ANNt_SHARPE),4)*100
  #sumbacktest[6,2]= round(AVERAGE_Return(RetornoMedioMean_Variance_Mkv),4)*100


  sumbacktest[1,3]= round(CUMULATIVE_Return(Comparativo_RETORNOS[,1]),4)*100
  sumbacktest[2,3]= round(CUMULATIVE_Return(MARKOWITZ),4)*100
  sumbacktest[3,3]= round(CUMULATIVE_Return(SHARPE),4)*100
  sumbacktest[4,3]= round(CUMULATIVE_Return(MF_EQ),4)*100
  sumbacktest[5,3]= round(CUMULATIVE_Return(MF_MKW),4)*100
  sumbacktest[6,3]= round(CUMULATIVE_Return(MF_SHARPE),4)*100
  sumbacktest[7,3]= round(CUMULATIVE_Return(ANNt_EQ),4)*100
  sumbacktest[8,3]= round(CUMULATIVE_Return(ANNt_MKW),4)*100
  sumbacktest[9,3]= round(CUMULATIVE_Return(ANNt_SHARPE),4)*100
  #sumbacktest[6,3]= round(CUMULATIVE_Return(RetornoMedioMean_Variance_Mkv),4)*100


  sumbacktest[1,4]= round(ANNUALIZED_VOLATILITY(Comparativo_RETORNOS[,1]),4)*100
  sumbacktest[2,4]= round(ANNUALIZED_VOLATILITY(MARKOWITZ),4)*100
  sumbacktest[3,4]= round(ANNUALIZED_VOLATILITY(SHARPE),4)*100
  sumbacktest[4,4]= round(ANNUALIZED_VOLATILITY(MF_EQ),4)*100
  sumbacktest[5,4]= round(ANNUALIZED_VOLATILITY(MF_MKW),4)*100
  sumbacktest[6,4]= round(ANNUALIZED_VOLATILITY(MF_SHARPE),4)*100
  sumbacktest[7,4]= round(ANNUALIZED_VOLATILITY(ANNt_EQ),4)*100
  sumbacktest[8,4]= round(ANNUALIZED_VOLATILITY(ANNt_MKW),4)*100
  sumbacktest[9,4]= round(ANNUALIZED_VOLATILITY(ANNt_SHARPE),4)*100
  #sumbacktest[6,4]= round(ANNUALIZED_VOLATILITY(RetornoMedioMean_Variance_Mkv),4)*100


  sumbacktest[1,5]= round(Var_95_(Comparativo_RETORNOS[,1]),4)*100
  sumbacktest[2,5]= round(Var_95_(MARKOWITZ),4)*100
  sumbacktest[3,5]= round(Var_95_(SHARPE),4)*100
  sumbacktest[4,5]= round(Var_95_(MF_EQ),4)*100
  sumbacktest[5,5]= round(Var_95_(MF_MKW),4)*100
  sumbacktest[6,5]= round(Var_95_(MF_SHARPE),4)*100
  sumbacktest[7,5]= round(Var_95_(ANNt_EQ),4)*100
  sumbacktest[8,5]= round(Var_95_(ANNt_MKW),4)*100
  sumbacktest[9,5]= round(Var_95_(ANNt_SHARPE),4)*100
  #sumbacktest[6,5]= round(Var_95_(RetornoMedioMean_Variance_Mkv),4)*100


  sumbacktest[1,6]= round(CVar_95(Comparativo_RETORNOS[,1]),4)*100
  sumbacktest[2,6]= round(CVar_95(MARKOWITZ),4)*100
  sumbacktest[3,6]= round(CVar_95(SHARPE),4)*100
  sumbacktest[4,6]= round(CVar_95(MF_EQ),4)*100
  sumbacktest[5,6]= round(CVar_95(MF_MKW),4)*100
  sumbacktest[6,6]= round(CVar_95(MF_SHARPE),4)*100
  sumbacktest[7,6]= round(CVar_95(ANNt_EQ),4)*100
  sumbacktest[8,6]= round(CVar_95(ANNt_MKW),4)*100
  sumbacktest[9,6]= round(CVar_95(ANNt_SHARPE),4)*100
  #sumbacktest[6,6]= round(CVar_95(RetornoMedioMean_Variance_Mkv),4)*100


  sumbacktest[1,7]= round(IS(Comparativo_RETORNOS[,1]),2)
  sumbacktest[2,7]= round(IS(MARKOWITZ),2)
  sumbacktest[3,7]= round(IS(SHARPE),2)
  sumbacktest[4,7]= round(IS(MF_EQ),2)
  sumbacktest[5,7]= round(IS(MF_MKW),2)
  sumbacktest[6,7]= round(IS(MF_SHARPE),2)
  sumbacktest[7,7]= round(IS(ANNt_EQ),2)
  sumbacktest[8,7]= round(IS(ANNt_MKW),2)
  sumbacktest[9,7]= round(IS(ANNt_SHARPE),2)
  #sumbacktest[6,7]= round(IS(RetornoMedioMean_Variance_Mkv),4)


  sumbacktest[1,8]= round(SortinoRatio_my(Comparativo_RETORNOS[,1]),2)
  sumbacktest[2,8]= round(SortinoRatio_my(MARKOWITZ),2)
  sumbacktest[3,8]= round(SortinoRatio_my(SHARPE),2)
  sumbacktest[4,8]= round(SortinoRatio_my(MF_EQ),2)
  sumbacktest[5,8]= round(SortinoRatio_my(MF_MKW),2)
  sumbacktest[6,8]= round(SortinoRatio_my(MF_SHARPE),2)
  sumbacktest[7,8]= round(SortinoRatio_my(ANNt_EQ),2)
  sumbacktest[8,8]= round(SortinoRatio_my(ANNt_MKW),2)
  sumbacktest[9,8]= round(SortinoRatio_my(ANNt_SHARPE),2)


  sumbacktest[1,9]= round(Beta(Comparativo_RETORNOS[,1], Comparativo_RETORNOS[,1]),2)
  sumbacktest[2,9]= round(Beta(MARKOWITZ, Comparativo_RETORNOS[,1]),2)
  sumbacktest[3,9]= round(Beta(SHARPE, Comparativo_RETORNOS[,1]),2)
  sumbacktest[4,9]= round(Beta(MF_EQ, Comparativo_RETORNOS[,1]),2)
  sumbacktest[5,9]= round(Beta(MF_MKW, Comparativo_RETORNOS[,1]),2)
  sumbacktest[6,9]= round(Beta(MF_SHARPE, Comparativo_RETORNOS[,1]),2)
  sumbacktest[7,9]= round(Beta(ANNt_EQ, Comparativo_RETORNOS[,1]),2)
  sumbacktest[8,9]= round(Beta(ANNt_MKW, Comparativo_RETORNOS[,1]),2)
  sumbacktest[9,9]= round(Beta(ANNt_SHARPE, Comparativo_RETORNOS[,1]),2)


  sumbacktest[1,10]= round(Alfa(Comparativo_RETORNOS[,1], Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[2,10]= round(Alfa(MARKOWITZ, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[3,10]= round(Alfa(SHARPE, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[4,10]= round(Alfa(MF_EQ, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[5,10]= round(Alfa(MF_MKW, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[6,10]= round(Alfa(MF_SHARPE, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[7,10]= round(Alfa(ANNt_EQ, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[8,10]= round(Alfa(ANNt_MKW, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[9,10]= round(Alfa(ANNt_SHARPE, Comparativo_RETORNOS[,1])*100,2)

  sumbacktest[1,11]= round(ITreynor(Comparativo_RETORNOS[,1], Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[2,11]= round(ITreynor(MARKOWITZ, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[3,11]= round(ITreynor(SHARPE, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[4,11]= round(ITreynor(MF_EQ, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[5,11]= round(ITreynor(MF_MKW, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[6,11]= round(ITreynor(MF_SHARPE, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[7,11]= round(ITreynor(ANNt_EQ, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[8,11]= round(ITreynor(ANNt_MKW, Comparativo_RETORNOS[,1])*100,2)
  sumbacktest[9,11]= round(ITreynor(ANNt_SHARPE, Comparativo_RETORNOS[,1])*100,2)

  #rownames(sumbacktest)= c("MF_MKV", RM, "ANNt_EQ", "MARKOWITZ", "ANNt_MKV",
   #                        "SHARPE", "MF_SHARPE", "ANNt_SHARPE")
  rownames(sumbacktest)= c( RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                            "ANNt_EQ", "ANNt_MKW","ANNt_SHARPE")
  colnames(sumbacktest) = c("Average Return (% p.d.)","Annualized Return (% p.a.)",
                            "Cumulative Return (% p.p.)", "Annualized Volatility (% p.a.)",
                            "VaR 95% (% p.d.)", "CVaR 95% (% p.d.)",
                            "Sharpe Ratio (Dimensionless)", "Sortino Ratio",
                            "CAPM Beta", "Jensen´s Alpha (% p.d.)", "Treynor (% p.d.)")
  Summary_Backtest = sumbacktest
  View(Summary_Backtest)
  save(Summary_Backtest, file="~/Summary_Backtest.rda")
  save(Summary_Backtest, file="Summary_Backtest.rda")
  Portfolio = rownames(as.data.frame(sumbacktest))
  SUMBACKTEST_Df = mutate(as.data.frame(Portfolio),
                          as.data.frame(sumbacktest))
  write_xlsx(SUMBACKTEST_Df, "~/SUMMARY_BACKTEST.xlsx")

  x="All backtest realized!"
  print(x)
}
