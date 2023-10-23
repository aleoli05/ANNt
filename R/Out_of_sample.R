#' Out of Sample
#' @description
#' Performs an out-of-sample run with the weights defined within the sample.
#' You need to generate portfolios first with the Gen_portfolio() command.
#'
#' @export
#' @param Initial_Date_Out Initial Date Out-of-sample
#' @param Final_Date_Out Final Date Out-of-sample
#'
#' @examples
#' Initial_Date_Out <- c('2023-08-03')
#' Final_Date_Out <- c('')
#' # Generate performs out-of-sample
#' Out_of_sample('2023-08-03','')
#'
Out_of_sample <-function(Initial_Date_Out, Final_Date_Out){


  # Duração do processamento 1720/length(dados)=1.2 min)
  load("~/scenario.set.rda") # Carrega objeto scenario.set
  load("~/T8.rda") # Carrega objeto scenario.set
  load("~/I_dataPredict.rda") # Carrega objeto scenario.set
  load("~/F_dataPredict.rda") # Carrega objeto scenario.set
  load("~/Initial_Date_Testing.rda")
  load("~/Final_Date_Testing.rda")
  load("~/Final_Date_Training.rda")
  #load("~/x1.rda")
  load('~/Pesos_MFractal_2.rda')
  load('~/Pesos_MFractal_Mkv.rda')
  load('~/Pesos_MFractal_Mkv2.rda')
  load('~/Pesos_C_Markov2.rda')
  load('~/Pesos_ANNt_Eq2.rda')
  load('~/Pesos_ANNt_Mkv2.rda')
  load('~/Weight_Sharpe_1.rda')
  load('~/Weight_Sharpe_MF.rda')
  load('~/Weight_ANNt_Sharpe.rda')
  load('~/Pesos_MFractal_2.rda')
  load("~/N_Assets.rda")

  scenario.set = data.frame(scenario.set)
  if(Initial_Date_Out==('')){
    Initial_Date_Out=as.Date(Final_Date_Testing)+1
  }

  if(Final_Date_Out==('')){
    Final_Date_Out=rownames(scenario.set[nrow(scenario.set),])
    #Final_Date_Testing=Sys.Date()
  }

  if(length(which(rownames(scenario.set)==Initial_Date_Out))==0){
    while(length(which(rownames(scenario.set)==Initial_Date_Out))==0){
      dia=as.Date(Initial_Date_Out)
      new_day=dia+1
      Initial_Date_Out = as.character(new_day)
    }
  }

  if(length(which(rownames(scenario.set)==Final_Date_Out))==0){
    while(length(which(rownames(scenario.set)==Final_Date_Out))==0){
      dia=as.Date(Final_Date_Out)
      new_day=dia-1
      Final_Date_Out = as.character(new_day)
    }
  }


  if(class(Initial_Date_Out)!=('numeric')){
    Datas1Predict = rownames(scenario.set)[
      (which(rownames(scenario.set)==Initial_Date_Out)):(which(rownames(scenario.set)==Final_Date_Out))]
  }else{
    Datas1Predict = rownames(scenario.set)[(Initial_Date_Out):(which(rownames(scenario.set)==Final_Date_Out))]
  }
  save(Datas1Predict,file='~/Datas1Predict.rda')
  PosCovidSP500 = as.matrix(scenario.set[Datas1Predict,1])
  colnames(PosCovidSP500)=colnames(scenario.set[1])
  rownames(PosCovidSP500)=Datas1Predict
  TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))

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
  options(warn=-1)

  ##############################################################################
  # Carteira de Markovitz de Minima Variância obtida a partir de todos ativos
  Carteira_Markowitz = colnames(Pesos_C_Markov2)
  C_Markowitz = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% Carteira_Markowitz )))
  C_Markowitz = C_Markowitz[Datas1Predict,]
  RetornoMedioMArkovitz= as.matrix(C_Markowitz) %*% Pesos_C_Markov2[1,]
  print(paste('[1] weights of the MARKOWITZ Portfolio:'))
  print(Pesos_C_Markov2)

  # Carteira Multifractal  ####################################################

  Carteira_MFractal = colnames(Pesos_MFractal_2)
  C_MFractal = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% Carteira_MFractal)))
  C_MFractal = C_MFractal[Datas1Predict,]
  Pesos_MFractal = c(rep(1/N_Assets,N_Assets))
  Ret_C_MFractal_EQ = as.matrix(C_MFractal) %*% Pesos_MFractal_2[1,]

  Pesos_MF_Eq2<- round(t(matrix(Pesos_MFractal)),4)
  colnames(Pesos_MF_Eq2) <- Carteira_MFractal
  rownames(Pesos_MF_Eq2)<-'Weight'
  print(paste('[2] weights of the MF_EQ Portfolio:'))
  print(Pesos_MF_Eq2)
  ###############################################################################
  # Carteira de Markovitz de Minima Variância M_Fractal

  Ret_C_MFractal = as.matrix(C_MFractal)%*% Pesos_MFractal_Mkv
  print(paste('[3] weights of the MF_MKW Portfolio:'))
  print(Pesos_MFractal_Mkv2)
  ###############################################################################
  # GMV - Global Minimum Variance
  if(nrow(TodosAtivosPredict)>ncol(TodosAtivosPredict)){
  EPR=colMeans(TodosAtivosPredict)
  COV=var(TodosAtivosPredict)
  GMV=globalMin.portfolio(EPR,COV)
  GMV_Return = GMV$er
  GMV_sd = GMV$sd
  weight_GMV = GMV$weights

    save(GMV_Return,file='~/GMV_Return.rda')
    save(GMV_sd,file='~/GMV_sd.rda')
  }

  scenario.set = data.frame(scenario.set)


  ##############################################################################
  # Carteira Pesos Iguais ANNt
  Carteira_ANNt_EQ = colnames(Pesos_ANNt_Eq2)
  C_ANNt_EQ = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% Carteira_ANNt_EQ)))
  C_ANNt_EQ = C_ANNt_EQ[Datas1Predict,]
  Media_C_Net_T_Comparativa = as.matrix(C_ANNt_EQ) %*% Pesos_ANNt_Eq2[1,]
  print(paste('[4] weights of the ANNt_EQ Portfolio:'))
  print(Pesos_ANNt_Eq2)
  ##############################################################################


  # Carteira RNA NNet dist T com pesos de Markovitz  para Comparação
  Carteira_ANNt_MKW = colnames(Pesos_ANNt_Mkv2)
  C_ANNt_MKW = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% Carteira_ANNt_MKW)))
  C_ANNt_MKW = C_ANNt_MKW[Datas1Predict,]
  Ret_Medio_RNA_T_Mkv = as.matrix(C_ANNt_MKW) %*% Pesos_ANNt_Mkv2[1,]
  print(paste('[5] weights of the ANNt_MKW Portfolio:'))
  print(Pesos_ANNt_Mkv2)

  ################################cARTEIRAS SHARPE ###############################
  ### Carteira Sharpe todos os ativos
  ## Optmization

  ### Retornos carteira Sharpe todos os ativos
  #RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% maxSR.weight.rp
  Carteira_Sharpe = colnames(Weight_Sharpe_1)
  C_Sharpe = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% Carteira_Sharpe)))
  C_Sharpe = C_Sharpe[Datas1Predict,]
  RetornoMedioMaxIS = as.matrix(C_Sharpe)%*% Weight_Sharpe_1[1,]
  print(paste('[6] weights of the SHARPE Portfolio:'))
  print(Weight_Sharpe_1)
  ##############################################################################
  ### Carteira Sharpe MF_DFA
  ##############################################################################
  ## Optmization


  ### Retornos carteira Sharpe MF_DFA Multifractal
  Carteira_MF_Sharpe = colnames(Weight_Sharpe_MF)
  C_MF_Sharpe = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% Carteira_MF_Sharpe)))
  C_MF_Sharpe = C_MF_Sharpe[Datas1Predict,]
  RetornoMedioMaxIS_MFractal = as.matrix(C_MF_Sharpe)%*% Weight_Sharpe_MF[1,]
  print(paste('[7] weights of the MF_SHARPE Portfolio:'))
  print(Weight_Sharpe_MF)
################################################################################
  ### Carteira Sharpe RNAt
  ##############################################################################
  ## Optmization

  Carteira_ANNt_Sharpe = colnames(Weight_ANNt_Sharpe)
  C_ANNt_Sharpe = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% Carteira_ANNt_Sharpe)))
  C_ANNt_Sharpe = C_ANNt_Sharpe[Datas1Predict,]
  RetornoMedioMaxIS_RNAt = as.matrix(C_ANNt_Sharpe)%*% Weight_ANNt_Sharpe[1,]
  print(paste('[8] weights of the ANNt_SHARPE Portfolio:'))
  print(Weight_ANNt_Sharpe)

  #####
  #############################################################################
  #############################################################################

  # Geração da Matriz de comparação dos Retornos
  RM <- colnames(scenario.set[1])
  Comparativo_RETORNOS = matrix(nrow=length(Ret_C_MFractal), ncol=9)
  Comparativo_RETORNOS[,1] = PosCovidSP500
  Comparativo_RETORNOS[,2] = RetornoMedioMArkovitz
  Comparativo_RETORNOS[,3] = RetornoMedioMaxIS
  Comparativo_RETORNOS[,4] = Ret_C_MFractal_EQ
  Comparativo_RETORNOS[,5] = Ret_C_MFractal
  Comparativo_RETORNOS[,6] = RetornoMedioMaxIS_MFractal
  Comparativo_RETORNOS[,7] = Media_C_Net_T_Comparativa
  Comparativo_RETORNOS[,8] = Ret_Medio_RNA_T_Mkv
  Comparativo_RETORNOS[,9] = RetornoMedioMaxIS_RNAt

  #Comparativo_RETORNOS[,6] = RetornoMedioMean_Variance_Mkv
  colnames(Comparativo_RETORNOS)= c(RM,"MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                                    "ANNt_EQ", "ANNt_MKW", "ANNt_SHARPE")
  rownames(Comparativo_RETORNOS) = rownames(PosCovidSP500)
  Datas_Comparativo_RETORNOS = rownames(as.data.frame(Comparativo_RETORNOS))
  Comparativos_RETORNOS_Df = mutate(as.data.frame(Datas_Comparativo_RETORNOS),
                                    as.data.frame(Comparativo_RETORNOS))
  View(Comparativo_RETORNOS)
  save(Comparativo_RETORNOS,file='~/Comparativo_RETORNOS.rda')
  write_xlsx(Comparativos_RETORNOS_Df, "~/Portfolio_Returns.xlsx")

  options(warn=-1)
  #
  # Geração da Matriz de comparação dos Retornos Acumulados
  Comparativo = matrix(nrow=length(Ret_C_MFractal), ncol=9)
  Comparativo[1,1] = PosCovidSP500[1,]
  Comparativo[1,2] = RetornoMedioMArkovitz[1,]
  Comparativo[1,3] = RetornoMedioMaxIS[1,]
  Comparativo[1,4] = Ret_C_MFractal_EQ[1,]
  Comparativo[1,5] = Ret_C_MFractal[1,]
  Comparativo[1,6] = RetornoMedioMaxIS_MFractal[1,]
  Comparativo[1,7] = Media_C_Net_T_Comparativa[1,]
  Comparativo[1,8] = Ret_Medio_RNA_T_Mkv [1,]
  Comparativo[1,9] = RetornoMedioMaxIS_RNAt[1,]


  for(i in 2:length(PosCovidSP500)) {
    Comparativo[i,1] = (Comparativo[i-1,1]+1)*(PosCovidSP500[i,]+1)-1
    Comparativo[i,2] = (as.matrix(Comparativo[i-1,2])+1)*
      (as.matrix(RetornoMedioMArkovitz[i,])+1)-1
    Comparativo[i,3] = (as.matrix(Comparativo[i-1,3])+1)*
      (as.matrix(RetornoMedioMaxIS[i,])+1)-1
    Comparativo[i,4] = (as.matrix(Comparativo[i-1,4])+1)*
      (as.matrix(Ret_C_MFractal_EQ[i,])+1)-1
    Comparativo[i,5] = (as.matrix(Comparativo[i-1,5])+1)*
      (as.matrix(Ret_C_MFractal[i,])+1)-1
    Comparativo[i,6] = (as.matrix(Comparativo[i-1,6])+1)*
      (as.matrix(RetornoMedioMaxIS_MFractal[i,])+1)-1
    Comparativo[i,7] = (as.matrix(Comparativo[i-1,7])+1)*
      (as.matrix(Media_C_Net_T_Comparativa[i,])+1)-1
    Comparativo[i,8] = (as.matrix(Comparativo[i-1,8])+1)*
      (as.matrix(Ret_Medio_RNA_T_Mkv[i,])+1)-1
    Comparativo[i,9] = (as.matrix(Comparativo[i-1,9])+1)*
      (as.matrix(RetornoMedioMaxIS_RNAt[i,])+1)-1
  }

  colnames(Comparativo)= c(RM,"MARKOWITZ", "SHARPE", "MF_MF", "MF_MKW", "MF_SHARPE",
                           "ANNt_EQ", "ANNt_MKW", "ANNt_SHARPE")
  rownames(Comparativo) = rownames(as.data.frame(PosCovidSP500))

  save(Comparativo,file='~/Comparativo.rda')
  #save(Rf,file='~/Rf.rda')

  Comparativo_Df = mutate(as.data.frame(Datas_Comparativo_RETORNOS),
                          as.data.frame(Comparativo))
  write_xlsx(as.data.frame(Comparativo_Df), "~/Cumulative_Portfolio_Retuns.xlsx")

  #### Matrix of weights
  Weights_All <- matrix(ncol=60, nrow=22)
  Weights_All <- as.data.frame((Weights_All))
  Weights_All [1,1] <- 'PORTFOLIOS'
  Weights_All [1,2] <- 'ASSETS'
  Weights_All [2,1] <- 'MARKOWITZ'
  for(k in (1:ncol(Pesos_C_Markov2))){
    Weights_All[2,k+1]=data.frame(colnames(Pesos_C_Markov2))[k,]
    Weights_All[3,k+1]=round(data.frame(Pesos_C_Markov2)[k],2)
  }
  Weights_All [4,1] <- 'MF_EQ'
  for(k in (1:ncol(Pesos_MFractal_2))){
    Weights_All[4,k+1]=data.frame(colnames(Pesos_MFractal_2))[k,]
    Weights_All[5,k+1]=round(data.frame(Pesos_MFractal_2)[k],2)
  }
  Weights_All [6,1] <- 'MF_MKW'
  for(k in (1:ncol(Pesos_MFractal_Mkv2))){
    Weights_All[6,k+1]=data.frame(colnames(Pesos_MFractal_Mkv2))[k,]
    Weights_All[7,k+1]=round(data.frame(Pesos_MFractal_Mkv2)[k],2)
  }
  Weights_All [8,1] <- 'ANNt_EQ'
  for(k in (1:ncol(Pesos_ANNt_Eq2))){
    Weights_All[8,k+1]=data.frame(colnames(Pesos_ANNt_Eq2))[k,]
    Weights_All[9,k+1]=round(data.frame(Pesos_ANNt_Eq2)[k],2)
  }
  Weights_All [10,1] <- 'ANNt_EQ'
  for(k in (1:ncol(Pesos_ANNt_Mkv2))){
    Weights_All[10,k+1]=data.frame(colnames(Pesos_ANNt_Mkv2))[k,]
    Weights_All[11,k+1]=round(data.frame(Pesos_ANNt_Mkv2)[k],2)
  }
  Weights_All [12,1] <- 'SHARPE'
  for(k in (1:ncol(Weight_Sharpe_1))){
    Weights_All[12,k+1]=data.frame(colnames(Weight_Sharpe_1))[k,]
    Weights_All[13,k+1]=round(data.frame(Weight_Sharpe_1)[k],2)
  }
  Weights_All [14,1] <- 'MF_SHARPE'
  for(k in (1:ncol(Weight_Sharpe_MF))){
    Weights_All[14,k+1]=data.frame(colnames(Weight_Sharpe_MF))[k,]
    Weights_All[15,k+1]=round(data.frame(Weight_Sharpe_MF)[k],2)
  }
  Weights_All [16,1] <- 'ANNt_SHARPE'
  for(k in (1:ncol(Weight_ANNt_Sharpe))){
    Weights_All[16,k+1]=data.frame(colnames(Weight_ANNt_Sharpe))[k,]
    Weights_All[17,k+1]=round(data.frame(Weight_ANNt_Sharpe)[k],2)
  }
  sd_sharpe = sd(as.data.frame(Comparativo_RETORNOS)$SHARPE)
  mean_sharpe = mean(as.data.frame(Comparativo_RETORNOS)$SHARPE)

  save(mean_sharpe,file="~/mean_sharpe.rda")
  save(sd_sharpe,file="~/sd_sharpe.rda")
  #save(weight_test,file="~/weight_test.rda")
  save(Initial_Date_Testing,file='~/Initial_Date_Testing.rda')
  #save(Classificacao_MFractal, file='~/Classificacao_MFractal.rda')
  #save(Rf,file='~/Rf.rda')
  #save(pesos_todosPredict,file='~/pesos_todosPredict.rda')
  #save(weight_Sharpe,file='~/weight_Sharpe.rda')
  save(Weights_All,file='~/Weights_All.rda')
  #save(N_Assets,file='~/N_Assets.rda')
  save(Final_Date_Testing,file='~/Final_Date_Testing.rda')


  #### Weights
  save(Pesos_MFractal_2,file='~/Pesos_MFractal_2.rda')
  save(Pesos_MFractal_Mkv2,file='~/Pesos_MFractal_Mkv2.rda')
  save(Pesos_C_Markov2,file='~/Pesos_C_Markov2.rda')
  save(Pesos_ANNt_Eq2,file='~/Pesos_ANNt_Eq2.rda')
  save(Pesos_ANNt_Mkv2,file='~/Pesos_ANNt_Mkv2.rda')
  save(Weight_Sharpe_1,file='~/Weight_Sharpe_1.rda')
  save(Weight_Sharpe_MF,file='~/Weight_Sharpe_MF.rda')
  save(Weight_ANNt_Sharpe,file='~/Weight_ANNt_Sharpe.rda')

  write_xlsx(as.data.frame(Weights_All), "~/Weights_All.xlsx")

  View(Weights_All)
  #View(Comparativo_RETORNOS)

}
