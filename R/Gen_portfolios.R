#' Gen_portfolios
#' @export
#' @param N_Assets limit of asset numbers in the portfolio
#' @param Initial_Date_Testing Initial Date of Test Series
#' @param Final_Date_Testing Final Date Test Series, if '' is the system date
#' @param Rf Risk free rate
#' @param type_ANNt Select type ANNt:
#' "T1"= NNet_Signal_Traning;
#' "T2"= NNet_t_Training;
#' "T3"= MC_Signal_Training;
#' "T4"= MC_t_Training;
#' "T5"= NNet_Signal_Test;
#' "T6"= NNet_t_Test;
#' "T7"= MC_Signal_Test;
#' "T8"= Type_ANNt: MC_t_Test
#' @author Alexandre Silva de Oliveira
#' @examples
#' N_Assets <- 3
#' Initial_Date_Testing <- c('2023-01-03')
#' Final_Date_Testing <- c('')
#' Rf <- 0
#' type_ANNt <- 'T8'
#' # Generate assets portfolio (maximum N assets specified)
#' Gen_portfolios(3,'2023-01-03','',0,'T8')
#'
Gen_portfolios <-function(N_Assets, Initial_Date_Testing, Final_Date_Testing, Rf, type_ANNt){


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

  #load('~/Rf.rda')


  if(type_ANNt=='T1'){
    load('~/T1.rda')
    Type_ANNt=T1
    message("T1= Type_ANNt: NNet_Signal_Traning - Assets with the highest probability obtained with the NeuralNet Package's ANN and Signal probability in the training sample is implemented")
  } else {
    if(type_ANNt=='T2'){
      load('~/T2.rda')
      Type_ANNt=T2
      message("T2= Type_ANNt: NNet_t_Training - Assets with the highest probability obtained with the NeuralNet Package´s ANN and Student's t probability distribution  in the training sample is implemented")
    } else{
      if(type_ANNt=='T3'){
        load('~/T3.rda')
        Type_ANNt=T3
        message("T3= Type_ANNt: MC_Signal_Training - Assets with the highest probability obtained with the manually programmed ANN and Signal probability in the training sample is implemented")
      } else{
        if(type_ANNt=='T4'){
          load('~/T4.rda')
          Type_ANNt=T4
          message("T4= Type_ANNt: MC_t_Training - Assets with the highest probability obtained with the manually programmed ANN and Signal probability in the test sample is implemented")
        } else{
          if(type_ANNt=='T5'){
            load('~/T5.rda')
            Type_ANNt=T5
            message("T5= Type_ANNt: NNet_Signal_Test - Assets with the highest probability obtained with the NeuralNet Package's ANN and Signal probability in the test sample is implemented")
          } else{
            if(type_ANNt=='T6'){
              load('~/T6.rda')
              Type_ANNt=T6
              message("T6= Type_ANNt: NNet_t_Test - Assets with the highest probability obtained with the NeuralNet Package´s ANN and Student's t probability distribution  in the test sample is implemented")
            } else{
              if(type_ANNt=='T7'){
                load('~/T7.rda')
                Type_ANNt=T7
                message("T7= Type_ANNt: MC_Signal_Test - Assets with the highest probability obtained with the manually programmed ANN and Signal probability in the test sample is implemented")
              } else{
                if(type_ANNt=='T8'){
                  load("~/T8.rda") # Carrega objeto scenario.set
                  Type_ANNt=T8
                  message("T8= Type_ANNt: MC_t_Test - Assets with the highest probability obtained with the manually programmed ANN and Student's t probability distribution  in the test sample is implemented")
                }}}}}}}}

  save(Type_ANNt, file='~/Type_ANNt.rda')
  # Duração do processamento 1720/length(dados)=1.2 min)
  load("~/scenario.set.rda") # Carrega objeto scenario.set
  load("~/I_dataPredict.rda") # Carrega objeto scenario.set
  load("~/F_dataPredict.rda") # Carrega objeto scenario.set
  if(exists('Initial_Date_Testing')==FALSE) {
    load("~/Initial_Date_Testing.rda")
  }
  if(exists('Final_Date_Testing')==FALSE){
    load("~/Final_Date_Training.rda")
  }

  if(file.exists("~/Signal_Sharpe.rda")==TRUE){
    load("~/Signal_Sharpe.rda")
  }else{
    Signal_Sharpe=0
    save(Signal_Sharpe, file="~/Signal_Sharpe.rda")
  }

  # h is the number of assets, case the ANNt_Oliveira_Ceretta went used
  if(N_Assets=='n_Assets'){
    load('~/x4.rda')
    N_Assets=x4
  }
  dados<-scenario.set
  nAtivos = ncol(dados)

  Fator_Tempo = 1808/(nrow(dados))
  Unidade=' minute(s)'
  Tempo= round(Fator_Tempo*(ncol((dados))-1),2)
  if (Tempo>120){
    Unidade=' hour(s)'
    Tempo=round(Tempo/60,2)
  }
  dados2=data.frame(dados)
  cat(paste("
   Estimating portfolios, total processing time: ", Tempo, Unidade,"
___________________________________________________________________
", sep=""))

  n_assets=N_Assets

  if(Initial_Date_Testing==('')){
    load("~/x1.rda")
    Final_Date_Training=x1
    D = which(rownames(scenario.set)==Final_Date_Training)
    Initial_Date_Testing=Initial_Date_Testing = rownames(as.data.frame(scenario.set)[D+1,])
  }
  if(length(which(rownames(dados2)==Initial_Date_Testing))==0){
    #Final_Date_Testing=rownames(dados2[nrow(dados2),])
    #Final_Date_Testing=Sys.Date()
    while(length(which(rownames(dados2)==Initial_Date_Testing))==0){
      dia=as.Date(Initial_Date_Testing)
      new_day=dia+1
      Initial_Date_Testing = as.character(new_day)
    }
  }
  if(Final_Date_Testing==('')){
    Final_Date_Testing=rownames(dados2[nrow(dados2),])
    #Final_Date_Testing=Sys.Date()
  }
  if(length(which(rownames(dados2)==Final_Date_Testing))==0){
    #Final_Date_Testing=rownames(dados2[nrow(dados2),])
    #Final_Date_Testing=Sys.Date()
    while(length(which(rownames(dados2)==Final_Date_Testing))==0){
      dia=as.Date(Final_Date_Testing)
      new_day=dia-1
      Final_Date_Testing = as.character(new_day)
    }
  }


  Rf=Rf/100

  scenario.set = data.frame(scenario.set)
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

  ######################## Adjust of nrow series for GMV and Sharpe #############
  all.returns <- TodosAtivosPredict
  #  if (nrow(all.returns)<ncol(all.returns)){
  #    message("The length of the series is less than the number of assets. I will increase the length so I can calculate the Sharpe portfolio of all assets. I'll do this just for this portfolio, ok!")
  #  }
  if ((nrow(all.returns)<ncol(all.returns))==TRUE){
    I = which(rownames(scenario.set)==rownames(all.returns)[nrow(all.returns)])
    I = I-(ncol(all.returns))-9
    Inicio=rownames(scenario.set)[I]
    Fim=rownames(all.returns)[nrow(all.returns)]


    while(length(which(rownames(scenario.set)==Inicio))==0){
      dia=as.Date(Inicio)
      new_day=dia-1
      Inicio = as.character(new_day)
    }

    while(length(which(rownames(scenario.set)==Fim))==0){
      dia=as.Date(Fim)
      new_day=dia-1
      Fim = as.character(new_day)
    }

  }


  all.returns=scenario.set[which(rownames(scenario.set)==as.character(Inicio)):which(rownames(scenario.set)==Fim),-1]

  # y=0
  #  for (k in 1:(nAtivos-1)){
  #  ativo = k
  #Envelope
  #  for( m in 1:nrow(all.returns)){
  #    if(all.returns[m,k]==0){
  #    all.returns[m,k]==0.0000000001
  #   x=1
  #    y=y+x
  #  }}}

  #Contador=round(nrow(all.returns),-1)
  #if(nrow(all.returns)-Contador<0){
  #Contador=Contador-10
  #}
  #Remover= nrow(all.returns)-Contador
  #if(ncol(all.returns)>10){
  #  all.returns <- all.returns[1:(nrow(all.returns)-Remover),]

   # if (nrow(all.returns)-ncol(all.returns)<10){
    #  Inicio=as.Date(rownames(all.returns)[1])
    #  Fim=as.Date(rownames(all.returns)[nrow(all.returns)])
    #  all.returns=scenario.set[(which(rownames(scenario.set)==Inicio)-20):which(rownames(scenario.set)==Fim),-1]
    #}
  #}


  #if(ncol(scenario.set)>480 & ncol(scenario.set)<500){
  #  Fi = ncol(scenario.set)+9
  #  I=(nrow(all.returns)-Fi)+1
  #  all.returns=all.returns[I:nrow(all.returns),]
  #}


  ###############################################################################




  a<-0.9
  N<-1024
  tsx<-MFsim(N,a)

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Probabilidade t de Student
  ResProb_MFractal = matrix(ncol=nAtivos-1,nrow=1)
  ResProb_MFractal = data.frame(ResProb_MFractal)
  colnames(ResProb_MFractal)=colnames(scenario.set)[-1]
  ResProb_MFractal

  # Criando as vari?veis como vetor para treinamento - com datas espec?ficas
  for (k in 1:(nAtivos-1)){
    ativo = k+1
    #Envelope
    test_logic = which(scenario.set[,ativo]==0)
    if(length(test_logic)!=0){
      test_logico=NULL
      for( m in 2:length(test_logic)){
        test_logico[m-1] = test_logic[m]-test_logic[m-1]
      }
      if(any(is.na(test_logico))==TRUE){
        test_logico=0}
    } else{ test_logico=0}


    Test_logico_ =any(test_logico==1)
    if(Test_logico_==TRUE){
      MDM=0
    } else {
      # Calculo das defasagens para cada ativo
      dat_MF <- data.frame(dados[,ativo])
      rownames(dat_MF)=rownames(dados)
      Inicio_data = Initial_Date_Testing
      Fim_data = Final_Date_Testing
      I_data = which(rownames(dat_MF)==Inicio_data)
      F_data = which(rownames(dat_MF)==Fim_data)
      if(class(Initial_Date_Testing)==('numeric')){
        I_data=Initial_Date_Testing
      }
      entradas = as.matrix(dat_MF[I_data:F_data,])
      saidas = as.matrix(dat_MF[(I_data+1):(F_data+1),1])

      scale=10:100
      q<--10:10
      m<-1
      b_MF<-MFDFA(as.timeSeries(dat_MF), scale, m, q)

      # MDM - Market Deficiency Measure
      MDM = 1/2*(abs((b_MF$Hq[1]-0.5))+abs(b_MF$Hq[11]-0.5))
      ## Not run:
      ## Results plot ####
    }

    # Resultados das Probabilidades

    ResProb_MFractal[1,k]= MDM

  }

  # Classificacao Muti_Fractal
  #Classificacao_MFractal <- sort(ResProb_MFractal, decreasing=FALSE)
  nomes = colnames(ResProb_MFractal)
  prob = t(ResProb_MFractal)
  Res=as.matrix(ResProb_MFractal)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  Classificacao_MFractal= matrix(data=ncol(ResProb_MFractal):1, nrow=1, ncol=ncol(ResProb_MFractal))
  colnames(Classificacao_MFractal)= Test$Nomes
  Classificacao_MFractal[1,]=Test$Prob

  #Classificacao_MFractal <- sort(ResProb_MFractal, decreasing=TRUE)

  # Carteira Multifractal
  Class=colnames(Classificacao_MFractal)
  Carteira_MFractal = Class[1:n_assets]
  C_MFractal = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% Carteira_MFractal)))
  C_MFractal = C_MFractal[Datas1Predict,]
  Pesos_MFractal = c(rep(1/n_assets,n_assets))
  Ret_C_MFractal_EQ = as.matrix(C_MFractal) %*% Pesos_MFractal

  # Weight extract
  C_Pesos_MFractal = data.frame(colnames(C_MFractal),Pesos_MFractal)
  Pesos_MFractal_1 <- C_Pesos_MFractal[C_Pesos_MFractal[,2]>0,]
  Pesos_MFractal_2<- t(matrix(Pesos_MFractal_1[,2]))
  colnames(Pesos_MFractal_2) <- Pesos_MFractal_1[,1]
  rownames(Pesos_MFractal_2)<-'Weight'
  #print(paste('Weights of the MF-EQ Portfolio:'))
  #   print(Pesos_MFractal_2)

  # Carteira de Markovitz de Minima Variância M_Fractal
  Pesos_MFractal_Mkv <- round(tseries::portfolio.optim(as.matrix(C_MFractal))$pw, 4)
  Ret_C_MFractal = as.matrix(C_MFractal)%*% Pesos_MFractal_Mkv

  # Weight extract
  C_Pesos_MFractal_Mkv = data.frame(colnames(C_MFractal),Pesos_MFractal_Mkv)
  Pesos_MFractal_Mkv1 <- C_Pesos_MFractal_Mkv[C_Pesos_MFractal_Mkv[,2]>0,]
  Pesos_MFractal_Mkv2<- t(matrix(Pesos_MFractal_Mkv1[,2]))
  colnames(Pesos_MFractal_Mkv2) <- Pesos_MFractal_Mkv1[,1]
  rownames(Pesos_MFractal_Mkv2)<-'Weight'
  #print(paste('Weights of the MF-MKW Portfolio:'))
  #   print(Pesos_MFractal_Mkv2)




  ################################################################################
  # Comparativo com a Carteira Multi Fractal _ MF
  # Carteira de Buffet - 7 ativos - 80%
  scenario.set = data.frame(scenario.set)

  #Datas1Predict = rownames(scenario.set)[
  # (which(rownames(scenario.set)=="2020-01-20")):(nrow(scenario.set)-1)]

  ###################################################################################
  # Carteira de Markovitz de Minima Variância obtida a partir de todos ativos
  TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))
  #pesos_todosPredict <- round(tseries::portfolio.optim(all.returns)$pw, 4)
  library(quadprog)
  library(Matrix)

  assets= ncol(all.returns)
  H <- cov(all.returns)
  f <- rep(0, assets)
  Aeq <- rep(1, assets)
  beq <- 1
  lb <- rep(0, assets)
  ub <- rep(1, assets)

  soluction <- quadprog(H, f, NULL, NULL, Aeq, beq, lb, ub)
  pesos_todosPredict <- round(soluction$x, 4)
  pesos_todosPredict


  #ts=as.ts(all.returns[11:nrow(all.returns),])
  #pd_D_mat <- nearPD(ts)

  #output <- solve.QP(Dmat = as.matrix(ts),
  #                   dvec = d_vec,
  #                  Amat = ts,
  #                   bvec = 0,
  #                  meq  = 1)

  RetornoMedioMArkovitz = as.matrix(TodosAtivosPredict)%*% pesos_todosPredict
  MKW = as.matrix(all.returns)%*% pesos_todosPredict
  mean_MKW=mean(MKW)
  sd_MKW=sd(MKW)

  # Weight extract
  Carteira_Markov = data.frame(colnames(TodosAtivosPredict),pesos_todosPredict)
  Pesos_C_Markov <- Carteira_Markov[Carteira_Markov[,2]>0,]
  Pesos_C_Markov2 <- t(matrix(Pesos_C_Markov[,2]))
  colnames(Pesos_C_Markov2) <- Pesos_C_Markov[,1]
  rownames(Pesos_C_Markov2)<-'Weight'
  print(paste('[1] weights of the MARKOWITZ Portfolio:'))
  print(Pesos_C_Markov2)


  print(paste('[2] Weights of the MF_MKW Portfolio:'))
  print(Pesos_MFractal_Mkv2)

  CarteiraComparativa = colnames(Type_ANNt[1:n_assets])
  C_Net_T_comparativa = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% CarteiraComparativa)))
  C_Net_T_comparativa = C_Net_T_comparativa[Datas1Predict,]

  # Carteira de pesos iguais
  PesosComparativos = c(rep(1/n_assets,n_assets))
  Media_C_Net_T_Comparativa = as.matrix(C_Net_T_comparativa) %*% PesosComparativos

  # Weight extract
  C_Pesos_Eq_ANNt= data.frame(colnames(C_Net_T_comparativa),PesosComparativos)
  Pesos_ANNt_Eq1 <- C_Pesos_Eq_ANNt[C_Pesos_Eq_ANNt[,2]>0,]
  Pesos_ANNt_Eq2<- round(t(matrix(Pesos_ANNt_Eq1[,2])),4)
  colnames(Pesos_ANNt_Eq2) <- Pesos_ANNt_Eq1[,1]
  rownames(Pesos_ANNt_Eq2)<-'Weight'
  print(paste('[3] Weights of the ANNt_EQ Portfolio:'))
  print(Pesos_ANNt_Eq2)


  ###############################################################################
  # GMV - Global Minimum Variance
  if(nrow(TodosAtivosPredict)<ncol(TodosAtivosPredict)){
    #EPR=colMeans(all.returns)
    #COV=var(all.returns)
    #GMV=globalMin.portfolio(EPR,COV)
    GMV_Return = mean(RetornoMedioMArkovitz)
    GMV_sd = sd(RetornoMedioMArkovitz)
    weight_GMV = pesos_todosPredict
  }else{
    EPR=colMeans(TodosAtivosPredict)
    COV=var(TodosAtivosPredict)
    GMV=globalMin.portfolio(EPR,COV)
    GMV_Return = GMV$er
    GMV_sd = GMV$sd
    weight_GMV = GMV$weights}


  ##############################################################################


  # Carteira RNA NNet dist T com pesos de Markovitz  para Comparação
  pesos_MarkovitzNNet_T <- round(tseries::portfolio.optim(
    as.matrix(C_Net_T_comparativa))$pw, 4)
  Ret_Medio_RNA_T_Mkv = as.matrix(C_Net_T_comparativa) %*% pesos_MarkovitzNNet_T

  # Weight extract
  C_Pesos_MKV_ANNt= data.frame(colnames(C_Net_T_comparativa),pesos_MarkovitzNNet_T)
  Pesos_ANNt_Mkv1 <- C_Pesos_MKV_ANNt[C_Pesos_MKV_ANNt[,2]>0,]
  Pesos_ANNt_Mkv2<- t(matrix(Pesos_ANNt_Mkv1[,2]))
  colnames(Pesos_ANNt_Mkv2) <- Pesos_ANNt_Mkv1[,1]
  rownames(Pesos_ANNt_Mkv2)<-'Weight'
  print(paste('[4] Weights of the ANNt_MKW Portfolio:'))
  print(Pesos_ANNt_Mkv2)



  ################################cARTEIRAS SHARPE ###############################
  ### Carteira Sharpe todos os ativos
  ## Optmization
  symbols = colnames(TodosAtivosPredict)
  #####

  #init.portf <- portfolio.spec(assets = symbols)
  #init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
  #init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
  #init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
  #init.portf

  #init.portf <- add.constraint(portfolio = init.portf, type = "risk",
  #                             name = "StdDev", multiplier = 0)
  #port1 <- add.constraint(portfolio = init.portf,
  #                        type = "diversification", min=0, max=1,
  #                        indexnum=2)
  #port1 <- add.constraint(portfolio = init.portf, type = "risk", name = "StdDev")


  ### Carteira Sharpe todos os ativos
  #maxSRport.rp <- optimize.portfolio(R=TodosAtivosPredict,
  #                                  portfolio = port1,
  #                                   optimize_method = "random",
  #                                  search_size = 20000,
  #                                 maxSR=TRUE, trace = TRUE)
  #maxSRport.rp

  #maxSR.weight.rp <- extractWeights(maxSRport.rp)


  ##############################################################################
  ######## Sharpe Calculate
  ##############################################################################
  if(Final_Date_Testing=='2022-07-12'){
    all.returns <- all.returns[1:(nrow(all.returns)-60),]


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


    ### Carteira Sharpe todos os ativos
    maxSRport.rp <- optimize.portfolio(R=all.returns,
                                       portfolio = port1,
                                       optimize_method = "random",
                                       search_size = 20000,
                                       maxSR=TRUE, trace = TRUE)
    maxSRport.rp

    weight_test <- extractWeights(maxSRport.rp)



  } else{
    #######################Verify if ANNt_Oliveira_Ceretta_S was ativate###########
    if (file.exists('~/weight_test.rda')==TRUE){
      load('~/weight_test.rda')
      load('~/sd_sharpe.rda')
      load('~/weight_test.rda')
    } else{weight_test=NULL}

    if(Signal_Sharpe==0 | length(weight_test)!=ncol(all.returns)){

      n_testing=which(rownames(dados2)==Final_Date_Testing)-which(rownames(dados2)==Initial_Date_Testing)
      #if(n_testing<ncol(dados2)){
      #  stop("You need to specify a test period greater than the number of assets for the Sharpe portfolio to have a solution!")
      #}

      ####### set up portfolio with objetive and constraints
      #n.assets <- length(colnames(all.returns))
      #port.sec <- portfolio.spec(assets = colnames(all.returns))
      #port.sec <- add.objective(portfolio = port.sec, type = "risk", name = "StdDev")
      #port.sec <- add.objective(portfolio = port.sec, type = "return", name = "mean")
      #port.sec <- add.constraint(portfolio = port.sec, type = "full_investiment")
      #port.sec <- add.constraint(portfolio = port.sec, type = "box", min = 0, max = 1)


      # map off efficient frontier (for variance risk)
      #eff.frontier <- create.EfficientFrontier(R = all.returns, portfolio = port.sec,
      #                                         n.portfolio = 2000, type = "mean-StdDev")

      # Daily Sharpe ratio
      #rf=(1+Rf)^(1/252)-1
      #sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf)/eff.frontier$frontier[,"StdDev"]
      #max.sharpe.ratio <- sharpe.ratios[sharpe.ratios==max(sharpe.ratios)]
      #optimal.port.name <- names(max.sharpe.ratio)
      #optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
      #optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

      #n.trading.days.per.year <- 1

      #print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
      #print(sprintf("Optimal E(port return): %f", optimal.mean*sqrt(n.trading.days.per.year)))
      #mean_sharpe = optimal.mean*sqrt(n.trading.days.per.year)
      #print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
      #sd_sharpe <- optimal.sd*sqrt(n.trading.days.per.year)

      #print("Optimal weights")
      #weight_test <- eff.frontier$frontier[optimal.port.name,(1:n.assets)+3]


      ################# SHARPE manual construction ###############################
      ################# Envelope LOOP 5000 vezes #######################################

      pesosCarteira <- function(retornosAtivos, retornoAlvo) {
        ## Argumentos:
        # retornosAtivos - conjunto de dados dos retornos dos ativos
        # retornoAlvo - o retorno-alvo da carteira

        ##  A fun??o solve.QP() do pacote quadprog implementa o m?todo dual de Goldfarb e Idnani (1982, 1983)
        ##  para a solu??o do problema de otimiza??o quadr?tica na forma
        ##  min(-d'b + 1/2 b' Db) com as restri??es A'T b >= b0.

        ## Para detalhes, veja D. Goldfarb and A. Idnani (1983). "A numerically stable dual method for solving strictly convex #quadratic programs". Mathematical Programming, 27, 1-33.

        ## A solu??o aqui s?o os pesos que minimizam o risco para o retorno em 'retornoAlvo'

        if(!require("quadprog")) install.packages("quadprog")
        suppressMessages(suppressWarnings(library(quadprog)))

        nAtivos  <-  ncol(retornosAtivos)
        portfolio <- solve.QP(
          Dmat <- cov(retornosAtivos),                        # matriz D
          dvec <- rep(0, times = nAtivos),                    # vetor  d
          Amat <- t(rbind(retorno = colMeans(retornosAtivos), # matriz A de restri??es
                          orcamento = rep(1, nAtivos),
                          longa = diag(nAtivos))),
          bvec <- c(retorno = retornoAlvo,                    # vetor  b0
                    orcamento = 1,
                    longa = rep(0, times = nAtivos)),
          meq = 2)                                            # as primeiro meq restri??es s?o igualdades

        pesos  <-  portfolio$solution # vetor contendo a solu??o do problema
        pesos
      }

      fronteiraCarteira <- function(retornosAtivos, nPontos = 40) {
        # Quantidade de ativos
        nAtivos <- ncol(retornosAtivos)
        # Retornos-alvo
        mu <- colMeans(retornosAtivos)
        retornoAlvo <- seq(min(mu), max(mu), length = nPontos)
        # Pesos ?timos
        pesos <- rep(0, nAtivos)
        pesos[which.min(mu)] <- 1
        for (i in 2:(nPontos-1)) {
          novosPesos <- pesosCarteira(retornosAtivos, retornoAlvo[i])
          pesos <- rbind(pesos, novosPesos)
        }
        novosPesos <- rep(0, nAtivos)
        novosPesos[which.max(mu)] <- 1
        pesos <- rbind(pesos, novosPesos)
        pesos <- round(pesos, 4)
        colnames(pesos) <- colnames(retornosAtivos)
        rownames(pesos) <- 1:nPontos

        # Valor do retorno
        pesos
      }

      retornosAtivos = all.returns
      pesos_front <- fronteiraCarteira(retornosAtivos, nPontos=500)
      Medias_set.returns <- as.matrix(t(apply(all.returns, 2, mean)))
      mu = Medias_set.returns
      retornoAlvos  <-  seq(min(mu), max(mu), length = nrow(pesos_front))

      riscosAlvo  <-  NULL
      for (i in 1:nrow(pesos_front)) {
        novoRiscoAlvo  <-  sqrt(pesos_front[i, ] %*%
                                  cov(retornosAtivos) %*%
                                  pesos_front[i, ])
        riscosAlvo  <-  c(riscosAlvo, novoRiscoAlvo)
      }

      rf=(1+Rf)^(1/252)-1
      S_=tan((retornoAlvos-rf)/riscosAlvo)

      fronteiraEficiente <- data.frame(risco=riscosAlvo, retorno=retornoAlvos, Sharpe = S_)
      sHARPEMAX = which(fronteiraEficiente$Sharpe==max(fronteiraEficiente$Sharpe))

      mean_sharpe=fronteiraEficiente$retorno[sHARPEMAX]
      sd_sharpe=fronteiraEficiente$risco[sHARPEMAX]
      weight_test = pesos_front[sHARPEMAX,]

    }else{
      load('~/mean_sharpe.rda')
      load('~/sd_sharpe.rda')
      load('~/weight_test.rda')
    }

  }
  #########################################

  weight_test <- round(weight_test,4)
  weight_Sharpe= weight_test[which(weight_test !=0)]
  weight_Sharpe

  # Weight extract
  Weight_Sharpe_1 <- t(as.data.frame(weight_Sharpe))
  colnames(Weight_Sharpe_1)<-str_replace(colnames(Weight_Sharpe_1),'w.','')
  rownames(Weight_Sharpe_1)<-'Weight'

  print(paste('[5] Weights of the SHARPE Portfolio:'))
  print(Weight_Sharpe_1)
  #weight
  ### Retornos carteira Sharpe todos os ativos
  #RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% maxSR.weight.rp
  RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% weight_test

  ##############################################################################
  ### Carteira Sharpe MF_DFA
  ##############################################################################
  ## Optmization
  #symbols_MFractal = colnames(C_MFractal)
  #init.portf.MF <- portfolio.spec(assets = symbols_MFractal)
  #init.portf.MF <- add.constraint(portfolio = init.portf.MF, type = "full_investment")
  #init.portf.MF <- add.constraint(portfolio = init.portf.MF, type = "long_only")
  #init.portf.MF <- add.objective(portfolio = init.portf.MF, type = "return", name = "mean")
  #init.portf.MF

  #init.portf.MF <- add.constraint(portfolio = init.portf.MF, type = "risk",
  #                               name = "StdDev", multiplier = 0)
  #port1.MF <- add.constraint(portfolio = init.portf.MF,
  #                         type = "diversification", min=0, max=1,
  #                        indexnum=2)
  #port1.MF <- add.constraint(portfolio = init.portf.MF, type = "risk", name = "StdDev")


  #maxSRport.rp.MFractal <- optimize.portfolio(R=C_MFractal,
  #                                           portfolio = port1.MF,
  #                                          optimize_method = "random",
  #                                         search_size = 2000,
  #                                        maxSR=TRUE, trace = TRUE)
  #maxSRport.rp.MFractal

  #maxSR.weight.rp.MFractal <- extractWeights(maxSRport.rp.MFractal)
  #maxSR.weight.rp.MFractal

  #RetornoMedioMaxIS_MFractal = as.matrix(C_MFractal)%*% maxSR.weight.rp.MFractal

  ##################################################################
  all.returns.MF <- as.matrix(C_MFractal)
  ## set up portfolio with objetive and constraints
  n.assets.MF <- length(colnames(all.returns.MF))
  port.sec.MF <- portfolio.spec(assets = colnames(all.returns.MF))
  port.sec.MF <- add.objective(portfolio = port.sec.MF, type = "risk", name = "StdDev")
  port.sec.MF <- add.objective(portfolio = port.sec.MF, type = "return", name = "mean")
  port.sec.MF <- add.constraint(portfolio = port.sec.MF, type = "full_investiment")
  port.sec.MF <- add.constraint(portfolio = port.sec.MF, type = "box", min = 0, max = 1)

  # map off efficient frontier (for variance risk)
  eff.frontier.MF <- create.EfficientFrontier(R = all.returns.MF, portfolio = port.sec.MF,
                                              n.portfolio = 2000, type = "mean-StdDev")

  # Daily Sharpe ratio

  sharpe.ratios.MF <- (eff.frontier.MF$frontier[,"mean"]-rf)/eff.frontier.MF$frontier[,"StdDev"]
  max.sharpe.ratio.MF <- sharpe.ratios.MF[sharpe.ratios.MF==max(sharpe.ratios.MF)]
  optimal.port.name.MF <- names(max.sharpe.ratio.MF)
  optimal.mean.MF <- eff.frontier.MF$frontier[optimal.port.name.MF,"mean"]
  optimal.sd.MF <- eff.frontier.MF$frontier[optimal.port.name.MF,"StdDev"]

  n.trading.days.per.year.MF <- 1

  #print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio.MF*sqrt(n.trading.days.per.year.MF)))
  #print(sprintf("Optimal E(port return): %f", optimal.mean.MF*sqrt(n.trading.days.per.year.MF)))
  mean_sharpe.MF = optimal.mean.MF*sqrt(n.trading.days.per.year.MF)
  #print(sprintf("Optimal sd(port return): %f", optimal.sd.MF*sqrt(n.trading.days.per.year.MF)))
  sd_sharpe.MF <- optimal.sd.MF*sqrt(n.trading.days.per.year.MF)

  #print("Optimal weights")
  weight_test_MF <- eff.frontier.MF$frontier[optimal.port.name.MF,(1:n.assets.MF)+3]
  weight_test_MF <- round(weight_test_MF,4)
  weight_Sharpe_MF= weight_test_MF[which(weight_test_MF !=0)]

  # Weight extract
  Weight_Sharpe_MF <- t(as.data.frame(weight_Sharpe_MF))
  colnames(Weight_Sharpe_MF)<-str_replace(colnames(Weight_Sharpe_MF),'w.','')
  rownames(Weight_Sharpe_MF)<-'Weight'

  print(paste('[6] Weights of the MF_SHARPE Portfolio:'))
  print(Weight_Sharpe_MF)

  ### Retornos carteira Sharpe MF_DFA Multifractal
  RetornoMedioMaxIS_MFractal = as.matrix(C_MFractal)%*% weight_test_MF

  ################################################################################
  ### Carteira Sharpe RNAt
  ##############################################################################
  ## Optmization
  #symbols_RNAt = colnames(C_Net_T_comparativa)

  #init.portf <- portfolio.spec(assets = symbols_RNAt)
  #init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
  #init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
  #init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
  #init.portf

  #init.portf <- add.constraint(portfolio = init.portf, type = "risk",
  #                            name = "StdDev", multiplier = 0)
  #port1 <- add.constraint(portfolio = init.portf,
  #                       type = "diversification", min=0, max=1,
  #                      indexnum=2)
  #port1 <- add.constraint(portfolio = init.portf, type = "risk", name = "StdDev")



  #maxSRport.rp.RNAt<- optimize.portfolio(R=C_Net_T_comparativa,
  #                                      portfolio = port1,
  #                                     optimize_method = "random",
  #                                    search_size = 2000,
  #                                   maxSR=TRUE, trace = TRUE)
  #maxSRport.rp.RNAt

  #maxSR.weight.rp.RNAt <- extractWeights(maxSRport.rp.RNAt)

  #RetornoMedioMaxIS_RNAt = as.matrix(C_Net_T_comparativa)%*% maxSR.weight.rp.RNAt

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
  rf=(1+Rf)^(1/252)-1
  sharpe.ratios_RNA_t <- (eff.frontier_RNA_t$frontier[,"mean"]-rf)/eff.frontier_RNA_t$frontier[,"StdDev"]
  max.sharpe.ratio_RNA_t <- sharpe.ratios_RNA_t[sharpe.ratios_RNA_t==max(sharpe.ratios_RNA_t)]
  optimal.port.name.RNAt <- names(max.sharpe.ratio_RNA_t)
  optimal.mean.RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,"mean"]
  optimal.sd.RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,"StdDev"]

  n.trading.days.per.year.RNAt <- 1

  #print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio_RNA_t*sqrt(n.trading.days.per.year.RNAt)))
  #print(sprintf("Optimal E(port return): %f", optimal.mean.RNAt*sqrt(n.trading.days.per.year.RNAt)))
  mean_sharpe_RNA_t = optimal.mean.RNAt*sqrt(n.trading.days.per.year.RNAt)
  #print(sprintf("Optimal sd(port return): %f", optimal.sd.RNAt*sqrt(n.trading.days.per.year.RNAt)))
  sd_sharpe_RNA_t <- optimal.sd.RNAt*sqrt(n.trading.days.per.year.RNAt)
  #print("Optimal weights")
  weight_test_RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,(1:n.assets.RNAt)+3]
  weight_test_RNAt <- round(weight_test_RNAt,4)
  weight_Sharpe_RNA_t= weight_test_RNAt[which(weight_test_RNAt !=0)]
  weight_Sharpe_RNA_t

  # Weight extract
  Weight_ANNt_Sharpe <- t(as.data.frame(weight_Sharpe_RNA_t))
  colnames(Weight_ANNt_Sharpe)<-str_replace(colnames(Weight_ANNt_Sharpe),'w.','')
  rownames(Weight_ANNt_Sharpe)<-'Weight'
  print(paste('[7] Weights of the ANNt_SHARPE Portfolio:'))
  print(Weight_ANNt_Sharpe)

  RetornoMedioMaxIS_RNAt = as.matrix(C_Net_T_comparativa)%*% weight_test_RNAt


  #####
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
  save(Rf,file='~/Rf.rda')

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
  Weights_All [10,1] <- 'ANNt_MKW'
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
  Rf=Rf*100
  save(mean_sharpe,file="~/mean_sharpe.rda")
  save(sd_sharpe,file="~/sd_sharpe.rda")
  save(weight_test,file="~/weight_test.rda")
  save(Initial_Date_Testing,file='~/Initial_Date_Testing.rda')
  save(Classificacao_MFractal, file='~/Classificacao_MFractal.rda')
  save(Rf,file='~/Rf.rda')
  save(pesos_todosPredict,file='~/pesos_todosPredict.rda')
  save(weight_Sharpe,file='~/weight_Sharpe.rda')
  save(Weights_All,file='~/Weights_All.rda')
  save(N_Assets,file='~/N_Assets.rda')
  save(Final_Date_Testing,file='~/Final_Date_Testing.rda')
  save(GMV_Return,file='~/GMV_Return.rda')
  save(GMV_sd,file='~/GMV_sd.rda')

  #### Weights
  save(Pesos_MFractal_2,file='~/Pesos_MFractal_2.rda')
  save(Pesos_MFractal_Mkv,file='~/Pesos_MFractal_Mkv.rda')
  save(Pesos_MFractal_Mkv2,file='~/Pesos_MFractal_Mkv2.rda')
  save(Pesos_C_Markov2,file='~/Pesos_C_Markov2.rda')
  save(Pesos_ANNt_Eq2,file='~/Pesos_ANNt_Eq2.rda')
  save(Pesos_ANNt_Mkv2,file='~/Pesos_ANNt_Mkv2.rda')
  save(Weight_Sharpe_1,file='~/Weight_Sharpe_1.rda')
  save(Weight_Sharpe_MF,file='~/Weight_Sharpe_MF.rda')
  save(Weight_ANNt_Sharpe,file='~/Weight_ANNt_Sharpe.rda')
  save(sd_MKW, file='~/sd_MKW.rda')
  save(mean_MKW, file='~/mean_MKW.rda')

  write_xlsx(as.data.frame(Weights_All), "~/Weights_All.xlsx")

  View(Weights_All)
  View(Comparativo_RETORNOS)

}
