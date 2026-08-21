#' ANNt_order classify assets by the Probability of return exceeding a RM
#' Use Artificial Neural Networks (ANN) and t-distribution, the number of ANN is the number of import assets


#' @param Initial_Date_Training Series start Date (Must be 7 periods greater than the analyzed series)
#' @param Final_Date_Training Series finish training date
#' @param Final_Date_Testing Series end Date (If '' is the System Date)
#' @param N_Lags Number of lags used in the input layer
#' @param Hidden Number of hidden neurons and hidden layers (If '', the neurons' number is the length series)
#' @param Stepmax Number of replications per asset to train the ANN
#' @param Loss Function: "MSE" for Mean Square Error, "MAE" for Mean Absolute Error,
#' "MADL" for Mean Absolute Directional Loss, and "GMADL" for Generalized Mean Absolute Directional Loss
#' @param Early_Stopping = 'No' or 'Yes'. Default is 'No'. If 'Yes' is necessary inform the value
#' @param Learning_Rate is the Artificial Neural Network learning rate
#' @param Decay L2 regularization or weight decay, add a penalty term to the loss function. "Yes" or "No.
#' No" is default. If  "Yes" is necessary inform the lambda or rate of regularization
#' @param Asymmetry "Negative" or "Positive". Shifts the Probability of the return being greater than the proxy to the right or left, "Negative" or "Positive". Default is to the right, "Negative"
#' @param Skew_t Incorporate skew parameter in the Probability: "Yes" or "No". Default is "No".
#' Second instruction: If central parameter is "Median" or "xi" for location parameter (ξ). Default is "Median".
#' Third instruction: Number of deviation consider in matrix Probability. Default is 1.
#' @param Prediction Model of prediction: "Predict" or "Forecast". "Predict" is the standard.
#' @param Bias include Bias, Yes or No, with auto learning
#' @param Order_Only disability the ANN and only order the historic Probability to outperformed the benchmark
#' @param Convolution addresses the bearish/bullish tendency or inverse tendency in the neural input (Trend, Neutral, Reverse)
#' @param Initialization define the parameters of weights initialization:
#' 1) Original: Define the first configuration. Uniform distribution and weights from 0 to 1;
#' 2) Xavier_N: Define the Normal distribution with variance of Xavier;
#' 3) Xavier_U: Define the Uniform distribution with limit's Xavier;
#' 4) Xavier_H: Define HeLu limit's Xavier;
#' 5) Xavier_Leaky: Define Leaky HeLU limit's Xavier
#' 6) Normal_D: Define the Normal distribution with mean=0 and Variance=1;
#' 7) Uniform_D: Define the Uniform distribution with minimum=-0.5 and maximum=0.5 for stability.
#'
#' @param Activation_Function Define the Activation Function
#' 1) Tangent: Define the Tangent Function Activation
#' 2) Sigmoid: Define the Sigmoid Function Activation
#' 3) Leaky_ReLU: Define the Leaky ReLU Function Activation
#'
#' @param Activation_F_Out Define the Activation Function:
#' 1) Original: Define the Tangent function activation and Sigmoid Derivate Backpropagation
#' 2) Linear: Define the linear function activation in the output layer
#' 3) Activation: Define the Activation function in the output layer specified in the hidden layers
#' @param Batch_Size Define the length of Batch. Standard is full batch size (Batch gradient descent)
#' @author Alexandre Silva de Oliveira

#' @examples
#' ANNt_order (
#' Initial_Date_Training = c('2018-01-11'),
#' Final_Date_Training = c('2022-12-29'),
#' Final_Date_Testing = c(''),
#' N_Lags=5,
#' Hidden = 7,
#' Stepmax = 300,
#' Loss = "MSE",
#' Learning_Rate = 0.001,
#' Decay=c('Yes',0.001),
#' Early_Stopping = c('Yes', 0.001),
#' Asymmetry='Negative',
#' Skew_t=c('Yes','Median',1),
#' Bias="No",
#' Order_Only='No',
#' Convolution = 'Neutral',
#' Initialization = 'Original',
#' Activation_Function='Tangent',
#' Activation_F_Out='Original',
#' Batch_Size=''
#' )

#' # Estimated processing time 2 hours.
#'
#Portfolio optimization system using Artificial Neural Networks.
#ANNt uses a Perceptron architecture, with a hidden layer of size equal to the
#number of information in the input neurons.     Classifies the output neurons
#of assets with leptokurtic distribution, which is more appropriate for financial
#data.

#' @export
ANNt_order <- function(Initial_Date_Training, Final_Date_Training, Final_Date_Testing,
                       N_Lags=5,
                       Hidden, Stepmax, Loss="MSE", Learning_Rate=0.3, Decay='No',
                       Early_Stopping = 'No', Asymmetry='Negative', Skew_t='No',
                       Prediction='Predict', Bias="No", Order_Only='No',
                       Convolution='Neutral', Initialization = 'Original',
                       Activation_Function='Tangent', Activation_F_Out='Original',
                       Batch_Size='') {
  ## Convers?o das variaveis
  # Excesso do retorno em relacao ao RM

  save(N_Lags, file='~/N_Lags.rda')
  save(Loss, file='~/Loss.rda')
  save(Learning_Rate, file='~/Learning_Rate.rda')
  save(Decay, file='~/Decay.rda')
  save(Early_Stopping, file='~/Early_Stopping.rda')
  save(Asymmetry, file='~/Asymmetry.rda')
  save(Type_ANNt, file='~/Type_ANNt.rda')
  save(N_Assets, file='~/N_Assets.rda')
  save(Order, file='~/Order.rda')
  save(Skew_t, file='~/Skew_t.rda')
  save(Bias, file='~/Bias.rda')
  save(Order_Only, file='~/Order_Only.rda')
  save(Convolution, file='~/Convolution.rda')
  save(Initialization, file='~/Initialization.rda')
  save(Activation_Function, file='~/Activation_Function.rda')
  save(Activation_F_Out, file='~/Activation_F_Out.rda')
  save(Batch_Size, file='~/Batch_Size.rda')



  if ((length(Skew_t)==1) & (Skew_t[1]=='Yes')){
    Skew_t=c('Yes',"Median",1)
  }

  library("quantmod")
  if (!require("sn", character.only = TRUE)) {
    install.packages("sn", dependencies = TRUE)
  }
  library("sn", character.only = TRUE)

  if (!require("stats", character.only = TRUE)) {
    install.packages("stats", dependencies = TRUE)
  }
  library("stats", character.only = TRUE)

  if (!require("nortest", character.only = TRUE)) {
    install.packages("nortest", dependencies = TRUE)
  }
  library("nortest", character.only = TRUE)
  if (!require("forecast", character.only = TRUE)) {
    install.packages("forecast", dependencies = TRUE)
  }
  library("forecast", character.only = TRUE)

  print('Starting ANNt_order Command')

  load("~/scenario.set.rda") # Carrega objeto scenario.set
  load("~/Initial_Date.rda") # Carrega objeto scenario.set
  #load("~/tickers.rda") # Carrega objeto scenario.set
  #load("~/Signal_Sharpe.rda") # Carrega objeto scenario.set
  ativos_fora=NULL
  tickers=colnames(scenario.set)

  dados<-scenario.set
  # if(Signal_Sharpe==1){
  #   load('~/x1_.rda')
  #   Initial_Date_Training=x1_
  # }
  if(Final_Date_Training==''){
    load('~/x1.rda')
    Final_Date_Training=x1
  }
  if(Initial_Date_Training==('')){
    Initial_Date_Training=rownames(as.data.frame(scenario.set)[6,])
  }
  if(length(which(rownames(as.data.frame(scenario.set))==Initial_Date_Training))==0){
    while(length(which(rownames(as.data.frame(scenario.set))==Initial_Date_Training))==0){
      dia=as.Date(Initial_Date_Training)
      new_day=dia+1
      Initial_Date_Training = as.character(new_day)
    }
  }

  if(length(which(rownames(as.data.frame(scenario.set))==Final_Date_Training))==0){
    while(length(which(rownames(as.data.frame(scenario.set))==Final_Date_Training))==0){
      dia=as.Date(Final_Date_Training)
      new_day=dia-1
      Final_Date_Training = as.character(new_day)
    }
  }

  if(Final_Date_Testing==('')){
    Final_Date_Testing=Sys.Date()
  }

  if(length(which(rownames(as.data.frame(scenario.set))==Final_Date_Testing))==0){
    while(length(which(rownames(as.data.frame(scenario.set))==Final_Date_Testing))==0){
      dia=as.Date(Final_Date_Testing)
      new_day=dia-1
      Final_Date_Testing = as.character(new_day)
    }
  }
 print(paste('Initial_Date_Training applied: ', Initial_Date_Training))
 print(paste('Final_Date_Training applied: ', Final_Date_Training))
 print(paste('Final_Date_Testing applied: ', Final_Date_Testing))
  # y1 is the number of hidden, case the ANNt_Oliveira_Ceretta went used
  if(Hidden[1]=='hidden'){
    load('~/x2.rda')
    Hidden=x2
  }
  # y2 is the number of Stepmax, case the ANNt_Oliveira_Ceretta went use
  if(Stepmax=='stepmax'){
    load('~/x3.rda')
    Stepmax=x3
  }

  if (Hidden[1]==''){
    Cont1=which(rownames(scenario.set)==Final_Date_Training)-5
  } else{
    Cont1=Hidden[1]
  }

  if (Asymmetry=='asymmetry'){
    load('~/X11.rda')
    Asymmetry=X11
  }

  # Duração do processamento 285.4/length(dados=0.2 horas)

  Fator_Tempo = (12000/nrow(dados))*(Cont1/(nrow(dados)-5))*Stepmax/2500
  Unidade=' minute(s)'
  Tempo= round(Fator_Tempo*(ncol(dados)-1),2)
  if (Tempo>120){
    Unidade=' hour(s)'
    Tempo=round(Tempo/60,2)
    Fator_Tempo=Fator_Tempo/60
  }
  dados2=data.frame(dados)
  cat(paste("
           Estimated total processing time: ", Tempo, Unidade,"
___________________________________________________________________
           Starting ANNt 1 of a total of ",ncol(dados)-1, " assets: ",colnames(dados2[2]), ".
", sep=""))

  ncoldados <- ncol(dados)
  nAtivos = ncol(dados)
  for (i in 2:ncoldados) {
    dados[,i] <- dados[,i] - dados[,1]
  }

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Train
  #### NeuralNet por Sinal
  ResProbPosNNet = matrix(ncol=nAtivos-1,nrow=1)
  ResProbPosNNet = data.frame(ResProbPosNNet)
  colnames(ResProbPosNNet)=tickers[-1]
  ResProbPosNNet

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Train
  #### NeuralNet por Probabilidade t de Student
  ResProbTPosNNet = matrix(ncol=nAtivos-1,nrow=1)
  ResProbTPosNNet = data.frame(ResProbTPosNNet)
  colnames(ResProbTPosNNet)=tickers[-1]
  ResProbTPosNNet

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Train
  #### Particular por Sinal
  ResProbPos = matrix(ncol=nAtivos-1,nrow=1)
  ResProbPos = data.frame(ResProbPos)
  colnames(ResProbPos)=tickers[-1]
  ResProbPos

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Train
  #### Particular por Probabilidade t de Student
  ResProbTPos = matrix(ncol=nAtivos-1,nrow=1)
  ResProbTPos = data.frame(ResProbTPos)
  colnames(ResProbTPos)=tickers[-1]
  ResProbTPos

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Sinal
  ResProbPosNNetPredict = matrix(ncol=nAtivos-1,nrow=1)
  ResProbPosNNetPredict = data.frame(ResProbPosNNetPredict)
  colnames(ResProbPosNNetPredict)=tickers[-1]
  ResProbPosNNetPredict

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Probabilidade t de Student
  ResProbTPosNNetPredict = matrix(ncol=nAtivos-1,nrow=1)
  ResProbTPosNNetPredict = data.frame(ResProbTPosNNetPredict)
  colnames(ResProbTPosNNetPredict)=tickers[-1]
  ResProbTPosNNetPredict

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Sinal
  ResProbPosPredict = matrix(ncol=nAtivos-1,nrow=1)
  ResProbPosPredict = data.frame(ResProbPosPredict)
  colnames(ResProbPosPredict)=tickers[-1]
  ResProbPosPredict

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Probabilidade t de Student
  ResProbTPosPredict = matrix(ncol=nAtivos-1,nrow=1)
  ResProbTPosPredict = data.frame(ResProbTPosPredict)
  colnames(ResProbTPosPredict)=tickers[-1]
  ResProbTPosPredict

  ########################
  #### Cria??o da vari?vel de armazenamento dos resultados de assimetria e curtose
  #### Particular por Probabilidade t de Student
  Resultados_Assim_Curtose_Training = matrix(ncol=nAtivos-1,nrow=18)
  Resultados_Assim_Curtose_Training = data.frame(Resultados_Assim_Curtose_Training)
  colnames(Resultados_Assim_Curtose_Training)=tickers[-1]
  rownames(Resultados_Assim_Curtose_Training) = c('Probability','Mean','Median','Stand. Dev.',
                                                  'Kurtosis','Skewness','Minimum','Maximum',
                                                  'xi', 'omega', 'alpha', 'nu', 'KS','AD',
                                                  'Dev_Left', 'Dev_Right','Prob_Left','Prob_Right')
  Resultados_Assim_Curtose_Testing = matrix(ncol=nAtivos-1,nrow=18)
  Resultados_Assim_Curtose_Testing = data.frame(Resultados_Assim_Curtose_Testing)
  colnames(Resultados_Assim_Curtose_Testing)=tickers[-1]
  rownames(Resultados_Assim_Curtose_Testing) = c('Probability','Mean','Median','Stand. Dev.',
                                                 'Kurtosis','Skewness','Minimum','Maximum',
                                                 'xi', 'omega', 'alpha', 'nu', 'KS','AD',
                                                 'Dev_Left', 'Dev_Right','Prob_Left','Prob_Right')
  Resultados_Assim_Curtose_Testing

  ################################################################################
  #####################################Envelope###################################
  ################################################################################

  for (k in 1:(nAtivos-1)){
    ativo = k+1
    #Envelope

    xi=1.0
    omega=0.0
    alpha=0.0
    nu=0.0
    ProbabilidadeTmedia=0.0
    Dev_Left=0.0
    Dev_Right=0.0
    Prob_Left=0.0
    Prob_Right=0.0
    ProbabilidadeTmedia_1=0.0
    Dev_Left_1=0.0
    Dev_Right_1=0.0
    Prob_Left_1=0.0
    Prob_Right_1=0.0
    xi_1=0.0
    omega_1=0.0
    alpha_1=0.0
    nu_1=0.0
    KS_test_1 = 0.0
    KS_pvalue_1=0.0
    AD_test_1 = 0.0
    AD_pvalue_1=0.0
    # Calculo das defasagens para cada ativo


    dat_r <- data.frame(dados2[ativo], Lag(dados[,1],1))
    colnames(dat_r)[2]="RM"


    defasagem = N_Lags
    for (i in 1:defasagem){
      dat_r[i+2] = Lag(dat_r[1],i)
    }
    ### removendo NAs
    #dat_r = na.fill(dat_r, "extend")

    dat_r = na.omit(dat_r)
    nlinhas = nrow(dat_r)
    dat_r <- as.matrix(dat_r)
    rownames(dat_r)=rownames(dados2[(defasagem+1):nrow(dados2),])
    #View(dat_r)
    ####################### Amostra de Tratamento################################
    # Criando as vari?veis como vetor para treinamento - com datas espec?ficas
    Inicio_data = as.Date(Initial_Date_Training)
    if(length(which(rownames(as.data.frame(dat_r))==Inicio_data))==0){
      while(length(which(rownames(as.data.frame(dat_r))==Inicio_data))==0){
        dia=as.Date(Inicio_data)
        new_day=dia+1
        Inicio_data = as.character(new_day)
      }
    }

    Fim_data = as.Date(Final_Date_Training)

    I_data = which(rownames(dat_r)==Inicio_data)
    F_data = which(rownames(dat_r)==Fim_data)
    entradas = as.matrix(dat_r[I_data:F_data,])
    if(Convolution=='Trend'){
      comprimento=(ncol(entradas)-1)/2
      for (i in 1:nrow(entradas)){
        contagem=sum(entradas[i,2:ncol(entradas)]<0, na.rm=TRUE)
        if(contagem>comprimento){
          entradas[i,2:ncol(entradas)]=ifelse(entradas[i,2:ncol(entradas)]>0,0,entradas[i,2:ncol(entradas)])
        } else{
          if(contagem<comprimento){
            entradas[i,2:ncol(entradas)]=ifelse(entradas[i,2:ncol(entradas)]<0,0,entradas[i,2:ncol(entradas)])
          }
        }
      }
    }
    if(Convolution=='Reverse'){
      comprimento=(ncol(entradas)-1)/2
      for (i in 1:nrow(entradas)){
        contagem=sum(entradas[i,2:ncol(entradas)]<0, na.rm=TRUE)
        if(contagem>comprimento){
          entradas[i,2:ncol(entradas)]=ifelse(entradas[i,2:ncol(entradas)]<0,0,entradas[i,2:ncol(entradas)])
        }else{
          if(contagem<comprimento){
            entradas[i,2:ncol(entradas)]=ifelse(entradas[i,2:ncol(entradas)]>0,0,entradas[i,2:ncol(entradas)])
          }
        }
      }
    }
    saidas = as.matrix(dat_r[(I_data+1):(F_data+1),1])


    #####
    nlinhas <- nrow(entradas)
    ncolunas <- ncol(entradas)

    #############################################################################
    ##Pacote neuralnet()
    library("quantmod")
    library("PerformanceAnalytics")
    library("magrittr")
    library("fBasics")
    library("tidyverse")
    library("stringr")
    library("dplyr")
    library("neuralnet")
    library("zoo")
    library("forecast")
    library("timetk")
    library("moments")
    library("data.table")
    library("ggplot2")
    library("rvest")
    library("caret")
    library("readxl")
    library("writexl")
    library("portfolio.optimization")
    library("PortfolioAnalytics")
    library("ROI")
    library("fPortfolio")
    library("timeSeries")
    library("gridExtra")
    library("cowplot")
    library("portfolioBacktest")
    library("CVXR")
    library("MFDFA")
    library("DEoptim")
    library("rvest")
    bias_saida=0
    bias_hidden=0
    for (N_Hidden in (1:length(Hidden))){
      if (Hidden[N_Hidden]==''){
        Hidden[N_Hidden]=nlinhas
      }
    }
    Hidden=as.numeric(Hidden)
    epocas = 10*Stepmax
    # Fun??o Sigmoide
    sigmoide = function(soma) {
      #return (1/ (1+exp(-soma)))
      #Fun??o Tangente Hiperb?lica
      #return (1-tanh(soma))
      return (tanh(soma))
    }
    colnames(entradas)[1]= "ATIVO"
    selecao_1=paste("V", 2+1:N_Lags, sep='')
    selecao_2=paste(selecao_1,collapse='+')
    formula=paste('ATIVO~RM',selecao_2,sep='+')
    Stop2=0.1
    if(Early_Stopping[1]=='Yes'){
      Stop2=as.numeric(Early_Stopping[2])
      nn= neuralnet( formula, data=entradas,
                     hidden = Hidden[1], act.fct = "tanh",
                     threshold = 0.05,
                     stepmax=epocas)
    }else{
      nn= neuralnet( formula, data=entradas,
                     hidden = Hidden[1], act.fct = "tanh",
                     threshold = 0.05,
                     stepmax=epocas)
    }


    # Plotagem da RNA
    #nn=as.matrix(sapply(nn, as.numeric))
    if(Hidden[1] %% 2 == 0) {
      escondida = Hidden
    #} else {if(Hidden[1] >15){
    #  Hidden_2=Hidden
    #  Hidden_2[1]=Hidden[1]+1
    #  escondida =Hidden_2
    } else {escondida = Hidden}
    nnplot= neuralnet( formula, data=entradas,
                       hidden = escondida, act.fct = "tanh", threshold = 0.05,
                       stepmax=epocas)


    colnames(entradas)[1]=colnames(dados2[ativo])


    arquivo = colnames(entradas)[1]
    #arquivo = paste(arquivo,"_N.pdf", sep="")
    #str_sub(arquivo, -1) <-"_RNA.pdf"
    #ggsave(arquivo,p1)
    #pdf(file=arquivo, height = 8, width = 9)
    #pdf(file=arquivo)

    if (ativo==ncol(dados)){

      ydev=dev.list()
      if(class(ydev)!="NULL"){
        dev.off()
      }
      #dev.capabilities()
      #op <- par(new = TRUE)
      #windowsFonts(A=windowsFont("Times New Roman"))
      #par(family="A")
      #png(file="~/ANN.png", width=1920, height=1200, res=296)
      #png(file="~/ANN.png")
      #ANN= plot(nnplot,main ="Artificial Neural Network")
      #save(ANN,file="~/ANN.png")
      #op <- par(new = TRUE)
      #windowsFonts(A=windowsFont("Times New Roman"))
      #par(family="A")
      if (Hidden[1]>15){
        plot(nnplot, main ="Artificial Neural Network",show.weights = FALSE)
      } else{
        plot(nnplot, main ="Artificial Neural Network")
      }
    }
    #dev.off()
    #p1 <- plot(nnplot)
    #p1
    #arquivo <- str_replace(arquivo,"_N","")
    arquivo = paste(arquivo,".pdf", sep="")
    pdf(file=arquivo)
    #dev.print(pdf, file = arquivo)
    #ARQUIVO <- str_replace(arquivo,".pdf",".png")
    #ggsave(ARQUIVO,p1)


    ## Previsao
    if(Prediction=='Predict'){
      prev = predict(nn, entradas)
    } else {
      prev = forecast(nn, h=length(entradas))
    }
    if ((Order_Only='Yes')==TRUE){
      prev=entradas
    }

    KS_test = ks.test(prev,'pnorm')
    KS_pvalue=KS_test$p.value
    AD_test = ad.test(prev)
    AD_pvalue=AD_test$p.value

    nome = colnames(entradas)[1]
    plot(as.vector(entradas[,1]), type="l", col= "red",
         main = paste("Retornos Amostra de Tratamento - Ativo", xnames = nome))
    legend("topright", legend = c("RNNet", nome), pch = 19,
           col = c("black", "red"))
    lines(prev)

    hist(prev, main = paste("Histograma Fase de Treinamento RNNet Ativo",
                            xnames= nome),
         xlab = paste("Retorno Excedente sobre", xnames = "RM"))
    mean(prev)

    Testes = compute(nn, entradas)
    Testes$net.result
    #png(ARQUIVO)
    #arquivo = colnames(entradas)[1]
    #arquivo = paste(arquivo,"HistTrain.pdf", sep="_")
    #ggsave(arquivo,p1)
    #pdf(file=arquivo, height = 8, width = 9)
    hist(Testes$net.result, main = paste("Histograma Previs?es RNA Ativo",
                                         xnames= nome), xlab = paste("Retorno Excedente sobre",
                                                                     xnames = "RM"))
    #dev.off()
    #arquivo <- str_replace(arquivo,"pdf","png")
    #png(filename=arquivo)
    hist(Testes$net.result, main = paste("Histograma Train RNA Ativo",
                                         xnames= nome), xlab = paste("Retorno Excedente sobre",
                                                                     xnames = "RM"))
    #dev.off()
    ## Avaliando a acur?cia (ME, RMSE, MAE, MPE e MAPE)
    dat_r2 = na.fill(entradas, "extend")
    prev2 = na.fill(entradas, "extend")
    accuracy(as.vector(prev2),entradas[1])

    sinal = NULL
    nlinhas = nrow(entradas)
    for (i in 1:nlinhas) {
      if(prev[i] > 0){
        sinal[i] = 1
      }
      else {
        sinal[i] = -1
      }
    }
    sinal
    ### Probabilidades
    RetornoPos <- sum(sinal ==1)
    RetornoNeg <- sum(sinal == -1)
    ProbPos <- RetornoPos/(RetornoPos + RetornoNeg)
    ProbPos

    #ProbabilidadeTmedia =pt(mean(prev),
    #                      df=length(prev)-1,lower.tail=FALSE)
    # Calculo do erro-padrao
    std.error= function(x) {
      sd(x)/sqrt(length(x))
    }
    se = std.error(prev)

    # Calculo da probabilidade excesso de retorno >0 c/ deslocamento da curva T
    if (Asymmetry=="Negative") {
      if (mean(prev)>0) {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(prev)-1,ncp = se, lower.tail=FALSE)
        #print(paste('1) This is the Probability NeuralNEt Prev (-)>0: ',ProbabilidadeTmedia))
        #print(paste("Right asymmetric density (Negative)"))
      } else {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(prev)-1,ncp = -se, lower.tail=FALSE)
        #print(paste("Left asymmetric density (Positive)"))
        #print(paste('2) This is the Probability NeuralNet Prev (-)<0: ',ProbabilidadeTmedia))
      }
    }else {if(Asymmetry=='Positive'){
      if (mean(prev)>0) {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(prev)-1,ncp = -se, lower.tail=FALSE)
        #print(paste("Right asymmetric density (Negative)"))
        #print(paste('3) This is the Probability NeuralNet Prev (+)>0: ',ProbabilidadeTmedia))
      } else {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(prev)-1,ncp = se, lower.tail=FALSE)
        #print(paste("Left asymmetric density (Positive)"))
        #print(paste('4) This is the Probability NeuralNet Prev (+)<0: ',ProbabilidadeTmedia))
      }
    }}
    if (Skew_t[1]=='Yes'){
      tryCatch(
        expr = {
          # Código principal a ser executado
          modelo_ajustado<- selm(prev ~1, family='ST')
          dist_sec <- extractSECdistr(modelo_ajustado)
          xi=dist_sec@dp[1]
          omega=dist_sec@dp[2]
          alpha=dist_sec@dp[3]
          nu=dist_sec@dp[4]
          Resultados_Curtose=kurtosis(prev)
          Resultados_Assim=skewness(prev)
          Media=mean(prev)
          Desvio=stdev(prev)
          KS_test = ks.test(prev,'pnorm')
          KS_pvalue=KS_test$p.value
          AD_test = ad.test(prev)
          AD_pvalue=AD_test$p.value
          if(Skew_t[2]=='Median'){
            Median = median(prev)
            Side_Left = prev[prev < Median]
            Side_Right = prev[prev >= Median]
            Dev_Left = sd(Side_Left)
            Dev_Right = sd(Side_Right)
            Return_Dev_Left = Median-as.numeric(Skew_t[3])*Dev_Left
            Return_Dev_Right = Median + as.numeric(Skew_t[3])*Dev_Right
            #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(prev)-1), family="ST")
            #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
            ProbabilidadeTmedia = pst(0.0, xi=Median, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
            #print(paste('5) This is the Probability NeuralNEt Skew_t Median: ',ProbabilidadeTmedia))
            Prob_Left = pst(0.0, xi=Return_Dev_Left, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
            Prob_Right = pst(0.0, xi=Return_Dev_Right, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
          }

          if(Skew_t[2]=='xi'){
            Prob_esquerda = pst(xi, xi=xi, omega=omega, alpha=alpha, nu=nu)
            Prob_direita = pst(xi, xi=xi, omega=omega, alpha=alpha, nu=nu, lower.tail=FALSE)
            integral = function(x){
              meu_dp = c(xi, omega, alpha)
              (x-xi)^2*sn::dst(x, xi=xi, omega=omega, alpha=alpha,nu=nu)
            }
            integral_LE=integrate(integral, lower=-Inf, upper=xi)$value
            integral_LD=integrate(integral, lower=xi, upper=Inf)$value
            variancia_esquerda = integral_LE/Prob_esquerda
            variancia_direita = integral_LD/Prob_direita
            Dev_Left = sqrt(variancia_esquerda)
            Dev_Right = sqrt(variancia_direita)
            Return_Dev_Left = xi-as.numeric(Skew_t[3])*Dev_Left
            Return_Dev_Right = xi+as.numeric(Skew_t[3])*Dev_Right
            #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(camadaSaidaPredict)-1), family="ST")
            #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
            ProbabilidadeTmedia = pst(0.0, xi=xi, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
            #print(paste('6) This is the Probability NeuralNEt Skew_t xi: ',ProbabilidadeTmedia))
            Prob_Left = pst(0.0, xi=Return_Dev_Left, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
            Prob_Right = pst(0.0, xi=Return_Dev_Right, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
          }



        },
        error = function(e) {
          # Código a ser executado se ocorrer um erro
          ProbabilidadeTmedia=0.0
          xi=0.0
          omega=0.0
          alpha=0.0
          nu=0.0
          Dev_Left=0
          Dev_Right=0
          Prob_Left=0
          Prob_Right=0
          KS_test = ks.test(prev,'pnorm')
          KS_pvalue=KS_test$p.value
          AD_test = ad.test(prev)
          AD_pvalue=AD_test$p.value
          ativos_fora[length(ativos_fora)+1]=ativo

        },
        warning = function(w) {
          # (Opcional) Código a ser executado se ocorrer um aviso (warning)
        },
        finally = {
          # (Opcional) Código a ser executado sempre, independentemente de erro ou aviso
        }
      )


    }
    #print(paste("Excess Return Probability:", ProbabilidadeTmedia, sep=' '))
    #ProbabilidadeTmedia =pt(mean(prev),
    #                    df=length(prev)-1,lower.tail=TRUE)

    colnames(entradas)[1]=paste("(",ativo-1,") ",colnames(dados2[ativo]), sep="")
    colnames(entradas)[2]=paste(colnames(dados2[1]),"-1",sep="")
    for (n_lag in (1:N_Lags)){
      colnames(entradas)[n_lag+2]=paste(colnames(dados2[ativo]),"-",n_lag,sep="")
    }
    #colnames(entradas)[3]=paste(colnames(dados2[ativo]),"-1",sep="")
    #colnames(entradas)[4]=paste(colnames(dados2[ativo]),"-2",sep="")
    #colnames(entradas)[5]=paste(colnames(dados2[ativo]),"-3",sep="")
    #colnames(entradas)[6]=paste(colnames(dados2[ativo]),"-4",sep="")
    #colnames(entradas)[7]=paste(colnames(dados2[ativo]),"-5",sep="")

    View(entradas)

    # Resultados das Probabilidades

    ResProbPosNNet[1,k]= ProbPos
    ResProbTPosNNet[1,k]= ProbabilidadeTmedia
    #############################################################################
    ## REDE NEURAL PARTICULAR

    #############################################################################

    # Acrescentando o Bias ao Banco de dados

    #BIAS da camada de entrada
    #for (i in 1:  nlinhas){
    # treinamento["BIAS"]<-1
    #}


    #View(saidas)
    ncolunas <- ncol(entradas)

    # Gerando pesos iniciais aleat?rios
    options(warn=-1)
    # Fixa a seed para gerar sempre os mesmos números
    set.seed(05)
    # 2. Definição da Arquitetura Manual (Neurônios Decrescentes)
    input_size  <- ncolunas-1
    #hidden_neurons <- Exemplo : c(6, 4, 2) # Camadas ocultas e neurônios decrescentes

    hidden_neurons <- Hidden

    output_size <- 1

    layer_sizes <- c(input_size, hidden_neurons, output_size)
    num_layers  <- length(layer_sizes)

    save(layer_sizes, file = '~/layer_sizes.rda')
    save(num_layers, file='~/num_layers.rda')
    # Fixa a seed para gerar sempre os mesmos números
    # 3. Inicialização de Pesos e Biases (Xavier/Glorot Uniforme Oficial)
    W <- list(); B <- list()
    #load('~/num_layers.rda')
    #load('~/layer_sizes.rda')
    #print(paste("num_layers :",num_layers))
    #print(paste("layer_sizes :",layer_sizes))
    if (Initialization=='Original'){
      for (i in 1:(num_layers - 1)) {
        # n_in: número de entradas da camada atual
        # n_out: número de saídas para a próxima camada
        n_in  <- as.numeric(layer_sizes[i])
        n_out <- as.numeric(layer_sizes[i+1])
        #print(paste("n_in :",n_in))
        #print(paste("n_out :",n_out))
        set.seed(05)
        W[[i]] <- matrix(runif(n_in * n_out, min = 0.0, max = 1.0), nrow = n_in, ncol = n_out)
        #print(paste("W :",W))
        B[[i]] <- matrix(0, nrow = 1, ncol = n_out)
      }
    }else{
      if (Initialization=='Xavier_N'){
        for (i in 1:(num_layers - 1)) {
          # n_in: número de entradas da camada atual
          # n_out: número de saídas para a próxima camada
          n_in  <- as.numeric(layer_sizes[i])
          n_out <- as.numeric(layer_sizes[i+1])
          limite_xavier <- sqrt(2 / (n_in + n_out)) # Ideal para distribuição normal
          set.seed(05)
          W[[i]] <- matrix(rnorm(n_in * n_out, mean = 0, sd = limite_xavier),
                           nrow = n_in, ncol = n_out)
          B[[i]] <- matrix(0, nrow = 1, ncol = n_out)
        }
      }else{
        if (Initialization=='Xavier_U'){
          for (i in 1:(num_layers - 1)) {
            # n_in: número de entradas da camada atual
            # n_out: número de saídas para a próxima camada
            n_in  <- as.numeric(layer_sizes[i])
            n_out <- as.numeric(layer_sizes[i+1])
            # Fórmula de Xavier/Glorot Uniforme para o limite do intervalo
            limite_xavier <- sqrt(6 / (n_in + n_out)) # Ideal para distribuição uniforme
            set.seed(05)
            W[[i]] <- matrix(runif(n_in * n_out, min = -limite_xavier, max = limite_xavier),
                             nrow = n_in, ncol = n_out)
            B[[i]] <- matrix(0, nrow = 1, ncol = n_out)
          }
        }else{
          if (Initialization=='Xavier_H'){
            for (i in 1:(num_layers - 1)) {
              # n_in: número de entradas da camada atual
              # n_out: número de saídas para a próxima camada
              n_in  <- as.numeric(layer_sizes[i])
              n_out <- as.numeric(layer_sizes[i+1])
              limite_xavier <- sqrt(2 / n_in) # Ideal para função de ativação ReLU
              set.seed(05)
              W[[i]] <- matrix(runif(n_in * n_out, min = -limite_xavier, max = limite_xavier),
                               nrow = n_in, ncol = n_out)
              B[[i]] <- matrix(0, nrow = 1, ncol = n_out)
            }
          }else{
            if (Initialization=='Xavier_Leaky'){
              for (i in 1:(num_layers - 1)) {
                # n_in: número de entradas da camada atual
                # n_out: número de saídas para a próxima camada
                n_in  <- as.numeric(layer_sizes[i])
                n_out <- as.numeric(layer_sizes[i+1])
                # LAÇO DE INICIALIZAÇÃO XAVIER UNIFORME MODIFICADA
                alpha_leaky <- 0.01
                # Cálculo do limite d para a Distribuição Uniforme [-d, d]
                limite_d <- sqrt(6 / ((1 + alpha_leaky^2) * n_in))
                limite_xavier <- sqrt(2 / n_in) # Ideal para função de ativação ReLU
                set.seed(05)
                W[[i]] <- matrix(runif(n_in * n_out, min = -limite_xavier, max = limite_xavier),
                                 nrow = n_in, ncol = n_out)
                B[[i]] <- matrix(0, nrow = 1, ncol = n_out)
                #print(paste("W: ",W))
              }
            }else{
              if (Initialization=='Normal_D'){
                for (i in 1:(num_layers - 1)) {
                  # n_in: número de entradas da camada atual
                  # n_out: número de saídas para a próxima camada
                  n_in  <- as.numeric(layer_sizes[i])
                  n_out <- as.numeric(layer_sizes[i+1])
                  # rnorm gera valores em uma distribuição normal
                  # rnorm sem Xavier padrao
                  set.seed(05)
                  W[[i]] <- matrix(rnorm(n_in * n_out, mean = 0, sd = 1),
                                   nrow = n_in, ncol = n_out)
                  B[[i]] <- matrix(0, nrow = 1, ncol = n_out)
                }
              }else{
                if (Initialization=='Uniform_D'){
                  for (i in 1:(num_layers - 1)) {
                    # n_in: número de entradas da camada atual
                    # n_out: número de saídas para a próxima camada
                    n_in  <- as.numeric(layer_sizes[i])
                    n_out <- as.numeric(layer_sizes[i+1])
                    # rnorm sem Xavier padrao
                    # runif gera valores entre o mínimo (-limite) e o máximo (+limite)
                    # runif sem Xavier definido um intervalo padrão de -0.05 a 0.05 para estabilidade inicial
                    set.seed(05)
                    W[[i]] <- matrix(runif(n_in * n_out, min = -0.05, max = 0.05), nrow = n_in, ncol = n_out)
                    B[[i]] <- matrix(0, nrow = 1, ncol = n_out)
                  }
                }}}}}}}


    #print(paste('W: ', W))
    save (W, file='~/W.rda')
    save (B, file='~/B.rda')

    my_list=function(){
      lista_fun=list(
        # Funcao de Ativacao
        Tangente = function(soma) {
          #Funcao Tangente Hiperbolica
          #return (1-tanh(soma))
          return (tanh(soma))
        },
        Sigmoide = function(soma) {
          return (1/ (1+exp(-soma)))
        },
        # Derivada da funcao Sigmoide
        SigmoideDerivada = function(sig) {
          return (sig * (1-sig))
        },
        # Derivada da funcao Tangente
        TangenteDerivada = function(tanh_out) {
          return (1 - (tanh_out ^ 2))
        },
        leaky_relu = function(x, alpha = 0.01) {
          ifelse(x > 0, x, alpha * x)
        },

        leaky_relu_derivative = function(x, alpha = 0.01) {
          ifelse(x > 0, 1, alpha)
        }
      )
      return(lista_fun)
    }
    functions=my_list()
    # Estimando o n?mero de ?pocas e a taxa de aprendizagem
    epocas = Stepmax
    epochs = Stepmax
    momento = 1
    taxaAprendizagem = Learning_Rate
    lr = Learning_Rate
    # Certifique-se de que a variável de texto bate exatamente com os testes abaixo
    # Exemplo: Activation_Function <- "Tangent"

    if (Activation_Function == 'Tangent') {
      Activation_Fun = functions$Tangente
      derivada = functions$TangenteDerivada

    } else if (Activation_Function == 'Sigmoid') {
      Activation_Fun = functions$Sigmoide
      derivada = functions$SigmoideDerivada

    } else if (Activation_Function == 'Leaky_ReLU') {
      Activation_Fun = functions$leaky_relu
      derivada = functions$leaky_relu_derivative

    } else {
      stop("Função de ativação desconhecida! Verifique a ortografia.")
    }


    bias_saida=0
    bias_hidden=0

    I_dataPredict = F_data+1
    F_dataPredict = nrow(dat_r)-1
    entradasPredict = as.matrix(dat_r[I_dataPredict:F_dataPredict,])
    saidasPredict = as.matrix(dat_r[(I_dataPredict+1):(F_dataPredict+1),1])
    ################################################################################
    # Training Loop - N-Layers
    ############################################ New ###############################
    #print(paste('Length W: ', length(W)))
    #print(paste('Length B: ', length(B)))
    #print(paste('Activation_Fun: ', Activation_Fun))
    X_train = entradas[, 2:ncol(entradas)]
    y_train = saidas
    n_amostras   <- nrow(X_train)
    if(Batch_Size==''){
      Batch_Size=n_amostras
    }
    num_batches  <- ceiling(n_amostras / Batch_Size)
    for (ep in 1:epochs) {
      # Ordem estritamente cronológica para séries temporais (SEM EMBARALHAMENTO)
      for (b in 1:num_batches) {
        start_idx <- ((b - 1) * Batch_Size) + 1
        end_idx   <- min(b * Batch_Size, n_amostras)
        X_batch <- X_train[start_idx:end_idx, , drop = FALSE]
        y_batch <- y_train[start_idx:end_idx, drop = FALSE]
        n_B     <- nrow(X_batch)
        #N <- nrow(entradas)
        N = n_B
        # Forward Pass
        A <- vector("list", num_layers) # Aloca o tamanho exato da lista na memória
        A[[1]] <- X_batch

        # Loop controlado estritamente por num_layers (vai de 1 até 2)
        for (i in 1:(num_layers - 1)) {

          # Realiza o cálculo da combinação linear
          net <- sweep(A[[i]] %*% W[[i]], 2, B[[i]], "+")

          # Verifica se é a última iteração (camada de saída)
          if (i == (num_layers - 1)) {
            if (Activation_F_Out == 'Linear') {
              A[[i+1]] <- net # Saída linear (regressão)
            } else if (Activation_F_Out == 'Original' || Activation_F_Out == 'Activation') {
              A[[i+1]] <- Activation_Fun(net) # Aplica a Ativacao na camada de Saída
            } else {
              A[[i+1]] <- Activation_Fun(net) # Fallback padrão
            }
          } else {
            # Camadas ocultas intermediárias
            A[[i+1]] <- Activation_Fun(net)
          }
        }

        #save (A, file='~/A.rda')
        #print(paste('Length A: ', length(A)))
        # Cálculo do Erro
        camadaSaida_batch <- A[[num_layers]]
        save(camadaSaida_batch, file='~/camadaSaida_batch.rda')

        ##################################### Loss Function #############################
        # back forward
        R_predicted = camadaSaida_batch
        R_observed = y_batch

        # CORREÇÃO CRÍTICA: Inicializa a lista delta com o tamanho total de camadas antes do bloco condicional
        delta <- vector("list", num_layers)


        if (Loss == "MSE") {
          errocamadaSaida_batch = mean((y_batch - camadaSaida_batch)^2)
          grad_perda <- A[[num_layers]] - y_batch

          if (Activation_F_Out == 'Original') {
            delta[[num_layers]] = as.matrix(rep(errocamadaSaida_batch, times=nlinhas), ncol=1) * (camadaSaida_batch - (1 - camadaSaida_batch))
          } else if (Activation_F_Out == 'Tangent') {
            delta[[num_layers]] <- grad_perda * (1 - (A[[num_layers]] ^ 2))
          } else if (Activation_F_Out == 'Linear') {
            delta[[num_layers]] <- grad_perda
          } else {
            # SEGURANÇA: Se o texto não bater com nenhum acima, assume Linear por padrão para não quebrar
            warning(paste("Aviso: Activation_F_Out inválido ('", Activation_F_Out, "'). Usando 'Linear' por padrão.", sep=""))
            delta[[num_layers]] <- grad_perda
          }

        } else if (Loss == "MAE") {
          errocamadaSaida_batch = y_batch - camadaSaida_batch
          mediaAbsoluta = mean(abs(errocamadaSaida_batch))
          errocamadaSaida_batch = mediaAbsoluta
          grad_perda <- sign(A[[num_layers]] - Y_real)

          if (Activation_F_Out == 'Original') {
            delta[[num_layers]] = as.matrix(rep(errocamadaSaida_batch, times=nlinhas), ncol=1) * (camadaSaida_batch - (1 - camadaSaida_batch))
          } else if (Activation_F_Out == 'Tangent') {
            delta[[num_layers]] <- grad_perda * (1 - (A[[num_layers]] ^ 2))
          } else if (Activation_F_Out == 'Linear') {
            delta[[num_layers]] <- grad_perda
          } else {
            # SEGURANÇA: Se o texto não bater com nenhum acima, assume Linear por padrão para não quebrar
            warning(paste("Aviso: Activation_F_Out inválido ('", Activation_F_Out, "'). Usando 'Linear' por padrão.", sep=""))
            delta[[num_layers]] <- grad_perda
          }

        } else if (Loss == "MADL") {
          madl_loss <- function(R_observed, R_predicted) {
            N <- length(R_observed)
            loss <- (1/N) * sum((-1) * sign(R_observed * R_predicted) * abs(R_observed))
            return(loss)
          }
          errocamadaSaida_batch = madl_loss(y_batch, camadaSaida_batch)
          R_real <- Y_real
          R_pred <- A[[num_layers]]

          direcao_errada <- sign(R_real) != sign(R_pred)
          grad_perda <- matrix(0, nrow = nrow(R_real), ncol = ncol(R_real))
          grad_perda[direcao_errada] <- sign(R_pred[direcao_errada] - R_real[direcao_errada])

          if (Activation_F_Out == 'Original') {
            delta[[num_layers]] = as.matrix(rep(errocamadaSaida_batch, times=nlinhas), ncol=1) * (camadaSaida_batch - (1 - camadaSaida_batch))
          } else if (Activation_F_Out == 'Tangent') {
            delta[[num_layers]] <- grad_perda * (1 - (A[[num_layers]] ^ 2))
          } else if (Activation_F_Out == 'Linear') {
            delta[[num_layers]] <- grad_perda
          } else {
            # SEGURANÇA: Se o texto não bater com nenhum acima, assume Linear por padrão para não quebrar
            warning(paste("Aviso: Activation_F_Out inválido ('", Activation_F_Out, "'). Usando 'Linear' por padrão.", sep=""))
            delta[[num_layers]] <- grad_perda
          }

        } else if (Loss == "GMADL") {
          gmadl_loss <- function(R_observed, R_predicted, a = 1, b = 1) {
            N <- length(R_observed)
            sigmoid <- function(x) { 1 / (1 + exp(-x)) }
            loss <- (1/N) * sum(- (sigmoid(a * R_observed * R_predicted) - 0.5) * abs(R_observed)^b)
            return(loss)
          }
          errocamadaSaida_batch = gmadl_loss(y_batch, camadaSaida_batch, a = 1, b = 1)
          a_param <- 1.0
          b_param <- 1.0
          R_real <- y_batch
          R_pred <- A[[num_layers]]

          argumento <- a_param * R_real * R_pred
          sigm_dir <- 1 / (1 + exp(-argumento))
          grad_perda <- -a_param * R_real * sigm_dir * (1 - sigm_dir) * (abs(R_real) ^ b_param)

          if (Activation_F_Out == 'Original') {
            delta[[num_layers]] = as.matrix(rep(errocamadaSaida_batch, times=nlinhas), ncol=1) * (camadaSaida_batch - (1 - camadaSaida_batch))
          } else if (Activation_F_Out == 'Tangent') {
            delta[[num_layers]] <- grad_perda * (1 - (A[[num_layers]] ^ 2))
          } else if (Activation_F_Out == 'Linear') {
            delta[[num_layers]] <- grad_perda
          } else {
            # SEGURANÇA: Se o texto não bater com nenhum acima, assume Linear por padrão para não quebrar
            warning(paste("Aviso: Activation_F_Out inválido ('", Activation_F_Out, "'). Usando 'Linear' por padrão.", sep=""))
            delta[[num_layers]] <- grad_perda
          }
        }

        ########### Implementação da Regularização
        if (Decay[1] == 'Yes') {
          # CORREÇÃO: Pega os pesos da última camada de transição (num_layers - 1) pois 'i' não está no escopo aqui
          weights = W[[num_layers - 1]]
          if (length(Decay) == 1) { Decay = c(Decay, 0.1) }
          lambda = as.numeric(Decay[2])

          l2_regularization <- function(weights, lambda) {
            return(lambda * sum(weights^2))
          }

          reg_term <- l2_regularization(weights, lambda)
          errocamadaSaida_batch <- errocamadaSaida_batch + reg_term
          if (Activation_F_Out == 'Original') {
            delta[[num_layers]] = as.matrix(rep(errocamadaSaida_batch, times=nlinhas), ncol=1) * (camadaSaida_batch - (1 - camadaSaida_batch))
          }
        } else {
          lambda = 0 # Valor padrão caso Decay seja 'No' para não quebrar a atualização dos pesos
        }


        ################################################################################
        ################################################################################

        #error <- as.matrix(rep(erroCamadaSaida, nlinhas))

        # CORREÇÃO CRÍTICA: Garante que o delta da camada de saída seja uma matriz antes de multiplicar
        delta[[num_layers]] <- as.matrix(delta[[num_layers]])

        # Backward Pass
        for (i in (num_layers - 1):2) {
          # Transforma o delta posterior em matriz por segurança em redes profundas
          delta[[i+1]] <- as.matrix(delta[[i+1]])

          # Realiza a multiplicação de matrizes sem quebrar
          delta[[i]] <- (delta[[i+1]] %*% t(W[[i]])) * derivada(A[[i]])
        }

        # Atualização de Pesos e Biases divididos por N (Média do Gradiente)
        for (i in 1:(num_layers - 1)) {

          # Garante que o delta usado na atualização também seja tratado como matriz
          delta_atual <- as.matrix(delta[[i+1]])
          gradiente_W <- (t(A[[i]]) %*% delta_atual) / N

          if (Activation_F_Out == 'Original') {
            W[[i]] <- W[[i]] - (lr * gradiente_W)
          } else {
            W[[i]] <- W[[i]] * (1 - lr * lambda) - (lr * gradiente_W)
          }

          if (Bias == 'Yes') {
            B[[i]] <- B[[i]] - (lr * matrix(colSums(delta_atual), nrow = 1) / N)
          }
        }


      } # Chave de fechamento do loop principal de épocas (for (ep in 1:epochs))
      ################################################################################
      # End Loop Training - N-Layers - With Batch-Size
      ######################################################################## New MLP
      # Epoch Evaluation - Training vs Testing
      ################################################################################

      ###### ---- Bloco de Treinamento
      ####### --- Bloco de Predição (Garantido que o R vai ler agora) --- ############
      camadaEntrada_Train = as.matrix(entradas)

      # Forward Pass da Predição
      A_train <- vector("list", num_layers) # Usando nome limpo para não misturar com o 'A' do treino
      A_train[[1]] <- camadaEntrada_Train[, 2:ncol(camadaEntrada_Train)]

      for (i in 1:(num_layers - 1)) {
        net_train <- sweep(A_train[[i]] %*% W[[i]], 2, B[[i]], "+")

        if (i == (num_layers - 1)) {
          if (Activation_F_Out == 'Linear') {
            A_train[[i+1]] <- net_train
          } else {
            A_train[[i+1]] <- Activation_Fun(net_train)
          }
        } else {
          A_train[[i+1]] <- Activation_Fun(net_train)
        }
      }

      # Cálculo do Erro de Treinamento
      camadaSaida_Train <- A_train[[num_layers]]

      # Verificação da Função de Perda no Treinamento (Estrutura Segura else if)
      if (Loss == "MSE") {
        erroCamadaSaida_Train = mean((saidas - camadaSaida_Train)^2)

      } else if (Loss == "MAE") {
        erroCamadaSaida_Train = mean(abs(saidas - camadaSaida_Train))

      } else if (Loss == "MADL") {
        madl_loss <- function(R_observed, R_predicted) {
          N <- length(R_observed)
          loss <- (1/N) * sum((-1) * sign(R_observed * R_predicted) * abs(R_observed))
          return(loss)
        }
        erroCamadaSaida_Train = madl_loss(saidas, camadaSaida_Train)

      } else if (Loss == "GMADL") {
        gmadl_loss <- function(R_observed, R_predicted, a = 1, b = 1) {
          N <- length(R_observed)
          sigmoid <- function(x) { 1 / (1 + exp(-x)) }
          loss <- (1/N) * sum(- (sigmoid(a * R_observed * R_predicted) - 0.5) * abs(R_observed)^b)
          return(loss)
        }
        erroCamadaSaida_Train = gmadl_loss(saidas, camadaSaida_Train, a = 1, b = 1)

      } else {
        # Fallback de segurança para não deixar a variável vazia
        erroCamadaSaida_Train = mean((saidas - camadaSaida_Train)^2)
      }


      erroCamadaSaida=erroCamadaSaida_Train
      camadaSaida=camadaSaida_Train
      ################################################################################
      if (ep == epocas) {
        print(paste("Train Loss:", erroCamadaSaida_Train))
        print(paste("Real Train Sd:", round(erroCamadaSaida_Train^0.5*100,2),"%"))
        print(paste("Predict Train Sd:", round(sd(saidas)*100,2),"%"))
        }


      ####### --- Bloco de Predição (Garantido que o R vai ler agora) --- ############
      camadaEntradaPredict = as.matrix(entradasPredict)

      # Forward Pass da Predição
      A_pred <- vector("list", num_layers) # Usando nome limpo para não misturar com o 'A' do treino
      A_pred[[1]] <- camadaEntradaPredict[, 2:ncol(camadaEntradaPredict)]

      for (i in 1:(num_layers - 1)) {
        net_pred <- sweep(A_pred[[i]] %*% W[[i]], 2, B[[i]], "+")

        if (i == (num_layers - 1)) {
          if (Activation_F_Out == 'Linear') {
            A_pred[[i+1]] <- net_pred
          } else {
            A_pred[[i+1]] <- Activation_Fun(net_pred)
          }
        } else {
          A_pred[[i+1]] <- Activation_Fun(net_pred)
        }
      }

      # Cálculo do Erro de Predição
      camadaSaidaPredict <- A_pred[[num_layers]]

      # Verificação da Função de Perda na Predição (Estrutura Segura else if)
      if (Loss == "MSE") {
        erroCamadaSaidaPredict = mean((saidasPredict - camadaSaidaPredict)^2)

      } else if (Loss == "MAE") {
        erroCamadaSaidaPredict = mean(abs(saidasPredict - camadaSaidaPredict))

      } else if (Loss == "MADL") {
        madl_loss <- function(R_observed, R_predicted) {
          N <- length(R_observed)
          loss <- (1/N) * sum((-1) * sign(R_observed * R_predicted) * abs(R_observed))
          return(loss)
        }
        erroCamadaSaidaPredict = madl_loss(saidasPredict, camadaSaidaPredict)

      } else if (Loss == "GMADL") {
        gmadl_loss <- function(R_observed, R_predicted, a = 1, b = 1) {
          N <- length(R_observed)
          sigmoid <- function(x) { 1 / (1 + exp(-x)) }
          loss <- (1/N) * sum(- (sigmoid(a * R_observed * R_predicted) - 0.5) * abs(R_observed)^b)
          return(loss)
        }
        erroCamadaSaidaPredict = gmadl_loss(saidasPredict, camadaSaidaPredict, a = 1, b = 1)

      } else {
        # Fallback de segurança para não deixar a variável vazia
        erroCamadaSaidaPredict = mean((saidasPredict - camadaSaidaPredict)^2)
      }
      if (ep == epocas) {
        print(paste("Test Loss:", erroCamadaSaidaPredict))
        print(paste("Real Test Sd:", round(erroCamadaSaidaPredict^0.5*100,2),'%'))
        print(paste("Predict Test Sd:", round(sd(saidasPredict)*100,2),"%"))
        print(paste("Loss Precision:",round((erroCamadaSaida_Train^0.5-erroCamadaSaidaPredict^0.5)*100,2),"%"))
        }

      # Early_Stopping
      if (length(Early_Stopping) != 1) {
        if (ep==1) {Anterior_Loss=0}
        Stop = as.numeric(Early_Stopping[2])
        if (class(erroCamadaSaidaPredict) == "numeric" && (erroCamadaSaidaPredict < 10)) {

          Delta_Loss= abs(erroCamadaSaidaPredict - Anterior_Loss)
          if ((Delta_Loss < Stop) == TRUE) {
            print(paste("Early stop with", ep, " epochs"))
            print(paste("Train_Loss:", erroCamadaSaida_Train))
            print(paste("Real Train Sd:", round(erroCamadaSaida_Train^0.5*100,2),"%"))
            print(paste("Predict Train Sd:", round(sd(saidas)*100,2), "%"))
            print(paste("Real Test Sd:", round(erroCamadaSaidaPredict^0.5*100,2),"%"))
            print(paste("Test_Loss:", erroCamadaSaidaPredict))
            print(paste("Predict Test Sd:", round(sd(saidasPredict)*100,2),"%"))
            print(paste("Loss Precision",round((erroCamadaSaida_Train^0.5-erroCamadaSaidaPredict^0.5)*100,2),"%"))
            print(paste("Delta_Loss:", Delta_Loss))
            ep = epocas
            break
          }
          Anterior_Loss=erroCamadaSaidaPredict
        }
      }
    } # Chave de fechamento do loop principal de épocas (for (ep in 1:epochs))
    ################################################################################
    # End Epoch Evaluation - Training ########################### New MLP
    ################################################################################


    #camadaSaida = sigmoide(somaSinapse1)
    KS_test = ks.test(prev,'pnorm')
    KS_pvalue=KS_test$p.value
    AD_test = ad.test(prev)
    AD_pvalue=AD_test$p.value
    ########################################################

    if ((Order_Only='Yes')==TRUE){
      CamadaSaida=entradas
    }
    save(camadaSaida,file='~/camadaSaida.rda')
    mean(camadaSaida)
    median(camadaSaida)
    sd(camadaSaida)
    std.error= function(x) {
      sd(x)/sqrt(length(x))
    }
    se = std.error(camadaSaida)
    # Probabildiade Normal
    pnorm(q = 0.01, # probabilidade do retorno ser superior a 0.1%
          mean = mean(camadaSaida), # media
          sd = sd(camadaSaida), # desvio-pedrao
          lower.tail = FALSE) # Calcula P[X >x]
    ## Probabilidade com curtorese (Lambda > 3) = Probabilidade t Student
    library(moments)
    #png(file = "leptokurtic.png")

    #if (kurtosis(camadaSaida)>3) {
    # print("Leptokutic curve kurtosis:")
    #print(kurtosis(camadaSaida))
    #}
    t = 3.373 # p =0.1
    q = -0.4
    ProbtStudent = (q-mean(camadaSaida))/(std.error(camadaSaida))
    #ProbtStudent
    ProbStudent = t*(std.error(camadaSaida)) + mean(camadaSaida)
    #ProbStudent

    # Calculo da probabilidade excesso de retorno >0 c/ deslocamento da curva T
    #ProbabilidadeTmedia =pt(mean(camadaSaida),
    #                       df=length(camadaSaida)-1, lower.tail=FALSE)
    ################################################################################
    #### Teste se soma ou subrai o sd na assimetria (Padrão=soma se (standart error) = Assimetria negativa)
    if (Asymmetry=="Negative") {
      if ((mean(camadaSaida)>0)==TRUE) {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(camadaSaida)-1,ncp = se, lower.tail=FALSE)
        # cat("Right asymmetric density (Negative)")
        #print(paste('7) This is the Probability Particular camadaSaida (-)>0: ',ProbabilidadeTmedia))
      } else {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(camadaSaida)-1,ncp = -se, lower.tail=FALSE)
        #cat("Left asymmetric density (Positive)")
        #print(paste('8) This is the Probability Particular camadaSaida (-)<0: ',ProbabilidadeTmedia))
      }
    }else {if(Asymmetry=='Positive'){
      if ((mean(camadaSaida)>0)==TRUE) {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(camadaSaida)-1,ncp = -se, lower.tail=FALSE)
        #print(paste('9) This is the Probability Particular camadaSaida (+)>0: ',ProbabilidadeTmedia))
        # cat("Right asymmetric density (Negative)")
      } else {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(camadaSaida)-1,ncp = se, lower.tail=FALSE)
        #cat("Left asymmetric density (Positive)")
        #print(paste('10) This is the Probability Particular camadaSaida (+)<0: ',ProbabilidadeTmedia))
      }
    }}
    if (Skew_t[1]=='Yes'){
      tryCatch(
        expr = {
          # Código principal a ser executado
          #print('Test TryCatch')
          #modelo_ajustado<- selm(camadaSaida ~1, family='ST')
          modelo_ajustado <- withCallingHandlers(
            {
              sn::selm(camadaSaida ~ 1, family = 'ST', method = "MPLE")
            },
            warning = function(w) {
              invokeRestart("muffleWarning") # Abafa o aviso e impede o pulo para o erro
            },
            message = function(m) {
              invokeRestart("muffleMessage") # Abafa mensagens de texto secundárias se houverem
            })
          #print('Test 2 TryCatch')
          dist_sec <- extractSECdistr(modelo_ajustado)
          xi<-dist_sec@dp[1]
          omega<-dist_sec@dp[2]
          alpha<-dist_sec@dp[3]
          nu<-dist_sec@dp[4]
          Resultados_Curtose<-kurtosis(camadaSaida)
          Resultados_Assim<-skewness(camadaSaida)
          Media<-mean(camadaSaida)
          Desvio<-stdev(camadaSaida)
          CamadaSaida_KS<-jitter(camadaSaida)
          KS_test <- ks.test(CamadaSaida_KS,'pnorm')
          KS_pvalue<-KS_test$p.value
          AD_test <- ad.test(camadaSaida)
          AD_pvalue<-AD_test$p.value
          #print('Test 1 TryCatch')
          if (Skew_t[2]=='Median'){
            Median <- median(camadaSaida)
            Side_Left <- camadaSaida[camadaSaida < Median]
            Side_Right <- camadaSaida[camadaSaida >= Median]
            Dev_Left_1 <- sd(Side_Left)
            Dev_Right_1 <- sd(Side_Right)
            Return_Dev_Left_1 <- Median-as.numeric(Skew_t[3])*Dev_Left_1
            Return_Dev_Right_1 <- Median + as.numeric(Skew_t[3])*Dev_Right_1
            ProbabilidadeTmedia <- pst(0.0, xi=Median, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
            #print(paste('11) This is the Probability Particular camadaSaida Skew_t Median: ',ProbabilidadeTmedia))
            Prob_Left_1 <- pst(0.0, xi=Return_Dev_Left_1, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
            Prob_Right_1 <- pst(0.0, xi=Return_Dev_Right_1, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
          }
          #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(camadaSaida)-1), family="ST")
          #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
          if(Skew_t[2]=='xi'){
            Prob_esquerda <- pst(xi, xi=xi, omega=omega, alpha=alpha, nu=nu)
            Prob_direita <- pst(xi, xi=xi, omega=omega, alpha=alpha, nu=nu, lower.tail=FALSE)
            integral <- function(x){
              meu_dp <- c(xi, omega, alpha)
              (x-xi)^2*sn::dst(x, xi=xi, omega=omega, alpha=alpha,nu=nu)
            }
            #print('Test 3 TryCatch')
            #integral_LE <- integrate(integral, lower=-Inf, upper=xi)$value
            integral_LE <- tryCatch({
              integrate(integral, lower=-Inf, upper=xi)$value
            }, error = function(e) {
              limite_inferior <- xi - (15 * omega)
              integrate(integral, lower=limite_inferior, upper=xi)$value
            })
            #integral_LD<-integrate(integral, lower=xi, upper=Inf)$value
            integral_LD <- tryCatch({
              integrate(integral, lower=xi, upper=Inf)$value
            }, error = function(e) {
              limite_superior <- xi + (15 * omega)
              integrate(integral, lower=xi, upper=limite_superior)$value
            })
            variancia_esquerda <- integral_LE/Prob_esquerda
            variancia_direita <- integral_LD/Prob_direita
            Dev_Left_1 <- sqrt(variancia_esquerda)
            Dev_Right_1 <- sqrt(variancia_direita)
            Return_Dev_Left_1 <- xi-as.numeric(Skew_t[3])*Dev_Left_1
            Return_Dev_Right_1 <- xi+as.numeric(Skew_t[3])*Dev_Right_1
            #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(camadaSaidaPredict)-1), family="ST")
            #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
            #print('Test 4 TryCatch')
            ProbabilidadeTmedia <- pst(0.0, xi=xi, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
            #print(paste('12) This is the Probability Particular camadaSaida Skew_t xi: ',ProbabilidadeTmedia))
            Prob_Left_1 <- pst(0.0, xi=Return_Dev_Left_1, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
            Prob_Right_1 <- pst(0.0, xi=Return_Dev_Right_1, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 2)
          }

          #print('Test 3 TryCatch')
          TEste_P <-ProbabilidadeTmedia
          ProbabilidadeTmedia_1 <-ProbabilidadeTmedia
          xi_1<-xi
          omega_1<-omega
          alpha_1<-alpha
          nu_1<-nu
          KS_test_1 <- KS_test
          KS_pvalue_1<-KS_pvalue
          AD_test_1 <- AD_test
          AD_pvalue_1<-AD_pvalue

        },  warning = function(w) {
          message("Notice captured: ", w$message)
          #invokeRestart("muffleWarning") # Se quiser silenciar o aviso completamente
        },
        error = function(e) {
          # Código a ser executado se ocorrer um erro
          ProbabilidadeTmedia <-0.0
          xi<-0.0
          omega<-0.0
          alpha<-0.0
          nu<-0.0
          KS_test <- ks.test(camadaSaida,'pnorm')
          KS_pvalue<-KS_test$p.value
          AD_test <- ad.test(na.omit(camadaSaida))

          AD_pvalue<-AD_test$p.value
          ativos_fora[length(ativos_fora)+1]<-ativo
          ProbabilidadeTmedia_1 <-0.0
          xi_1<-0.0
          omega_1<-0.0
          alpha_1<-0.0
          nu_1<-0.0
          KS_test_1 <- KS_test
          KS_pvalue_1<-KS_pvalue
          AD_test_1 <- AD_test
          AD_pvalue_1<-AD_pvalue
          Dev_Left_1<-0.0
          Dev_Right_1<-0.0
          Prob_Left_1<-0.0
          Prob_Right_1<-0.0
          print("Details of the error generated:")
          print(e)
        },
        finally = {
          # (Opcional) Código a ser executado sempre, independentemente de erro ou aviso
        }
      )



    }
    ################################################################################
    #ProbabilidadeTmedia =pt(mean(camadaSaida),
    #                 df=length(camadaSaida)-1, lower.tail = TRUE)

    Probabilidadest = NULL
    for (l in 1:nlinhas){
      Probabilidadest[l] = pt(camadaSaida[l],
                              df=length(camadaSaida)-1, lower.tail=FALSE)
    }

    # Plot RM e a rede neural artificial no teste
    library(data.table)
    Data <- as.data.table(camadaSaida, keep.rownames = TRUE)
    Data <- as.data.frame(Data)

    nome = colnames(entradas)[1]
    matplot(cbind(camadaSaida, saidas), type = "l", xaxt = "n",
            xlab = "Data",
            ylab = "Retorno",
            main = paste("Otimiza??o RNA Fase de Treinamento - Ativo",
                         xname = nome))
    legend("topright", legend = c("RNA", nome), pch = 19, col = c("black", "red"))
    axis(1, 1:nlinhas, Data$rn)

    ## Sinal Maior que a media
    #sinal = NULL
    #for (i in 1:nlinhas) {
    #  if(camadaSaida[i] > mean(camadaSaida)){
    #    sinal[i] = 1
    #  }
    #  else {
    #    sinal[i] = -1
    #  }
    #}
    #sinal

    ## Sinal POSITIVO
    sinal = NULL
    for (i in 1:nlinhas) {
      if(camadaSaida[i] > 0){
        sinal[i] = 1
      }
      else {
        sinal[i] = -1
      }
    }
    sinal

    ### Probabilidades
    RetornoPos <- sum(sinal ==1)
    RetornoNeg <- sum(sinal == -1)
    ProbPos <- RetornoPos/(RetornoPos + RetornoNeg)
    #ProbPos

    # Resultados das Probabilidades

    ResProbPos[1,k]= ProbPos
    ResProbTPos[1,k]= ProbabilidadeTmedia

    #dev.off() ### Salvando gr?ficos anteriores

    ## Exibindo histograma
    #hist(camadaSaida,
    #    main = paste("Histograma Previs?es RNA Fase de Treinamento - Ativo",
    #                xnames= nome),
    #  xlab = paste("Retorno Excedente sobre", xnames = RM))

    ############################################################################
    ############################################################################
    ############################################################################
    ################################## Fase de Teste ###########################
    ############################################################################
    ############################################################################
    ############################################################################



    ####################### Amostra de Teste ###################################
    # Criando as vari?veis como vetor para teste- com o restantes da amostra que
    # excede o Tratamento at? o ?ltimo preg?o
    I_dataPredict = F_data+1
    F_dataPredict = nrow(dat_r)-1
    entradasPredict = as.matrix(dat_r[I_dataPredict:F_dataPredict,])
    saidasPredict = as.matrix(dat_r[(I_dataPredict+1):(F_dataPredict+1),1])


    # Criando as vari?veis como vetor para teste - com datas espec?ficas
    #Inicio_dataPredict = "2022-01-04"
    #Fim_dataPredict = "2022-06-23"
    #I_dataPredict = which(rownames(dat_r)==Inicio_dataPredict)
    #F_dataPredict = which(rownames(dat_r)==Fim_dataPredict)
    #entradasPredict = as.matrix(dat_r[I_dataPredict:F_dataPredict,])
    #saidasPredict = as.matrix(dat_r[(I_dataPredict+1):(F_dataPredict+1),1])


    #####
    # Criando as vari?veis como vetor para teste -50% dos dados
    #comprimentoPredict = ceiling(0.5*nrow(dat_r)) # ceiling arredonda para mais
    #testePredict=as.data.frame(dat_r)
    #nAtivosPredict = ncol(dados)
    #entradasPredict = as.matrix(testePredict[1:comprimentoPredict,])
    #saidas = as.matrix(testePredict[2:(comprimentoPredict+1),1])
    #I_data = entradasPredict[1]
    #F_data = entradasPredict[length(entradasPredict)]

    #####
    nlinhasPredict <- nrow(entradasPredict)
    ncolunasPredict <- ncol(entradasPredict)

    ## Previs?o
    if (Prediction=='Predict'){
      prevPredict = predict(nn, entradasPredict)}
    else {
      prevPredict = forecast(nn, h=length(entradasPredict))
    }

    if ((Order_Only='Yes')==TRUE){
      prevPredict=entradasPredict
    }
    nome = colnames(entradasPredict)[1]
    KS_test = ks.test(prevPredict,'pnorm')
    KS_pvalue=KS_test$p.value
    if (length(prevPredict) <= 7) {
      tryCatch(
        expr = {
          AD_test =shapiro.test(prevPredict)
        },
        error = function(e) {
          AD_test = 0
        }
      )
    }else{
      AD_test = ad.test(prevPredict)
    }
    AD_pvalue=AD_test$p.value

    plot(as.vector(entradasPredict[,1]), type="l", col = "red",
         main = paste("Retornos Amostra de Teste", xnames = nome))
    legend("topright", legend = c("RNA", nome), pch = 19,
           col = c("black", "red"))
    lines(prevPredict)


    hist(prevPredict,
         main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
                      xnames= nome),
         xlab = paste("Retorno Excedente sobre", xnames = "RM"))
    mean(prevPredict)

    TestesPredict = compute(nn, entradasPredict)
    TestesPredict$net.result
    hist(TestesPredict$net.result,
         main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
                      xnames= nome), xlab = paste("Retorno Excedente sobre",
                                                  xnames = "RM"))
    ## Avaliando a acur?cia (ME, RMSE, MAE, MPE e MAPE)
    dat_r2Predict = na.fill(entradasPredict, "extend")
    prev2Predict = na.fill(entradasPredict, "extend")
    accuracy(as.vector(prev2Predict),entradas[1])

    sinal = NULL
    nlinhasPredict = nrow(entradasPredict)
    for (i in 1:nlinhasPredict) {
      if(prevPredict[i] > 0){
        sinal[i] = 1
      }
      else {
        sinal[i] = -1
      }
    }
    sinal
    ### Probabilidades
    RetornoPos <- sum(sinal ==1)
    RetornoNeg <- sum(sinal == -1)
    ProbPos <- RetornoPos/(RetornoPos + RetornoNeg)
    ProbPos

    #ProbabilidadeTmedia =pt(mean(prev),
    #                      df=length(prev)-1,lower.tail=FALSE)
    # Calculo do erro-padrao
    std.error= function(x) {
      sd(x)/sqrt(length(x))
    }
    se = std.error(prevPredict)

    # Calculo da probabilidade excesso de retorno >0 c/ deslocamento da curva T
    if (Asymmetry=="Negative") {
      if (mean(prevPredict)>0) {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(prevPredict)-1,ncp = se,
                                lower.tail=FALSE)
        #cat("Right asymmetric density (Negative)")
      } else {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(prevPredict)-1,ncp = -se,
                                lower.tail=FALSE)
        #cat("Left asymmetric density (Positive)")
      }
    }else {if(Asymmetry=='Positive'){
      if (mean(prevPredict)>0) {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(prevPredict)-1,ncp = -se,
                                lower.tail=FALSE)
        #cat("Right asymmetric density (Negative)")
      } else {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(prevPredict)-1,ncp = se,
                                lower.tail=FALSE)
        #cat("Left asymmetric density (Positive)")
      }
    }}
    if (Skew_t[1]=='Yes'){
      tryCatch(
        expr = {
          # Código principal a ser executado
          modelo_ajustado<- selm(prevPredict ~1, family='ST')
          dist_sec <- extractSECdistr(modelo_ajustado)
          xi=dist_sec@dp[1]
          omega=dist_sec@dp[2]
          alpha=dist_sec@dp[3]
          nu=dist_sec@dp[4]
          Resultados_Curtose=kurtosis(prevPredict)
          Resultados_Assim=skewness(prevPredict)
          Media=mean(prevPredict)
          Desvio=stdev(prevPredict)
          KS_test = ks.test(prevPredict,'pnorm')
          KS_pvalue=KS_test$p.value
          if (length(prevPredict) <= 7) {
            tryCatch(
              expr = {
                AD_test =shapiro.test(prevPredict)
              },
              error = function(e) {
                AD_test = 0
              }
            )
          }else{
            AD_test = ad.test(prevPredict)
          }
          AD_pvalue=AD_test$p.value
          if (Skew_t[2]=='Median'){
            Median = median(prevPredict)
            Side_Left = prevPredict[prevPredict < Median]
            Side_Right = prevPredict[prevPredict >= Median]
            Dev_Left = sd(Side_Left)
            Dev_Right = sd(Side_Right)
            Return_Dev_Left = Median-as.numeric(Skew_t[3])*Dev_Left
            Return_Dev_Right = Median+as.numeric(Skew_t[3])*Dev_Right
            #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(prevPredict)-1), family="ST")
            #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
            ProbabilidadeTmedia = pst(0.0, xi=Median, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 1)
            Prob_Left = pst(0.0, xi=Return_Dev_Left, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 1)
            Prob_Right = pst(0.0, xi=Return_Dev_Right, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE,method = 1)
          }
        },
        error = function(e) {
          # Código a ser executado se ocorrer um erro
          ProbabilidadeTmedia=0.0
          xi=0.0
          omega=0.0
          alpha=0.0
          nu=0.0
          KS_test = ks.test(prevPredict,'pnorm')
          KS_pvalue=KS_test$p.value
          if (length(prevPredict) <= 7) {
            tryCatch(
              expr = {
                AD_test =shapiro.test(prevPredict)
              },
              error = function(e) {
                AD_test = 0
              }
            )
          }else{
            AD_test = ad.test(prevPredict)
          }
          AD_pvalue=AD_test$p.value
          ativos_fora[length(ativos_fora)+1]=ativo
          Dev_Left=0
          Dev_Right=0
          Prob_Left=0
          Prob_Right=0

        },
        warning = function(w) {
          # (Opcional) Código a ser executado se ocorrer um aviso (warning)
        },
        finally = {
          # (Opcional) Código a ser executado sempre, independentemente de erro ou aviso
        }
      )
    }
    #ProbabilidadeTmedia =pt(mean(prev),
    #                    df=length(prev)-1,lower.tail=TRUE)

    # Resultados das Probabilidades

    ResProbPosNNetPredict[1,k]= ProbPos
    ResProbTPosNNetPredict[1,k]= ProbabilidadeTmedia

    ####### Carteira Particular#################
    # fed forward
    #camadaEntradaPredict = as.matrix(entradasPredict)
    #somaSinapse0Predict = camadaEntradaPredict %*% pesos0
    #camadaOcultaPredict = sigmoide(somaSinapse0Predict)


    #somaSinapse1Predict = camadaOcultaPredict %*% pesos1
    #camadaSaidaPredict = sigmoide(somaSinapse1Predict)
    ################################################################################

    ####### --- Bloco de Predição (Garantido que o R vai ler agora) --- ############
    #for (ep in 1:epochs) {
    #camadaEntradaPredict = as.matrix(entradasPredict)

    # Forward Pass da Predição
    #A_pred <- vector("list", num_layers) # Usando nome limpo para não misturar com o 'A' do treino
    #A_pred[[1]] <- camadaEntradaPredict[, 2:ncol(camadaEntradaPredict)]

    #for (i in 1:(num_layers - 1)) {
    # net_pred <- sweep(A_pred[[i]] %*% W[[i]], 2, B[[i]], "+")

    #if (i == (num_layers - 1)) {
    #  if (Activation_F_Out == 'Linear') {
    #    A_pred[[i+1]] <- net_pred
    #  } else {
    #    A_pred[[i+1]] <- Activation_Fun(net_pred)
    #  }
    #} else {
    #  A_pred[[i+1]] <- Activation_Fun(net_pred)
    #}
    #}

    # Cálculo do Erro de Predição
    #camadaSaidaPredict <- A_pred[[num_layers]]
    #}

    if ((Order_Only='Yes')==TRUE){
      camadaSaidaPredict=camadaEntradaPredict
    }
    KS_test = ks.test(camadaSaidaPredict,'pnorm')
    KS_pvalue=KS_test$p.value
    if (length(camadaSaidaPredict) <= 7) {
      tryCatch(
        expr = {
          AD_test =shapiro.test(camadaSaidaPredict)
        },
        error = function(e) {
          AD_test = 0
        }
      )
    }else{
      AD_test = ad.test(camadaSaidaPredict)
    }
    AD_pvalue=AD_test$p.value


    hist(camadaSaidaPredict,
         main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
                      xnames= nome),
         xlab = paste("Retorno Excedente sobre", xnames = "RM"))
    mean(camadaSaidaPredict)
    median(camadaSaidaPredict)
    sd(camadaSaidaPredict)
    std.error= function(x) {
      sd(x)/sqrt(length(x))
    }
    se = std.error(camadaSaidaPredict)
    # Probabildiade Normal
    pnorm(q = 0.01, # probabilidade do retorno ser superior a 0.1%
          mean = mean(camadaSaidaPredict), # media
          sd = sd(camadaSaidaPredict), # desvio-pedrao
          lower.tail = FALSE) # Calcula P[X >x]
    ## Probabilidade com curtorese (Lambda > 3) = Probabilidade t Student
    library(moments)
    #png(file = "leptokurtic.png")
    ku=kurtosis(camadaSaidaPredict, na.rm = TRUE)
    #if ((ku>3)==TRUE) {
    ku1=round(ku,2)
    print(paste("Kurtosis:", ku1))
    #print(ku)
    #}
    t = 3.373 # p =0.1
    q = -0.4
    ProbtStudentPredict=(q-mean(camadaSaidaPredict))/(std.error(camadaSaidaPredict))
    #ProbtStudent
    ProbStudentPredict= t*(std.error(camadaSaidaPredict)) + mean(camadaSaidaPredict)
    #ProbStudent

    # Calculo da probabilidade excesso de retorno >0 c/ deslocamento da curva T
    #ProbabilidadeTmedia =pt(mean(camadaSaida),
    #                       df=length(camadaSaida)-1, lower.tail=FALSE)
    if (Asymmetry=="Negative") {
      if (mean(camadaSaidaPredict)>0) {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(camadaSaidaPredict)-1,ncp = se,
                                lower.tail=FALSE)
        print(paste("Right asymmetric density (Negative)"))
      } else {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(camadaSaidaPredict)-1,ncp = -se,
                                lower.tail=FALSE)
        print(paste("Left asymmetric density (Positive)"))
      }
    }else {if(Asymmetry=='Positive'){
      if (mean(camadaSaidaPredict)>0) {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(camadaSaidaPredict)-1,ncp = -se,
                                lower.tail=FALSE)
        print(paste("Right asymmetric density (Negative)"))
      } else {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(camadaSaidaPredict)-1,ncp = se,
                                lower.tail=FALSE)
        print(paste("Left asymmetric density (Positive)"))
      }
    }}
    if (Skew_t[1]=='Yes'){
      tryCatch(
        expr = {
          # Código principal a ser executado
          modelo_ajustado<- selm(camadaSaidaPredict ~1, family='ST')
          dist_sec <- extractSECdistr(modelo_ajustado)
          xi=dist_sec@dp[1]
          omega=dist_sec@dp[2]
          alpha=dist_sec@dp[3]
          nu=dist_sec@dp[4]
          Resultados_Curtose=kurtosis(camadaSaidaPredict)
          Resultados_Assim=skewness(camadaSaidaPredict)
          Media=mean(camadaSaidaPredict)
          Desvio=stdev(camadaSaidaPredict)
          KS_test = ks.test(camadaSaidaPredict,'pnorm')
          KS_pvalue=KS_test$p.value
          if (length(camadaSaidaPredict) <= 7) {
            tryCatch(
              expr = {
                AD_test =shapiro.test(camadaSaidaPredict)
              },
              error = function(e) {
                AD_test = 0
              }
            )
          }else{
            AD_test = ad.test(camadaSaidaPredict)
          }
          AD_pvalue=AD_test$p.value
          if(Skew_t[2]=='Median'){
            Median = median(CamadaSaidaPredict)
            Side_Left = CamadaSaidaPredict[CamadaSaidaPredict < Median]
            Side_Right = CamadaSaidaPredict[CamadaSaidaPredict >= Median]
            Dev_Left = sd(Side_Left)
            Dev_Right = sd(Side_Right)
            Return_Dev_Left = Median-as.numeric(Skew_t[3])*Dev_Left
            Return_Dev_Right = Median+as.numeric(Skew_t[3])*Dev_Right
            #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(camadaSaidaPredict)-1), family="ST")
            #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
            ProbabilidadeTmedia = pst(0.0, xi=Median, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE)
            Prob_Left = pst(0.0, xi=Return_Dev_Left, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE)
            Prob_Right = pst(0.0, xi=Return_Dev_Right, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE)
          }
          if(Skew_t[2]=='xi'){
            Prob_esquerda = pst(xi, xi=xi, omega=omega, alpha=alpha, nu=nu)
            Prob_direita = pst(xi, xi=xi, omega=omega, alpha=alpha, nu=nu, lower.tail=FALSE)
            integral = function(x){
              meu_dp = c(xi, omega, alpha)
              (x-xi)^2*sn::dst(x, xi=xi, omega=omega, alpha=alpha,nu=nu)
            }
            integral_LE=integrate(integral, lower=-Inf, upper=xi, rel.tol=1e-4)$value
            integral_LD=integrate(integral, lower=xi, upper=Inf)$value
            variancia_esquerda = integral_LE/Prob_esquerda
            variancia_direita = integral_LD/Prob_direita
            Dev_Left = sqrt(variancia_esquerda)
            Dev_Right = sqrt(variancia_direita)
            Return_Dev_Left = xi-as.numeric(Skew_t[3])*Dev_Left
            Return_Dev_Right = xi+as.numeric(Skew_t[3])*Dev_Right
            #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(camadaSaidaPredict)-1), family="ST")
            #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
            ProbabilidadeTmedia = pst(0.0, xi=xi, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE)
            Prob_Left = pst(0.0, xi=Return_Dev_Left, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE)
            Prob_Right = pst(0.0, xi=Return_Dev_Right, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE)
          }


        },
        error = function(e) {
          # Código a ser executado se ocorrer um erro
          ProbabilidadeTmedia=0.0
          xi=0.0
          omega=0.0
          alpha=0.0
          nu=0.0
          Dev_Left=0
          Dev_Right=0
          Prob_Left=0
          Prob_Right=0
          KS_test = ks.test(camadaSaidaPredict,'pnorm')
          KS_pvalue=KS_test$p.value
          if (length(camadaSaidaPredict) <= 7) {
            tryCatch(
              expr = {
                AD_test =shapiro.test(camadaSaidaPredict)
              },
              error = function(e) {
                AD_test = 0
              }
            )
          }else{
            AD_test = ad.test(camadaSaidaPredict)
          }
          AD_pvalue=AD_test$p.value
          ativos_fora[length(ativos_fora)+1]=ativo

        },
        warning = function(w) {
          # (Opcional) Código a ser executado se ocorrer um aviso (warning)
        },
        finally = {
          # (Opcional) Código a ser executado sempre, independentemente de erro ou aviso
        }
      )



    }
    #ProbabilidadeTmedia =pt(mean(camadaSaida),
    #                 df=length(camadaSaida)-1, lower.tail = TRUE)
    if (Skew_t[1]=='Yes'){
      print(paste("Excess Return Probability:", round(ProbabilidadeTmedia_1,2),"%", sep=' '))
    } else  {
      print(paste("Excess Return Probability:", round(ProbabilidadeTmedia,2),"%", sep=' '))
    }
      # Processing monitoring

    if (ativo<(ncol(dados2))){
      cat(paste("___________________________________________________________________
           Starting ANNt ",ativo," of a total of ",ncol(dados2)-1, " assets: ",colnames(dados2[ativo+1]),".
           Estimated total processing time: ", round(Tempo-Fator_Tempo*(ativo-1),2), Unidade,"
", sep=""))
    } else{
      cat(paste("___________________________________________________________________
                   ANNt for all assets concluded
___________________________________________________________________
"))
    }


    Probabilidadest = NULL
    for (l in 1:nlinhasPredict){
      Probabilidadest[l] = pt(camadaSaidaPredict[l],
                              df=length(camadaSaidaPredict)-1, lower.tail=FALSE)
    }

    # Plot benchmark e a rede neural artificial no teste
    library(data.table)
    DataPredict <- as.data.table(camadaSaidaPredict, keep.rownames = TRUE)
    DataPredict <- as.data.frame(DataPredict)

    matplot(cbind(camadaSaidaPredict, saidasPredict), type = "l", xaxt = "n",
            xlab = "Data",
            ylab = "Retorno",
            main = paste("Otimiza??o RNA Fase de Teste - Ativo", xname = nome))
    legend("topright", legend = c("RNA", nome), pch = 19,
           col = c("black", "red"))
    axis(1, 1:nlinhasPredict, DataPredict$rn)

    ## Sinal Maior que a media
    #sinal = NULLnlinhasPredict
    #for (i in 1:nlinhasPredict) {
    #  if(camadaSaidaPredict[i] > mean(camadaSaidaPredict)){
    #    sinal[i] = 1
    #  }
    #  else {
    #    sinal[i] = -1
    #  }
    #}
    #sinal

    ## Sinal POSITIVO
    sinal = NULL
    for (i in 1:nlinhasPredict) {
      if(camadaSaidaPredict[i] > 0){
        sinal[i] = 1
      }
      else {
        sinal[i] = -1
      }
    }
    sinal

    ### Probabilidades
    RetornoPos <- sum(sinal ==1)
    RetornoNeg <- sum(sinal == -1)
    ProbPos <- RetornoPos/(RetornoPos + RetornoNeg)
    #ProbPos

    # Resultados das Probabilidades

    ResProbPosPredict[1,k]= ProbPos
    ResProbTPosPredict[1,k]= ProbabilidadeTmedia

    Resultados_Assim_Curtose_Training[1,k]=ProbabilidadeTmedia_1
    Resultados_Assim_Curtose_Training[2,k]=mean(camadaSaida)
    Resultados_Assim_Curtose_Training[3,k]=median(camadaSaida)
    Resultados_Assim_Curtose_Training[4,k]=sd(camadaSaida)
    Resultados_Assim_Curtose_Training[5,k]=kurtosis(camadaSaida)
    Resultados_Assim_Curtose_Training[6,k]=skewness(camadaSaida)
    Resultados_Assim_Curtose_Training[7,k]=min(camadaSaida)
    Resultados_Assim_Curtose_Training[8,k]=max(camadaSaida)
    Resultados_Assim_Curtose_Training[9,k]=xi_1
    Resultados_Assim_Curtose_Training[10,k]=omega_1
    Resultados_Assim_Curtose_Training[11,k]=alpha_1
    Resultados_Assim_Curtose_Training[12,k]=nu_1
    Resultados_Assim_Curtose_Training[13,k]=KS_pvalue_1
    Resultados_Assim_Curtose_Training[14,k]=AD_pvalue_1
    Resultados_Assim_Curtose_Training[15,k]= Dev_Left_1
    Resultados_Assim_Curtose_Training[16,k]= Dev_Right_1
    Resultados_Assim_Curtose_Training[17,k]= Prob_Left_1
    Resultados_Assim_Curtose_Training[18,k]= Prob_Right_1


    Resultados_Assim_Curtose_Testing[1,k]=ProbabilidadeTmedia
    Resultados_Assim_Curtose_Testing[2,k]=mean(camadaSaidaPredict)
    Resultados_Assim_Curtose_Testing[3,k]=median(camadaSaidaPredict)
    Resultados_Assim_Curtose_Testing[4,k]=sd(camadaSaidaPredict)
    Resultados_Assim_Curtose_Testing[5,k]=kurtosis(camadaSaidaPredict)
    Resultados_Assim_Curtose_Testing[6,k]=skewness(camadaSaidaPredict)
    Resultados_Assim_Curtose_Testing[7,k]=min(camadaSaidaPredict)
    Resultados_Assim_Curtose_Testing[8,k]=max(camadaSaidaPredict)
    Resultados_Assim_Curtose_Testing[9,k]=xi
    Resultados_Assim_Curtose_Testing[10,k]=omega
    Resultados_Assim_Curtose_Testing[11,k]=alpha
    Resultados_Assim_Curtose_Testing[12,k]=nu
    Resultados_Assim_Curtose_Testing[13,k]=KS_pvalue
    Resultados_Assim_Curtose_Testing[14,k]=AD_pvalue
    Resultados_Assim_Curtose_Testing[15,k]= Dev_Left
    Resultados_Assim_Curtose_Testing[16,k]= Dev_Right
    Resultados_Assim_Curtose_Testing[17,k]= Prob_Left
    Resultados_Assim_Curtose_Testing[18,k]= Prob_Right

    dev.off() ### Salvando gr?ficos do Ativo dentro Loop

    #############################Fim Amostra de Teste###########################


  }




  ################################################################################
  ############################Fim do envelope#####################################
  ################################################################################




  ResProbPosNNet   #Resultado Probabilidade Sinal Positivo - NeuralNet Train
  ResProbPos       #Resultado Probabilidade Sinal Positivo - RNA Particular Train
  ResProbTPosNNet  #Resultado Probabilidade Distribui??o t - NeuralNet Train
  ResProbTPos      #Resultado Probabilidade Distribui??o t - RNA Particular Train

  ResProbPosNNetPredict #Resultado Probabilidade Sinal Positivo - NeuralNet Test
  ResProbPosPredict #Resultado Probabilidade Sinal Positivo - RNA Particular Test
  ResProbTPosNNetPredict #Resultado Probabilidade Distribui??o t - NeuralNet Test
  ResProbTPosPredict #Resultado Probabilidade Distribui??o t - RNA Particular Test
  ###
  write_xlsx (ResProbPosNNet,"ResProbPosNNet.xlsx")
  write_xlsx (ResProbPos,"ResProbPos.xlsx")
  write_xlsx (ResProbTPosNNet, "ResProbTPosNNet.xlsx")
  write_xlsx (ResProbTPos, "ResProbTPos.xlsx")

  write_xlsx (ResProbPosNNetPredict, "ResProbPosNNetPredict.xlsx")
  write_xlsx (ResProbPosPredict, "ResProbPosPredict.xlsx")
  write_xlsx (ResProbTPosNNetPredict, "ResProbTPosNNetPredict.xlsx")
  write_xlsx (ResProbTPosPredict, "ResProbTPosPredict.xlsx")


  ################################################################################
  ################################################################################
  ########################          AMOSTRAS          ############################
  ################################################################################
  ################################################################################

  ## Train NNet
  order(as.matrix(ResProbPosNNet))
  nomes = colnames(ResProbPosNNet)
  prob = t(ResProbPosNNet)
  Res=as.matrix(ResProbPosNNet)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteNNet= matrix(data=ncol(ResProbPosNNet):1, nrow=1, ncol=ncol(ResProbPosNNet))
  colnames(TesteNNet)= Test$Nomes
  TesteNNet[1,]=Test$Prob

  ###

  order(as.matrix(ResProbPos))
  nomes = colnames(ResProbPos)
  prob = t(ResProbPos)
  Res=as.matrix(ResProbPos)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TestePos= matrix(data=ncol(ResProbPos):1, nrow=1, ncol=ncol(ResProbPos))
  colnames(TestePos)= Test$Nomes
  TestePos[1,]=Test$Prob

  ###

  order(as.matrix(ResProbTPosNNet))
  nomes = colnames(ResProbTPosNNet)
  prob = t(ResProbTPosNNet)
  Res=as.matrix(ResProbTPosNNet)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTNNet= matrix(data=ncol(ResProbTPosNNet):1, nrow=1, ncol=ncol(ResProbTPosNNet))
  colnames(TesteTNNet)= Test$Nomes
  TesteTNNet[1,]=Test$Prob

  ###

  order(as.matrix(ResProbTPos))
  nomes = colnames(ResProbTPos)
  prob = t(ResProbTPos)
  Res=as.matrix(ResProbTPos)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTPos= matrix(data=ncol(ResProbTPos):1, nrow=1, ncol=ncol(ResProbTPos))
  colnames(TesteTPos)= Test$Nomes
  TesteTPos[1,]=Test$Prob


  ###
  ### Test

  order(as.matrix(ResProbPosNNetPredict))
  nomes = colnames(ResProbPosNNetPredict)
  prob = t(ResProbPosNNetPredict)
  Res=as.matrix(ResProbPosNNetPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteNNetPredict= matrix(data=ncol(ResProbPosNNetPredict):1, nrow=1, ncol=ncol(ResProbPosNNetPredict))
  colnames(TesteNNetPredict)= Test$Nomes
  TesteNNetPredict[1,]=Test$Prob

  ###


  order(as.matrix(ResProbPosPredict))
  nomes = colnames(ResProbPosPredict)
  prob = t(ResProbPosPredict)
  Res=as.matrix(ResProbPosPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TestePosPredict= matrix(data=ncol(ResProbPosPredict):1, nrow=1, ncol=ncol(ResProbPosPredict))
  colnames(TestePosPredict)= Test$Nomes
  TestePosPredict[1,]=Test$Prob

  ###


  order(as.matrix(ResProbTPosNNetPredict))
  nomes = colnames(ResProbTPosNNetPredict)
  prob = t(ResProbTPosNNetPredict)
  Res=as.matrix(ResProbTPosNNetPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTNNetPredict= matrix(data=ncol(ResProbTPosNNetPredict):1, nrow=1, ncol=ncol(ResProbTPosNNetPredict))
  colnames(TesteTNNetPredict)= Test$Nomes
  TesteTNNetPredict[1,]=Test$Prob

  ###
  #### RNA-t Particular para armazenar em T8

  order(as.matrix(ResProbTPosPredict))
  nomes = colnames(ResProbTPosPredict)
  prob = t(ResProbTPosPredict)
  Res=as.matrix(ResProbTPosPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTPosPredict= matrix(data=ncol(ResProbTPosPredict):1, nrow=1, ncol=ncol(ResProbTPosNNetPredict))
  colnames(TesteTPosPredict)= Test$Nomes
  TesteTPosPredict[1,]=Test$Prob

  ###############################################################################
  # Statistic Summary
  ## Train NNet
  order(as.matrix(Resultados_Assim_Curtose_Training[1,]))
  nomes = colnames(Resultados_Assim_Curtose_Training)
  prob = data.frame(t(Resultados_Assim_Curtose_Training))
  Summary_ANNt_Training = arrange(prob, desc(Probability))
  ## Test NNet
  order(as.matrix(Resultados_Assim_Curtose_Testing[1,]))
  nomes = colnames(Resultados_Assim_Curtose_Testing)
  prob = data.frame(t(Resultados_Assim_Curtose_Testing))
  Summary_ANNt_Testing = arrange(prob, desc(Probability))



  ###
  #T1=data.frame(TesteNNet)   #Resultado Sinal Positivo - NeuralNet Ordenada Train
  #T2=data.frame(TesteTNNet)  #Resultado Sinal Positivo - RNA Particular Ord Train
  #T3=data.frame(TestePos)    #Resultado Prob. Dist t - NeuralNet Ordenada Train
  #T4=data.frame(TesteTPos)   #Resultado Proba. Dist t - RNA Particular Ord Train
  #T5=data.frame(TesteNNetPredict) #Res Prob. Sinal Positivo - NeuralNet Ord Test
  #T6=data.frame(TestePosPredict) #Resultado Prob. Dist t - NeuralNet Ordenada Test
  #T7=data.frame(TesteTNNetPredict) #Res Prob. Sinal Positivo - RNA Part Ord Test
  #T8=data.frame(TesteTPosPredict) #Res Prob. Dist t - RNA Particular Ordenada Test

  ###
  T1=data.frame(TesteNNet)   #Resultado Sinal Positivo - NeuralNet Ordenada Train
  T2=data.frame(TesteTNNet)  #Resultado Prob. Dist t - NeuralNet Ordenada Train
  T3=data.frame(TestePos)    #Resultado Sinal Positivo - RNA Particular Ord Train
  T4=data.frame(TesteTPos)   #Resultado Proba. Dist t - RNA Particular Ord Train
  T5=data.frame(TesteNNetPredict) #Res Prob. Sinal Positivo - NeuralNet Ord Test
  T6=data.frame(TestePosPredict) #Res Prob. Sinal Positivo - RNA Part Ord Test
  T7=data.frame(TesteTNNetPredict) #Resultado Prob. Dist t - NeuralNet Ordenada Test
  T8=data.frame(TesteTPosPredict) #Res Prob. Dist t - RNA Particular Ordenada Test


  #View(T8)

  Assets_ANNt_Order = T8
  rownames(Assets_ANNt_Order)='Probability_Testing'
  View(Assets_ANNt_Order)
  View(Summary_ANNt_Testing)
  print(Assets_ANNt_Order)
  save(Assets_ANNt_Order,file='~/Assets_ANNt_Order.rda')
  nome_asset= str_replace(Final_Date_Testing,"-","_")
  nome_asset= str_replace(nome_asset,"-","_")
  nome_asset= str_replace(nome_asset,":","_")
  nome_asset= str_replace(nome_asset,":","_")
  nome_Asset_order=paste("~/Assets_ANNt_Order_",nome_asset,".xlsx", sep="")
  nome_Summary_ANNt_Training=paste("~/Summary_ANNt_Training_",nome_asset,".xlsx", sep="")
  nome_Summary_ANNt_Testing=paste("~/Summary_ANNt_Testing_",nome_asset,".xlsx", sep="")
  save(Initial_Date_Training, file='~/Initial_Date_Training.rda')
  save(Final_Date_Training, file='~/Final_Date_Training.rda')

  D = which(rownames(scenario.set)==Final_Date_Training)
  Initial_Date_Testing = rownames(as.data.frame(scenario.set)[D+1,])
  #Initial_Date_Testing=rownames(as.data.frame(entradasPredict)[1,])
  save(Initial_Date_Testing, file='~/Initial_Date_Testing.rda')
  save(Final_Date_Testing, file='~/Final_Date_Testing.rda')
  save(Hidden, file='~/Hidden.rda')
  save(Stepmax, file='~/Stepmax.rda')
  save(I_dataPredict,file='~/I_dataPredict.rda')
  save(F_dataPredict,file='~/F_dataPredict.rda')

  Summary_ANNt_Training[is.na(Summary_ANNt_Training)| sapply(Summary_ANNt_Training, is.nan)] <- 0
  Summary_ANNt_Testing[is.na(Summary_ANNt_Testing)| sapply(Summary_ANNt_Testing, is.nan)] <- 0

  save(Summary_ANNt_Training,file='~/Summary_ANNt_Training.rda')
  save(Summary_ANNt_Testing,file='~/Summary_ANNt_Testing.rda')
  save(ResProbTPosPredict,file='~/ResProbTPosPredict.rda')
  save(T1,file='~/T1.rda')
  save(T2,file='~/T2.rda')
  save(T3,file='~/T3.rda')
  save(T4,file='~/T4.rda')
  save(T5,file='~/T5.rda')
  save(T6,file='~/T6.rda')
  save(T7,file='~/T7.rda')
  save(T8,file='~/T8.rda')
  save(ativos_fora, file='~/ativos_fora.rda')

  write_xlsx(Assets_ANNt_Order, nome_Asset_order)

  Summary_ANNt_Training_xls=data.frame(rownames(Summary_ANNt_Training),Summary_ANNt_Training)
  names2=colnames(Summary_ANNt_Training_xls)
  names2[1]='Ticker'
  colnames(Summary_ANNt_Training_xls)<-names2
  write_xlsx(Summary_ANNt_Training_xls, nome_Summary_ANNt_Training)

  Summary_ANNt_Testing_xls=data.frame(rownames(Summary_ANNt_Testing),Summary_ANNt_Testing)
  names2=colnames(Summary_ANNt_Testing_xls)
  names2[1]='Ticker'
  colnames(Summary_ANNt_Testing_xls)<-names2
  write_xlsx(Summary_ANNt_Testing_xls, nome_Summary_ANNt_Testing)
  ###############################



}
