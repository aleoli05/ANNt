#' classify assets by the probability of return exceeding a RM with LSTM
#' Use Long Short Term RNN and t-distribution, the number of input neurons is the number of import assets


#' @param Inicial_Date Series start Date (Must be 7 periods greater than the analyzed series)
#' @param Date_Training Series finish training date
#' @param Final_Date_Training Series end Date (If '' is the System Date)
#' @param Hidden Number of hidden neurons (If '' is the length series)
#' @param Stepmax Number of replications per asset to train the ANN
#' @param Asymmetry "Negative" or "Positive". Shifts the probability of the return being greater than the proxy to the right or left, "Negative" or "Positive". Default is to the right, "Negative"
#' @param View_Metrics "True" or "False" for view realtime plot of training metrics
#' @param Verbose Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch)
#' @param Plot Plot return´s frequency histogram
#' @param Skew_t Incorporate skew parameter in the probability: "Yes" or "No". Default is "No".
#' @author Alexandre Silva de Oliveira

#' @examples
#' Initial_Date_Training <-c('2018-01-11')
#' Final_Date_Training <- c('2022-12-29')
#' Final_Date_Testing <-c('')
#' Hidden <- 5
#' Stepmax <- 2500
#' LSTMt_order ('2018-01-11','2022-12-29','', 5, 2500)
#' # Estimated processing time 30 minutes per asset
#'
#Portfolio optimization system using Artificial Neural Networks.
#ANNt uses a Perceptron architecture, with a hidden layer of size equal to the
#number of information in the input neurons.     Classifies the output neurons
#of assets with leptokurtic distribution, which is more appropriate for financial
#data.

#' @export
LSTMt_order <- function(Initial_Date_Training, Final_Date_Training,
                        Final_Date_Testing, Hidden, Stepmax, Asymmetry='Negative',
                        View_Metrics=TRUE,Verbose=1, Plot='Yes', Skew_t='No') {
  ## Convers?o das variaveis
  # Excesso do retorno em relacao ao RM
library("quantmod")
  if (!require("sn", character.only = TRUE)) {
    install.packages("sn", dependencies = TRUE)
  }
  library("sn", character.only = TRUE)
  Hidden_ANNt=5
  Stepmax_ANNt=200
print('Starting LSTMt_order Command')

  load("~/scenario.set.rda") # Carrega objeto scenario.set
  load("~/Initial_Date.rda") # Carrega objeto scenario.set
  #load("~/tickers.rda") # Carrega objeto scenario.set
  load("~/Signal_Sharpe.rda") # Carrega objeto scenario.set
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

 # y1 is the number of hidden, case the ANNt_Oliveira_Ceretta went used
 if(Hidden=='hidden'){
  load('~/x2.rda')
   Hidden=x2
 }
 # y2 is the number of Stepmax, case the ANNt_Oliveira_Ceretta went use
 if(Stepmax=='stepmax'){
   load('~/x3.rda')
   Stepmax=x3
 }
 if (Hidden==''){
   Cont1=which(rownames(scenario.set)==Final_Date_Training)-5
 } else{
   Cont1=Hidden
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
           Starting LSTMt 1 of a total of ",ncol(dados)-1, " assets: ",colnames(dados2[2]), ".
", sep=""))

  ncoldados <- ncol(dados)
  nAtivos = ncol(dados)
  for (i in 2:ncoldados) {
    dados[,i] <- dados[,i] - dados[,1]
  }

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Train
  #### LSTM por Sinal
  ResProbPosLSTMt = matrix(ncol=nAtivos-1,nrow=1)
  ResProbPosLSTMt = data.frame(ResProbPosLSTMt)
  colnames(ResProbPosLSTMt)=tickers[-1]
  ResProbPosLSTMt

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Train
  #### LSTM por Probabilidade t de Student
  ResProbTPosLSTMt = matrix(ncol=nAtivos-1,nrow=1)
  ResProbTPosLSTMt = data.frame(ResProbTPosLSTMt)
  colnames(ResProbTPosLSTMt)=tickers[-1]
  ResProbTPosLSTMt

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
  ResProbPosLSTMtPredict = matrix(ncol=nAtivos-1,nrow=1)
  ResProbPosLSTMtPredict = data.frame(ResProbPosLSTMtPredict)
  colnames(ResProbPosLSTMtPredict)=tickers[-1]
  ResProbPosLSTMtPredict

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Probabilidade t de Student
  ResProbTPosLSTMtPredict = matrix(ncol=nAtivos-1,nrow=1)
  ResProbTPosLSTMtPredict = data.frame(ResProbTPosLSTMtPredict)
  colnames(ResProbTPosLSTMtPredict)=tickers[-1]
  ResProbTPosLSTMtPredict

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
  Resultados_Assim_Curtose = matrix(ncol=nAtivos-1,nrow=8)
  Resultados_Assim_Curtose = data.frame(Resultados_Assim_Curtose)
  colnames(Resultados_Assim_Curtose)=tickers[-1]
  rownames(Resultados_Assim_Curtose) = c('Probability','Mean','Median','Stand. Dev.',
                                         'Kurtosis','Skewness','Minimum','Maximum')
  Resultados_Assim_Curtose


  ################################################################################
  #####################################Envelope###################################
  ################################################################################

  for (k in 1:(nAtivos-1)){
    ativo = k+1
    #Envelope


    # Calculo das defasagens para cada ativo


    dat_r <- data.frame(dados2[ativo], Lag(dados[,1],1))
    colnames(dat_r)[2]="RM"

    defasagem = 5
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
    Fim_data = as.Date(Final_Date_Training)

    I_data = which(rownames(dat_r)==Inicio_data)
    F_data = which(rownames(dat_r)==Fim_data)
    entradas = as.matrix(dat_r[I_data:F_data,])
    saidas = as.matrix(dat_r[(I_data+1):(F_data+1),1])


    #####
    nlinhas <- nrow(entradas)
    ncolunas <- ncol(entradas)

    #############################################################################
    ##Pacote LSTM()
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

##LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
    No = 'No_execute'
  if(No =='Yes_execute')  {
        if (Hidden==''){
      Hidden=nlinhas
    }

    epocas = Stepmax_ANNt
    # Fun??o Sigmoide
    sigmoide = function(soma) {
      #return (1/ (1+exp(-soma)))
      #Fun??o Tangente Hiperb?lica
      #return (1-tanh(soma))
      return (tanh(soma))
    }
    colnames(entradas)[1]= "ATIVO"
    nn= LSTM( ATIVO ~ RM + V3 + V4 + V5 + V6 + V7, data=entradas,
                   hidden = Hidden, act.fct = "tanh", threshold = 0.1,
                   stepmax=epocas)
    # Plotagem da RNA

    if(Hidden %% 2 == 0) {
      escondida = Hidden
    } else {if(Hidden >15){
        escondida =Hidden+1
        } else {escondida = Hidden}}
    nnplot= LSTM( ATIVO ~ RM + V3 + V4 + V5 + V6 + V7, data=entradas,
                       hidden = escondida, act.fct = "tanh", threshold = 0.1,
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
     plot(nnplot, main ="Artificial Neural Network")

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
    }
  Ticket = paste(colnames(dados2[ativo]))
  Hidden_dim = Hidden
  Num_epochs = Stepmax
  LSTM_RNN (Tickers=Ticket, Lookback=8, Initial_Date_Training=Initial_Date_Training,
                       Final_Date_Training=Final_Date_Training,
                       Final_Date_Testing=Final_Date_Testing,
                       Input_dim = 1,
                       Hidden_dim = Hidden,
                       Num_layers = 2,
                       Output_dim = 1,
                       Num_epochs = Stepmax,
                       Import ='No',
                       Metric = 'Return',
                       Plot = 'No', View_Metrics=View_Metrics, Verbose=Verbose)
  load('~/y_train_pred_rescaled.rda')
  load('~/y_test_pred_rescaled.rda')
##LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
    ## Previs?o
    #prev = predict(nn, entradas)
    prev = y_train_pred_rescaled

    nome = colnames(entradas)[1]
if (Plot=='Yes'){
    plot(as.vector(entradas[,1]), type="l", col= "red",
         main = paste("Training Sample Returns - Asset", xnames = nome))
    legend("topright", legend = c("RLSTMt", nome), pch = 19,
           col = c("black", "red"))
    lines(prev)

    hist(prev, main = paste("Training Histogram Step RLSTMt Asset",
                            xnames= nome),
         xlab = paste("Retorno Excedente sobre", xnames = "RM"))
    mean(prev)
}
    #Testes = compute(nn, entradas)
    Testes = y_train_pred_rescaled
   #Testes$net.result
    #png(ARQUIVO)
    #arquivo = colnames(entradas)[1]
    #arquivo = paste(arquivo,"HistTrain.pdf", sep="_")
    #ggsave(arquivo,p1)
    #pdf(file=arquivo, height = 8, width = 9)
    if(Plot=='Yes'){
    hist(Testes, main = paste("Predict Histogram ANN Asset",
                                         xnames= nome), xlab = paste("Excess Returns over",
                                                                     xnames = "RM"))
    }
    #hist(Testes$net.result, main = paste("Histograma Previs?es RNA Ativo",
     #                                    xnames= nome), xlab = paste("Retorno Excedente sobre",
      #                                                               xnames = "RM"))
    #dev.off()
    #arquivo <- str_replace(arquivo,"pdf","png")
    #png(filename=arquivo)
    #hist(Testes, main = paste("Histograma Train RNA Ativo",
     #                                    xnames= nome), xlab = paste("Retorno Excedente sobre",
      #                                                               xnames = "RM"))
    #hist(Testes$net.result, main = paste("Histograma Train RNA Ativo",
     #                                    xnames= nome), xlab = paste("Retorno Excedente sobre",
      #                                                               xnames = "RM"))
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
      #print(paste("Right asymmetric density (Negative)"))
    } else {
      ProbabilidadeTmedia =pt(0.0,
                              df=length(prev)-1,ncp = -se, lower.tail=FALSE)
      #print(paste("Left asymmetric density (Positive)"))
    }
    }else {if(Asymmetry=='Positive'){
      if (mean(prev)>0) {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(prev)-1,ncp = -se, lower.tail=FALSE)
        #print(paste("Right asymmetric density (Negative)"))
      } else {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(prev)-1,ncp = se, lower.tail=FALSE)
        #print(paste("Left asymmetric density (Positive)"))
      }
    }}
    if (Skew_t=='Yes'){
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
      #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(prev)-1), family="ST")
      #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
      ProbabilidadeTmedia = pst(0.0, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE)
        },
      error = function(e) {
        # Código a ser executado se ocorrer um erro
        ProbabilidadeTmedia=0.0
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
    #ProbabilidadeTmedia =pt(mean(prev),
    #                    df=length(prev)-1,lower.tail=TRUE)

    colnames(entradas)[1]=paste("(",ativo-1,") ",colnames(dados2[ativo]), sep="")
    colnames(entradas)[2]=paste(colnames(dados2[1]),"-1",sep="")
    colnames(entradas)[3]=paste(colnames(dados2[ativo]),"-1",sep="")
    colnames(entradas)[4]=paste(colnames(dados2[ativo]),"-2",sep="")
    colnames(entradas)[5]=paste(colnames(dados2[ativo]),"-3",sep="")
    colnames(entradas)[6]=paste(colnames(dados2[ativo]),"-4",sep="")
    colnames(entradas)[7]=paste(colnames(dados2[ativo]),"-5",sep="")

    View(y_train_pred_rescaled)

    # Resultados das Probabilidades

    ResProbPosLSTMt[1,k]= ProbPos
    ResProbTPosLSTMt[1,k]= ProbabilidadeTmedia
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
    pesos0 = matrix(runif(ncolunas*nlinhas, min = 0, max = 1), nrow = ncolunas,
                    ncol = Hidden_ANNt, byrow = T)
    pesos1 = matrix(runif(ncolunas*(nlinhas), min = 0, max = 1), nrow = Hidden_ANNt,
                    ncol = 1, byrow = T)
    options(warn=-1)
    ### pesos1 com Bias na camada oculta
    #pesos1 = matrix(runif(ncolunas*(nlinhas+1), min = 0, max = 1),
    #nrow = nlinhas+1, ncol = 1, byrow = T)
    #View(pesos0)

    # Gerando pesos iniciais aleat?rios PARAMETRIZADOS (Revisar)
    #repeat {
    #pesos0 = matrix(runif(ncolunas, min = 0, max = 1, sum(ncolunas)=1),
    #nrow = ncolunas+1, ncol = 1, byrow = T)
    #until
    #sum(pesos0)=1
    #}

    # Gerando pesos iniciais carteira ing?nua (pesos iguais)
    #pesos0 = matrix(1/ncol(entradas), nrow = ncolunas, ncol = nlinhas, byrow = T)
    #pesos1 = matrix(runif(ncolunas*nlinhas, min = -1, max = 1), nrow = nlinhas+1,
    #ncol = 1, byrow = T)
    #View(pesos0)


    # Fun??o Sigmoide
    sigmoide = function(soma) {
      #return (1/ (1+exp(-soma)))
      #Fun??o Tangente Hiperb?lica
      #return (1-tanh(soma))
      return (tanh(soma))
    }

    # Derivada da fun??o Sigmoide
    sigmoideDerivada = function(sig) {
      return (sig * (1-sig))
    }

    # Estimando o n?mero de ?pocas e a taxa de aprendizagem
    epocas = Stepmax_ANNt
    momento = 1
    taxaAprendizagem = 0.3
    #########################################
    for(j in 1:epocas) {
      # fed forward
      camadaEntrada = as.matrix(entradas)
      somaSinapse0 = camadaEntrada %*% pesos0
      camadaOculta = sigmoide(somaSinapse0)

      ### Introduzindo Bias na segunda camada
      #BIAS2 = NULL
      # for (i in 1:  nlinhas){
      #   BIAS2[i] <- 1
      #}
      #BIAS2 <- data.frame(BIAS2)
      #camadaOculta <- cbind(camadaOculta,BIAS2)
      #camadaOculta = as.matrix(camadaOculta)


      somaSinapse1 = camadaOculta %*% pesos1
      camadaSaida = sigmoide(somaSinapse1)

      # back forward
      #erroCamadaSaida = 1 - saidas - camadaSaida # M?xima diferen?a
      erroCamadaSaida = saidas - camadaSaida # M?nima diferen?a
      mediaAbsoluta = mean(abs(erroCamadaSaida))

      if (ativo==ncol(dados)){
      #print(paste('Error:', mediaAbsoluta))
      }

      derivadaSaida = sigmoideDerivada(camadaSaida)
      deltaSaida = erroCamadaSaida * derivadaSaida

      #deltaSaidaXPeso = deltaSaida %*% pesos1 # Matrizes com dimens?es diferentes,
      #por isso da erro, ? preciso transpor a matriz pesos1
      pesos1Transposta = t(pesos1)
      deltaSaidaXPeso = deltaSaida %*% pesos1Transposta
      deltaCamadaOculta = deltaSaidaXPeso * sigmoideDerivada(camadaOculta)

      # (backpropagation)
      # Atualiza??o dos pesos da camada de sa?da at? a oculta
      camadaOcultaTransposta = t(camadaOculta)
      pesosNovo1 = camadaOcultaTransposta %*% deltaSaida
      pesos1 = (pesos1 * momento) + (pesosNovo1 * taxaAprendizagem)

      # Atualiza??o dos pesos da camada oculta at? a de entrada
      camadaEntradaTransposta = t(camadaEntrada)
      pesosNovo0 = camadaEntradaTransposta %*% deltaCamadaOculta
      pesos0 = (pesos0 * momento) + (pesosNovo0[,-(nlinhas+1)] * taxaAprendizagem)

    }

    hist(camadaSaida,
         main = paste("Histograma Previs?es RNA Fase de Treinamento - Ativo",
                      xnames= nome),
         xlab = paste("Retorno Excedente sobre", xnames = "RM"))
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
          if (mean(camadaSaida)>0) {
            ProbabilidadeTmedia =pt(0.0,
                                    df=length(camadaSaida)-1,ncp = se, lower.tail=FALSE)
           # cat("Right asymmetric density (Negative)")
          } else {
            ProbabilidadeTmedia =pt(0.0,
                                    df=length(camadaSaida)-1,ncp = -se, lower.tail=FALSE)
            #cat("Left asymmetric density (Positive)")
          }
    }else {if(Asymmetry=='Positive'){
      if (mean(camadaSaida)>0) {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(camadaSaida)-1,ncp = -se, lower.tail=FALSE)
        # cat("Right asymmetric density (Negative)")
      } else {
        ProbabilidadeTmedia =pt(0.0,
                                df=length(camadaSaida)-1,ncp = se, lower.tail=FALSE)
        #cat("Left asymmetric density (Positive)")
      }
    }}
    if (Skew_t=='Yes'){
      tryCatch(
        expr = {
      modelo_ajustado<- selm(camadaSaida ~1, family='ST')
      dist_sec <- extractSECdistr(modelo_ajustado)
      xi=dist_sec@dp[1]
      omega=dist_sec@dp[2]
      alpha=dist_sec@dp[3]
      nu=dist_sec@dp[4]
      Resultados_Curtose=kurtosis(camadaSaida)
      Resultados_Assim=skewness(camadaSaida)
      Media=mean(camadaSaida)
      Desvio=stdev(camadaSaida)
      #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(camadaSaida)-1), family="ST")
      #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
      ProbabilidadeTmedia = pst(0.0, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE)
        },
      error = function(e) {
        # Código a ser executado se ocorrer um erro
        ProbabilidadeTmedia=0.0
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
    if (Plot=='Yes'){
    matplot(cbind(camadaSaida, saidas), type = "l", xaxt = "n",
            xlab = "Data",
            ylab = "Retorno",
            main = paste("Otimiza??o RNA Fase de Treinamento - Ativo",
                         xname = nome))
    legend("topright", legend = c("RNA", nome), pch = 19, col = c("black", "red"))
    axis(1, 1:nlinhas, Data$rn)
    }

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

##LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
    ## Previs?o
    # prevPredict = predict(nn, entradasPredict)
    prevPredict = y_test_pred_rescaled
    nome = colnames(entradasPredict)[1]

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

    #TestesPredict = compute(nn, entradasPredict)


    #TestesPredict$net.result
    #hist(TestesPredict$net.result,
    #     main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
    #                  xnames= nome), xlab = paste("Retorno Excedente sobre",
     #                                             xnames = "RM"))
    #hist(TestesPredict,
     #    main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
      #                xnames= nome), xlab = paste("Retorno Excedente sobre",
       #                                           xnames = "RM"))
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
    if (Skew_t=='Yes'){
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
          #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(prevPredict)-1), family="ST")
          #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
          ProbabilidadeTmedia = pst(0.0, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE)
        },
        error = function(e) {
          # Código a ser executado se ocorrer um erro
          ProbabilidadeTmedia=0.0
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
    #ProbabilidadeTmedia =pt(mean(prev),
    #                    df=length(prev)-1,lower.tail=TRUE)

    # Resultados das Probabilidades

    ResProbPosLSTMtPredict[1,k]= ProbPos
    ResProbTPosLSTMtPredict[1,k]= ProbabilidadeTmedia

    ####### Carteira Particular#################
    # fed forward
    camadaEntradaPredict = as.matrix(entradasPredict)
    somaSinapse0Predict = camadaEntradaPredict %*% pesos0
    camadaOcultaPredict = sigmoide(somaSinapse0Predict)


    somaSinapse1Predict = camadaOcultaPredict %*% pesos1
    camadaSaidaPredict = sigmoide(somaSinapse1Predict)


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
      ku=kurtosis(camadaSaidaPredict)
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
    if (Skew_t=='Yes'){
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
          #dpst1 <- cp2dp(c(Media, Desvio, Resultados_Assim, length(camadaSaidaPredict)-1), family="ST")
          #ProbabilidadeTmedia = pst(0.0, dp=dpst1, lower.tail = FALSE)
          ProbabilidadeTmedia = pst(0.0, omega=omega, alpha=alpha, nu=nu, lower.tail = FALSE)
        },
        error = function(e) {
          # Código a ser executado se ocorrer um erro
          ProbabilidadeTmedia=0.0
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
    if (Plot=='Yes'){
    matplot(cbind(camadaSaidaPredict, saidasPredict), type = "l", xaxt = "n",
            xlab = "Data",
            ylab = "Retorno",
            main = paste("Otimizacao RNA Fase de Teste - Ativo", xname = nome))
    legend("topright", legend = c("RNA", nome), pch = 19,
           col = c("black", "red"))
    axis(1, 1:nlinhasPredict, DataPredict$rn)
    }
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

    Resultados_Assim_Curtose[1,k]=ProbabilidadeTmedia
    Resultados_Assim_Curtose[2,k]=mean(camadaSaidaPredict)
    Resultados_Assim_Curtose[3,k]=median(camadaSaidaPredict)
    Resultados_Assim_Curtose[4,k]=sd(camadaSaidaPredict)
    Resultados_Assim_Curtose[5,k]=kurtosis(camadaSaidaPredict)
    Resultados_Assim_Curtose[6,k]=skewness(camadaSaidaPredict)
    Resultados_Assim_Curtose[7,k]=min(camadaSaidaPredict)
    Resultados_Assim_Curtose[8,k]=max(camadaSaidaPredict)


    dev.off() ### Salvando gr?ficos do Ativo dentro Loop

    #############################Fim Amostra de Teste###########################


  }




  ################################################################################
  ############################Fim do envelope#####################################
  ################################################################################







  ResProbPosLSTMt   #Resultado Probabilidade Sinal Positivo - LSTM Train
  ResProbPos       #Resultado Probabilidade Sinal Positivo - RNA Particular Train
  ResProbTPosLSTMt  #Resultado Probabilidade Distribui??o t - LSTM Train
  ResProbTPos      #Resultado Probabilidade Distribui??o t - RNA Particular Train

  ResProbPosLSTMtPredict #Resultado Probabilidade Sinal Positivo - LSTM Test
  ResProbPosPredict #Resultado Probabilidade Sinal Positivo - RNA Particular Test
  ResProbTPosLSTMtPredict #Resultado Probabilidade Distribui??o t - LSTM Test
  ResProbTPosPredict #Resultado Probabilidade Distribui??o t - RNA Particular Test
  ###
  write_xlsx (ResProbPosLSTMt,"ResProbPosLSTMt.xlsx")
  write_xlsx (ResProbPos,"ResProbPos.xlsx")
  write_xlsx (ResProbTPosLSTMt, "ResProbTPosLSTMt.xlsx")
  write_xlsx (ResProbTPos, "ResProbTPos.xlsx")

  write_xlsx (ResProbPosLSTMtPredict, "ResProbPosLSTMtPredict.xlsx")
  write_xlsx (ResProbPosPredict, "ResProbPosPredict.xlsx")
  write_xlsx (ResProbTPosLSTMtPredict, "ResProbTPosLSTMtPredict.xlsx")
  write_xlsx (ResProbTPosPredict, "ResProbTPosPredict.xlsx")


  ################################################################################
  ################################################################################
  ########################          AMOSTRAS          ############################
  ################################################################################
  ################################################################################

  ## Train LSTMt
  order(as.matrix(ResProbPosLSTMt))
  nomes = colnames(ResProbPosLSTMt)
  prob = t(ResProbPosLSTMt)
  Res=as.matrix(ResProbPosLSTMt)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteLSTMt= matrix(data=ncol(ResProbPosLSTMt):1, nrow=1, ncol=ncol(ResProbPosLSTMt))
  colnames(TesteLSTMt)= Test$Nomes
  TesteLSTMt[1,]=Test$Prob

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

  order(as.matrix(ResProbTPosLSTMt))
  nomes = colnames(ResProbTPosLSTMt)
  prob = t(ResProbTPosLSTMt)
  Res=as.matrix(ResProbTPosLSTMt)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTLSTMt= matrix(data=ncol(ResProbTPosLSTMt):1, nrow=1, ncol=ncol(ResProbTPosLSTMt))
  colnames(TesteTLSTMt)= Test$Nomes
  TesteTLSTMt[1,]=Test$Prob

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

  order(as.matrix(ResProbPosLSTMtPredict))
  nomes = colnames(ResProbPosLSTMtPredict)
  prob = t(ResProbPosLSTMtPredict)
  Res=as.matrix(ResProbPosLSTMtPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteLSTMtPredict= matrix(data=ncol(ResProbPosLSTMtPredict):1, nrow=1, ncol=ncol(ResProbPosLSTMtPredict))
  colnames(TesteLSTMtPredict)= Test$Nomes
  TesteLSTMtPredict[1,]=Test$Prob

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


  order(as.matrix(ResProbTPosLSTMtPredict))
  nomes = colnames(ResProbTPosLSTMtPredict)
  prob = t(ResProbTPosLSTMtPredict)
  Res=as.matrix(ResProbTPosLSTMtPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTLSTMtPredict= matrix(data=ncol(ResProbTPosLSTMtPredict):1, nrow=1, ncol=ncol(ResProbTPosLSTMtPredict))
  colnames(TesteTLSTMtPredict)= Test$Nomes
  TesteTLSTMtPredict[1,]=Test$Prob

  ###
  #### RNA-t Particular para armazenar em T8

  order(as.matrix(ResProbTPosPredict))
  nomes = colnames(ResProbTPosPredict)
  prob = t(ResProbTPosPredict)
  Res=as.matrix(ResProbTPosPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTPosPredict= matrix(data=ncol(ResProbTPosPredict):1, nrow=1, ncol=ncol(ResProbTPosLSTMtPredict))
  colnames(TesteTPosPredict)= Test$Nomes
  TesteTPosPredict[1,]=Test$Prob

###############################################################################
  # Statistic Summary
  ## Train LSTMt
  order(as.matrix(Resultados_Assim_Curtose[1,]))
  nomes = colnames(Resultados_Assim_Curtose)
  prob = data.frame(t(Resultados_Assim_Curtose))
  Summary_ANNt = arrange(prob, desc(Probability))



  ###
  #T1=data.frame(TesteLSTMt)   #Resultado Sinal Positivo - LSTM Ordenada Train
  #T2=data.frame(TesteTLSTMt)  #Resultado Sinal Positivo - RNA Particular Ord Train
  #T3=data.frame(TestePos)    #Resultado Prob. Dist t - LSTM Ordenada Train
  #T4=data.frame(TesteTPos)   #Resultado Proba. Dist t - RNA Particular Ord Train
  #T5=data.frame(TesteLSTMtPredict) #Res Prob. Sinal Positivo - LSTM Ord Test
  #T6=data.frame(TestePosPredict) #Resultado Prob. Dist t - LSTM Ordenada Test
  #T7=data.frame(TesteTLSTMtPredict) #Res Prob. Sinal Positivo - RNA Part Ord Test
  #T8=data.frame(TesteTPosPredict) #Res Prob. Dist t - RNA Particular Ordenada Test

  ###
  T1=data.frame(TesteLSTMt)   #Resultado Sinal Positivo - LSTM Ordenada Train
  T2=data.frame(TesteTLSTMt)  #Resultado Prob. Dist t - LSTM Ordenada Train
  T3=data.frame(TestePos)    #Resultado Sinal Positivo - RNA Particular Ord Train
  T4=data.frame(TesteTPos)   #Resultado Proba. Dist t - RNA Particular Ord Train
  T5=data.frame(TesteLSTMtPredict) #Res Prob. Sinal Positivo - LSTM Ord Test
  T6=data.frame(TestePosPredict) #Res Prob. Sinal Positivo - RNA Part Ord Test
  T7=data.frame(TesteTLSTMtPredict) #Resultado Prob. Dist t - LSTM Ordenada Test
  T8=data.frame(TesteTPosPredict) #Res Prob. Dist t - RNA Particular Ordenada Test


#View(T8)
Assets_ANNt_Order = T8
rownames(Assets_ANNt_Order)='Probability'
View(Assets_ANNt_Order)
View(Summary_ANNt)
print(Assets_ANNt_Order)
save(Assets_ANNt_Order,file='~/Assets_ANNt_Order.rda')

cat(paste("___________________________________________________________________
                  LSTMt for all assets concluded
___________________________________________________________________
"))

#View(T7)
Assets_LSTMt_Order = T7
rownames(Assets_LSTMt_Order)='Probability'
View(Assets_LSTMt_Order)
#View(Summary_ANNt)
print(Assets_LSTMt_Order)
save(Assets_LSTMt_Order,file='~/Assets_LSTMt_Order.rda')



nome_asset= str_replace(Final_Date_Testing,"-","_")
nome_asset= str_replace(nome_asset,"-","_")
nome_asset= str_replace(nome_asset,":","_")
nome_asset= str_replace(nome_asset,":","_")
nome_Asset_order=paste("~/Assets_ANNt_Order_",nome_asset,".xlsx", sep="")
nome_Summary_ANNt=paste("~/Summary_ANNt_",nome_asset,".xlsx", sep="")
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
  save(Summary_ANNt,file='~/Summary_ANNt.rda')
  save(ResProbTPosPredict,file='~/ResProbTPosPredict.rda')
  save(T1,file='~/T1.rda')
  save(T2,file='~/T2.rda')
  save(T3,file='~/T3.rda')
  save(T4,file='~/T4.rda')
  save(T5,file='~/T5.rda')
  save(T6,file='~/T6.rda')
  save(T7,file='~/T7.rda')
  save(T8,file='~/T8.rda')

write_xlsx(Assets_ANNt_Order, nome_Asset_order)

Summary_ANNt_xls=data.frame(rownames(Summary_ANNt),Summary_ANNt)
names2=colnames(Summary_ANNt_xls)
names2[1]='Ticker'
colnames(Summary_ANNt_xls)<-names2
write_xlsx(Summary_ANNt_xls, nome_Summary_ANNt)
  ###############################

}
