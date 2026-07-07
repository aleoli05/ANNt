#' Plot_NEF_New
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
#'@param Lambda Risk Aversion parameter. Default is 0.5
#'@param nPoints number of simulation. Default is 200
#' @author Alexandre Silva de Oliveira
#' @examples
#' N_Assets <- 3
#' Initial_Date_Testing <- c('2023-01-03')
#' Final_Date_Testing <- c('')
#' Rf <- 0
#' type_ANNt <- 'T8'
#' # Generate assets portfolio (maximum N assets specified)
#' Plot_NEF_New(3,'2023-01-03','',0,'T8')
#'
Plot_NEF_New <-function(N_Assets, Initial_Date_Testing, Final_Date_Testing, Rf,
                        type_ANNt,Lambda=0.5,
                        nPoints =200){


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
  if (!requireNamespace("IntroCompFinR", quietly = TRUE)) {
    install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
  }
  library(IntroCompFinR)
  library(Matrix)
  print('Generating Plot__New_efficient_frontier Command')
  #load('~/Rf.rda')
  if(class(N_Assets)=='character'){
    N_Assets=as.numeric(N_Assets)
    Lambda=as.numeric(Lambda)
    nPoints=as.numeric(nPoints)
  }


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
  if (file.exists("~/x5.rda")==TRUE){
    load("~/x5.rda") # Carrega objeto scenario.set
  } else { x5=Rf}
  load("~/scenario.set.rda") # Carrega objeto scenario.set
  load("~/I_dataPredict.rda") # Carrega objeto scenario.set
  load("~/F_dataPredict.rda") # Carrega objeto scenario.set
  if(Rf=='x5'){
    Rf=x5
  }
  if(exists('Initial_Date_Testing')==FALSE) {
    load("~/Initial_Date_Testing.rda")
  }
  if(exists('Final_Date_Testing')==FALSE){
    load("~/Final_Date_Training.rda")
    load("~/Final_Date_Testing.rda")
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

  Fator_Tempo = 1.80/(nrow(dados))
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

    if(length(which(rownames(as.data.frame(scenario.set))==Final_Date_Training))==0){
      while(length(which(rownames(as.data.frame(scenario.set))==Final_Date_Training))==0){
        dia=as.Date(Final_Date_Training)
        new_day=dia-1
        Final_Date_Training = as.character(new_day)
      }
    }

    D = which(rownames(as.data.frame(scenario.set))==Final_Date_Training)
    Initial_Date_Testing= rownames(as.data.frame(scenario.set)[D+1,])
  }
  if(length(which(rownames(dados2)==Initial_Date_Testing))==0){
    #Final_Date_Testing=rownames(dados2[nrow(dados2),])
    #Final_Date_Testing=Sys.Date()
    while(length(which(rownames(dados2)==Initial_Date_Testing))==0){
      dia=as.Date(Initial_Date_Testing)
      new_day=dia+1
      Initial_Date_Testing = as.character(new_day)
      print ('Initial Ok')
    }
    #print ('Initial Date Testing Ok')
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
      #print ('Final Ok')
    }
    #print ('Initial Date Testing Ok')
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

  print("Dates Verification Ok")
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
    all.returns=scenario.set[which(rownames(scenario.set)==as.character(Inicio)):which(rownames(scenario.set)==Fim),-1]
  }

  if ((ncol(TodosAtivosPredict)<nrow(TodosAtivosPredict))==TRUE){
    all.returns=TodosAtivosPredict
  }


################################################################################


# 1. Carregar o pacote para otimização quadrática
if(!require(quadprog)) install.packages("quadprog")
library(quadprog)
library(Matrix)
library(splines)
library(ggplot2)
library(dplyr)

load('~/Summary_ANNt_Training.rda')
load('~/Summary_ANNt_Testing.rda')
load('~/scenario.set.rda')
# 2. Configurando o problema (Maximizar: Retorno - lambda * Variância)
# Na notação matricial da solve.QP: min 0.5 * w' Dmat w - dvec' w
#Dmat <- 2 * lambda * matriz_cov
#dvec <- retornos

if (type_ANNt=="T4"){
Ativos=rownames(Summary_ANNt_Training)
P=Summary_ANNt_Training[1:N_Assets,c(17,1,18)]
}
if (type_ANNt=="T8"){
Ativos=rownames(Summary_ANNt_Testing)
P=Summary_ANNt_Testing[1:N_Assets,c(17,1,18)]
}
Ativos=Ativos[1:N_Assets]
R = as.data.frame(all.returns) %>%
  dplyr::select(which((colnames(all.returns) %in% Ativos)))
#R=R1[6:which(rownames(R1)=='2022-12-29'),]
#R=R1[which(rownames(R1)=='2022-12-29'):nrow(R1),]

Nomes_ordem = rownames(P)
R=R[,Nomes_ordem]
#P=1-P


matriz_quadrada <- matrix(0, nrow = N_Assets, ncol = N_Assets-3)

# 3. Inserir a matriz original nas 3 primeiras linhas da nova matriz
P2<- cbind(P,matriz_quadrada)
Dmat <- -as.matrix(P2)
#######

Dmat_fixed=Dmat

##########
#retornoAlvo <- seq(min(mu), max(mu), length = nPoints)

pesosCarteira <- function(retornosAtivos, retornoAlvo) {

  if(!require("quadprog")) install.packages("quadprog")
  suppressMessages(suppressWarnings(library(quadprog)))

nAtivos  <-  N_Assets

portfolio <- solve.QP(
  Dmat <- 2*Lambda*nearPD(as.matrix(Dmat_fixed))$mat,
  #Dmat <- cov(retornosAtivos),                        # matriz D
  #dvec <- rep(0, times = nAtivos)/Lambda,                    # vetor  d
  dvec <- colMeans(R),
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
fronteiraCarteira <- function(retornosAtivos, nPontos = nPoints) {
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
retornosAtivos = R

tryCatch({
  pesos_front <- fronteiraCarteira(retornosAtivos, nPontos=nPoints)
}, error=function(e){
  eig <- eigen(Dmat)
  # Substitui autovalores negativos por 1e-8
  eig$values <- pmax(as.numeric(eig$values), 1e-8)
  Dmat_fixed <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  pesos_front <- fronteiraCarteira(retornosAtivos, nPontos=nPoints)
})


Retornos_Carteiras= as.matrix(pesos_front) %*%colMeans(R)
Prob_Carteiras=as.matrix(pesos_front) %*%P[,2]

ANNt_weights_Max_Ret =pesos_front[which.max(Retornos_Carteiras),]
Prob_ANNt_weights_Max_Ret = P[,2] %*% ANNt_weights_Max_Ret

Ret_ANNt_weights_Max_Ret = colMeans(R) %*% ANNt_weights_Max_Ret
ANNt_weights_Max_Ret <- ANNt_weights_Max_Ret[ANNt_weights_Max_Ret>0]
ANNt_weights_Max_Ret


ANNt_weights_Max_Prob =pesos_front[which.max(Prob_Carteiras),]
Prob_ANNt_weights_Max_Prob = P[,2] %*% ANNt_weights_Max_Prob
Nomes_Prob = rownames(as.data.frame(ANNt_weights_Max_Prob))

Ret_ANNt_weights_Max_Prob = colMeans(R) %*% ANNt_weights_Max_Prob
ANNt_weights_Max_Prob <- ANNt_weights_Max_Prob[ANNt_weights_Max_Prob>0]
ANNt_weights_Max_Prob

Asset_Prob = rownames(as.data.frame(ANNt_weights_Max_Prob))
#Retornos_Asset_Prob = colMeans(R %>% select(all_of(Asset_Prob)))
#Retornos_Asset_Prob = R %>% select(all_of(Asset_Prob)) %>% rowMeans(na.rm = TRUE)
R_Asset_Prob = as.data.frame(R[, Asset_Prob])
Retornos_Asset_Prob <- colMeans(R_Asset_Prob)
Prob_Asset_Prob = P[match(Asset_Prob, rownames(P)), 2]
Prob_Asset_Prob = P[rownames(P) %in% Asset_Prob, 2]
Points_Prob = cbind(Prob_Asset_Prob,Retornos_Asset_Prob)

############################################################################
ydev=dev.list()
if(class(ydev)!="NULL"){
  dev.off()
  print('Starting Plot__New_efficient_frontier Command')
}else{print('Starting Plot__New_efficient_frontier Command')}
dev.capabilities()
#############################################################################
op <- par(new = TRUE)
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A")
x=P[,2]
y=colMeans(R)
points=cbind(x,y)
hull_indices = chull(points)
hull_points=points[c(hull_indices, hull_indices[1]),] # Fechar o poligono

pareto_front_superior_direita <- function(x,y){
  stopifnot(length(x)==length(y))
  df<-data.frame(x,y)
  df <- df[order(df$x, df$y, decreasing = TRUE),]
  is_dominated <- logical(nrow(df))
  max_y <- -Inf
  for (i in seq_along(df$y)){
    if (df$y[i]> max_y){
      is_dominated[i] <- FALSE
      max_y <- df$y[i]
    } else{
      is_dominated[i]<- TRUE
    }
  }
  return(df[!is_dominated,])
}

envoltoria = pareto_front_superior_direita(x,y)

png(file="~/NEF_New.png", width=1920, height=1200, res=296, family = "A")
par(mgp = c(3, 0.5, 0))
plot(points,
  #x = Prob_Carteiras,
  #y = Retornos_Carteiras,
  family="A",
  type ="pch",
  #xaxt = "n",
  ylab = "Average Returns",
  xlab = "Return Probability > Return Benchmark",
  las =1,
  col="gray",
  xlim = c(min(P[,2]), max(P[,2])+(max(P[,2])-min(P[,2]))*0.55),
  main = "NEF with Budget Constraint (Sum w = 1)")
#points(x=P[,2],y=colMeans(R), #,main="Fronteira Eficiente por Desvio",
#       #ylab="Retorno", xlab="Desvio-Padr?o",
#       col="gray"
#       #xlim = c(0.01, 0.07))
#)
lines(hull_points,
      col='gray')
lines(envoltoria,
      col='black',
      lwd=2)
points(Points_Prob,
       col="red",
       pch=21
)
points(x=Prob_ANNt_weights_Max_Prob,y=Ret_ANNt_weights_Max_Prob, #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="red",
       pch=19
       #xlim = c(0.01, 0.07))
)
text(Points_Prob,
     labels = rownames(Points_Prob),
     col="red",
     cex = 0.6,
     adj = -0.3
)
points(x=Prob_ANNt_weights_Max_Ret,y=Ret_ANNt_weights_Max_Ret, #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="blue",
       pch=19
       #xlim = c(0.01, 0.07))
)
text(x=Prob_ANNt_weights_Max_Ret,y=Ret_ANNt_weights_Max_Ret,
     labels = rownames(as.data.frame(ANNt_weights_Max_Ret)),
     col="blue",
     cex = 0.6,
     adj = -0.3
)
lines(x = Prob_Carteiras,
y = Retornos_Carteiras,
 #pch=18
)
legend(
  "topright",
  legend = c("Single Assets", "Data Envelopment", "Great combination", "NEF", "Great Probability", "Great Return"),
  col = c("gray", "gray", "black", "black", "red", "blue"),
  #family=c("A",,),
  pch = c(21, NA, NA, NA, 19, 19),
  lty = c(NA, 1, 1, 1, NA, NA),
  lwd = c(1, 1, 1, 2, 1, 1),
  bty = "n",
  cex=0.6
)
dev.off()
################################################################################

par(mgp = c(3, 0.5, 0))
plot(points,
     #x = Prob_Carteiras,
     #y = Retornos_Carteiras,
     family="A",
     type ="pch",
     #xaxt = "n",
     ylab = "Average Returns",
     xlab = "Return Probability > Return Benchmark",
     las =1,
     col="gray",
     xlim = c(min(P[,2]), max(P[,2])+(max(P[,2])-min(P[,2]))*0.55),
     main = "NEF with Budget Constraint (Sum w = 1)")
#points(x=P[,2],y=colMeans(R), #,main="Fronteira Eficiente por Desvio",
#       #ylab="Retorno", xlab="Desvio-Padr?o",
#       col="gray"
#       #xlim = c(0.01, 0.07))
#)
lines(hull_points,
      col='gray')
lines(envoltoria,
      col='black',
      lwd=2)
points(Points_Prob,
       col="red",
       pch=21
)
points(x=Prob_ANNt_weights_Max_Prob,y=Ret_ANNt_weights_Max_Prob, #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="red",
       pch=19
       #xlim = c(0.01, 0.07))
)
text(Points_Prob,
     labels = rownames(Points_Prob),
     col="red",
     cex = 0.6,
     adj = -0.3
)
points(x=Prob_ANNt_weights_Max_Ret,y=Ret_ANNt_weights_Max_Ret, #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="blue",
       pch=19
       #xlim = c(0.01, 0.07))
)
text(x=Prob_ANNt_weights_Max_Ret,y=Ret_ANNt_weights_Max_Ret,
     labels = rownames(as.data.frame(ANNt_weights_Max_Ret)),
     col="blue",
     cex = 0.6,
     adj = -0.3
)
lines(x = Prob_Carteiras,
      y = Retornos_Carteiras,
      #pch=18
)
legend(
  "topright",
  legend = c("Single Assets", "Data Envelopment", "Great combination", "NEF", "Great Probability", "Great Return"),
  col = c("gray", "gray", "black", "black", "red", "blue"),
  #family=c("A",,),
  pch = c(21, NA, NA, NA, 19, 19),
  lty = c(NA, 1, 1, 1, NA, NA),
  lwd = c(1, 1, 1, 2, 1, 1),
  bty = "n",
  cex=0.6
)
################################################################################
}
