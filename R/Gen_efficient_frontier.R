#' Gen_efficient_frontier
#' Generate efficient frontier of Markowitz

#' @param Initial_Analysis_Date Initial Date of Analysis. If '' is the same of ANNt Testing
#' @param Final_Analysis_date Final Date of Analysis. If '' is the same of ANNt testing
#' @examples
#' Initial_Analysis_Date <- c('2023-01-03')
#' Final_Analysis_Date <- c('2023-01-03')
#' Gen_efficient_frontier('','')
#' @export
Gen_efficient_frontier<-function(Initial_Analysis_Date,Final_Analysis_Date){

  library(PerformanceAnalytics)
  library(stringr)
  library(tibble)
  library(timetk)
  library(writexl)
  options(warn=-1)
  ### Long Term Analysis
  load('~/scenario.set.rda')
  load('~/Initial_Date_Testing.rda')
  load('~/Final_Date_Testing.rda')
  load('~/T8.rda')
  load('~/Rf.rda')
  load('~/ResProbTPosPredict.rda')
  load('~/N_Assets.rda')
  load('~/Classificacao_MFractal.rda')
  load('~/weight_SHARPE.rda')
  load('~/pesos_todosPredict.rda')
  load('~/Comparativo_RETORNOS.rda')

  scenario.set = data.frame(scenario.set)
  if(Final_Analysis_Date==('')){
    Final_Analysis_Date=rownames(as.data.frame(scenario.set)[nrow(scenario.set),])
  }
  if(Initial_Analysis_Date==('')){
    Initial_Analysis_Date=Initial_Date_Testing
  }

  if(class(Initial_Analysis_Date)!=('numeric')){

    if(length(which(rownames(Comparativo_RETORNOS)==Initial_Analysis_Date))==0){
      while(length(which(rownames(Comparativo_RETORNOS)==Initial_Analysis_Date))==0){
        dia=as.Date(Initial_Analysis_Date)
        new_day=dia+1
        Initial_Analysis_Date = as.character(new_day)
      }}
      if(length(which(rownames(Comparativo_RETORNOS)==Final_Analysis_Date))==0){
        while(length(which(rownames(Comparativo_RETORNOS)==Final_Analysis_Date))==0){
          dia=as.Date(Final_Analysis_Date)
          new_day=dia-1
          Final_Analysis_Date = as.character(new_day)
        }}

    Datas1Predict = rownames(scenario.set)[
      (which(rownames(scenario.set)==Initial_Analysis_Date)):
        (which(rownames(scenario.set)==Final_Analysis_Date))]
  }else{
      Datas1Predict = rownames(scenario.set)[(Initial_Analysis_Date+6):(
      which(rownames(scenario.set)==Final_Analysis_Date))]
  }


  TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))


  PosCovidSP500 = as.matrix(scenario.set[Datas1Predict,1])
  P1 = ts(PosCovidSP500)

  PosCovid_set.returns = as.data.frame(scenario.set[Datas1Predict,])
  P2 = ts(PosCovid_set.returns)

  Betas <- CAPM.beta(Ra=P2[,1:ncol(P2)], Rb=P1)


  Medias_set.returns <- as.matrix(t(apply(PosCovid_set.returns[,-1], 2, mean)))
  Betas_set <- Betas[,-1]

  Desvios_set.returns <- as.matrix(t(apply(PosCovid_set.returns[,-1], 2, sd)))

  ProbabilidadexDesvio <- as.matrix((Desvios_set.returns^2+ResProbTPosPredict^2)^0.5)

  Base_Palomar <- rbind(Medias_set.returns,Desvios_set.returns, Betas_set,
                        as.matrix(ResProbTPosPredict), ProbabilidadexDesvio)
  nomes <- c("Media","Desvio", "Betas", "Prob>SP500", "Probabilidade do Desvio")
  rownames(Base_Palomar)<-nomes

  MF_DFA = as.data.frame(Classificacao_MFractal)[1:N_Assets]
  #Buffet = c("AAPL", "BAC", "CVX", "KO", "AXP", "KHC", "OXY", "MCO")
  ANNt = T8[1:N_Assets]
  max_frame = as.data.frame(weight_Sharpe)
  assets_max_frame <- str_replace(rownames(max_frame),"w.","")
  rownames(max_frame)=assets_max_frame
  Sharpe_ativos = t(dplyr::filter(max_frame,max_frame>0))
  Base_Palomar_frame = as.data.frame(Base_Palomar)

  sd_sharpe = sd(as.data.frame(Comparativo_RETORNOS)$SHARPE)
  mean_sharpe = mean(as.data.frame(Comparativo_RETORNOS)$SHARPE)
  RNAt = Base_Palomar_frame%>%dplyr::select(which((colnames(Base_Palomar_frame) %in% colnames(ANNt))))
  MF = Base_Palomar_frame%>%dplyr::select(which((colnames(Base_Palomar_frame) %in% colnames(MF_DFA))))
  #BF = Base_Palomar_frame%>%dplyr::select(which((colnames(Base_Palomar_frame) %in% Buffet)))
  Sharpe = Base_Palomar_frame%>%dplyr::select(which((colnames(Base_Palomar_frame) %in% colnames(Sharpe_ativos))))
  pesos_Mkw <- t(as.data.frame(pesos_todosPredict))
  colnames(pesos_Mkw)<-colnames(Base_Palomar)
  pesos_Mkw_transposta <- as.data.frame(t(pesos_Mkw))
  Mkw_cart = dplyr::filter(pesos_Mkw_transposta,pesos_Mkw_transposta>0)
  Mkw_ativos = t(Mkw_cart)
  Mkw = Base_Palomar_frame%>%dplyr::select(which((colnames(Base_Palomar_frame) %in% colnames(Mkw_ativos))))

  #### Fronteira Eficiente###################################################################################################
##Adjust necessary length series
  if (nrow(TodosAtivosPredict)<ncol(TodosAtivosPredict)){
    message("The length of the series is less than the number of assets. I will increase the length so I can calculate the Sharpe portfolio of all assets. I'll do this just for this portfolio, ok!")
  }
  while (nrow(TodosAtivosPredict)<ncol(TodosAtivosPredict)){
    Inicio=as.Date(rownames(TodosAtivosPredict)[1])-(ncol(TodosAtivosPredict)-nrow(TodosAtivosPredict))
    Fim=as.Date(rownames(TodosAtivosPredict)[nrow(TodosAtivosPredict)])

    if(length(which(rownames(scenario.set)==Inicio))==0){
      while(length(which(rownames(scenario.set)==Inicio))==0){
        dia=as.Date(Inicio)
        new_day=dia+1
        Inicio = as.character(new_day)
      }
    }

    if(length(which(rownames(scenario.set)==Fim))==0){
      while(length(which(rownames(scenario.set)==Fim))==0){
        dia=as.Date(Fim)
        new_day=dia-1
        Fim = as.character(new_day)
      }
    }

    TodosAtivosPredict=scenario.set[(which(rownames(scenario.set)==Inicio)-10):which(rownames(scenario.set)==Fim),-1]
  }

  all.returns=TodosAtivosPredict
  Contador=round(nrow(all.returns),-1)
  #if(nrow(all.returns)-Contador<0){
  Contador=Contador-10
  #}
  Remover= nrow(all.returns)-Contador
  if(ncol(all.returns)>10){
    all.returns <- all.returns[1:(nrow(all.returns)-Remover),]

    if (nrow(all.returns)-ncol(all.returns)<10){
      Inicio=as.Date(rownames(all.returns)[1])
      Fim=as.Date(rownames(all.returns)[nrow(all.returns)])
      all.returns=scenario.set[(which(rownames(scenario.set)==Inicio)-20):which(rownames(scenario.set)==Fim),-1]
    }
  }

  TodosAtivosPredict=all.returns

  ##############################################################################
   # Cria um n?mero de carteiras aleatorias
  num_cart <- 150000
  acoes = colnames(as.data.frame(scenario.set)[-1])
  # Cria uma matriz para armazenar os pesos
  todos_pesos <- matrix(nrow = num_cart,
                        ncol = length(acoes))
  # Cria um vetor vazio para armazenar os retornos da carteira
  ret_carteira <- vector('numeric', length = num_cart)
  # Cria um vetor vazio para armazenar os desvios-padr?o da carteira
  risco_carteira <- vector('numeric', length = num_cart)
  # Cria um vetor vazio para armazenar os ?ndices de Sharpe da carteira
  indice_sharpe <- vector('numeric', length = num_cart)

  ################# Envelope LOOP 5000 vezes #######################################


  id <- diag(length(acoes))
  ret_medio <- colMeans(TodosAtivosPredict)
  #mat_cov <- cov(TodosAtivosPredict)
  mat_cov <- apply(TodosAtivosPredict, 2, skewness)
  rf <- (1+Rf)^(1/252-1) # Renda Fixa
  for (i in seq_along(ret_carteira)) {
    if (i <= length(acoes)) # carteiras com apenas uma a??o
      pesos <- id[i,]
    else
      pesos <- runif(length(acoes))
    pesos <- pesos/sum(pesos)
    # Guarda os pesos em uma matriz
    todos_pesos[i,] <- pesos
    # Retornos das carteiras
    port_ret <- sum(pesos * ret_medio)
    #port_ret <- ((port_ret + 1)^252) - 1
    # Salvando os retornos das carteiras
    ret_carteira[i] <- port_ret
    # Cria e salva os riscos das carteiras
    #port_sd <- sqrt(t(pesos) %*% (mat_cov %*% pesos))
    port_sd <- (t(mat_cov) * pesos)^2
    risco_carteira[i] <- port_sd
    # Cria e salva os ?ndices de Sharpe das carteiras
    i_sharpe <- (port_ret - rf) / port_sd
    indice_sharpe[i] <- i_sharpe
  }

  # armazenar os valores, converter a matriz em um ?tibble`, alterar os nomes das vari?veis e combinar os
  # valores
  valores_carteira <- tibble(Retorno = ret_carteira,
                             Risco = risco_carteira,
                             IndiceSharpe = indice_sharpe)
  todos_pesos <- tk_tbl(todos_pesos)
  colnames(todos_pesos) <- acoes
  valores_carteira <- tk_tbl(cbind(todos_pesos, valores_carteira))

  # Excluindo a matriz diagonal
  #todos_pesos2 <- todos_pesos[491:nrow(todos_pesos),]

  #valores_carteira2 <- valores_carteira[491:nrow(todos_pesos),]
  #valores_carteira2_ <- tk_tbl(cbind(todos_pesos2, valores_carteira2))



  # A carteira de vari?ncia m?nima
  # A carteira de tang?ncia (a carteira com maior ?ndice sharpe)
  min_var <- valores_carteira[which.min(valores_carteira$Risco),]
  max_i_sharpe <- valores_carteira[which.max(valores_carteira$IndiceSharpe),]
  max_i_sharpe <- valores_carteira[which.max(valores_carteira$IndiceSharpe),]

  min_var_= min_var[,which(min_var !=0)]
  max_i_sharpe_= as.data.frame(max_i_sharpe[,which(max_i_sharpe !=0)])

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

  retornosAtivos = TodosAtivosPredict
  pesos_front <- fronteiraCarteira(retornosAtivos, nPontos=200)
  mu = Medias_set.returns
  retornoAlvos  <-  seq(min(mu), max(mu), length = nrow(pesos_front))

  riscosAlvo  <-  NULL
  for (i in 1:nrow(pesos_front)) {
    novoRiscoAlvo  <-  sqrt(pesos_front[i, ] %*%
                              cov(retornosAtivos) %*%
                              pesos_front[i, ])
    riscosAlvo  <-  c(riscosAlvo, novoRiscoAlvo)
  }
  fronteiraEficiente <- data.frame(risco=riscosAlvo, retorno=retornoAlvos)

  #  all.returns <- TodosAtivosPredict
  ## set up portfolio with objetive and constraints
  #n.assets <- length(colnames(all.returns))
  #port.sec <- portfolio.spec(assets = colnames(all.returns))
  #port.sec <- add.objective(portfolio = port.sec, type = "risk", name = "StdDev")
  #port.sec <- add.objective(portfolio = port.sec, type = "return", name = "mean")
  #port.sec <- add.constraint(portfolio = port.sec, type = "full_investiment")
  #port.sec <- add.constraint(portfolio = port.sec, type = "box", min = 0, max = 1)

  # map off efficient frontier (for variance risk)
  #eff.frontier <- create.EfficientFrontier(R = all.returns, portfolio = port.sec,
  #                                        n.portfolio = 100, type = "mean-StdDev")

  # Daily Sharpe ratio
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
  #weight_test <- round(weight_test,4)
  #weight_Sharpe= weight_test[which(weight_test !=0)]
  #weight_Sharpe
  Initial_Analysis_Date =Datas1Predict[1]

  save(fronteiraEficiente,file='~/fronteiraEficiente.rda')
  save(Initial_Analysis_Date,file='~/Initial_Analysis_Date.rda')
  save(Final_Analysis_Date,file='~/Final_Analysis_Date.rda')
  save(max_frame,file='~/max_frame.rda')
  save(Base_Palomar,file='~/Base_Palomar.rda')
  save(Mkw,file='~/Mkw.rda')
  save(RNAt,file='~/RNAt.rda')
  save(MF,file='~/MF.rda')
  save(Sharpe,file='~/Sharpe.rda')
  save(riscosAlvo,file='~/riscosAlvo.rda')
  save(retornoAlvos,file='~/retornoAlvos.rda')

  write_xlsx(Base_Palomar_frame,'Base_Palomar.xts')

    X="Efficient frontier generated!"
    print(X)
}
