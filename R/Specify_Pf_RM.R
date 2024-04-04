#'Specify RM
#'permit that create a specific portfolio benchmark to use in ANNt
#'@description
#'Portfolio generate with the assets and weights that you specify
#'@param Name Name of the portfolio
#'@param Portfolio Describes the assets in the portfolio
#'@param Weights Describe the weights of the portfolio´s assets
#'@examples
#'Name = c("My_Pf")
#'Portfolio=c("AAPL", "BAC", "CVX", "KO", "AXP", "KHC", "OXY", "MCO") # PORTFOLIO´s Buffet 2023
#'Weights=c( 0.441, 0.089, 0.079, 0.075, 0.074, 0.038, 0.038, 0.023) # PORTFOLIO weights 2023
#'Specify_Pf_RM(Name,Portfolio,Weights)
#'@export
Specify_Pf_RM<-function(Name,Portfolio,Weights){
  library(dplyr)
  #Buffet = c("AAPL", "BAC", "KO", "AXP", "CVX", "KHC", "OXY") # PORTFOLIO_2022
  #PesosBuffet = c( 0.414, 0.102, 0.073, 0.068, 0.068, 0.037, 0.033) # PORTFOLIO_2022
  load('~/scenario.set.rda')
  #load('~/Datas1Predict.rda')
  load('~/RM.rda')
  load("~/Initial_Date.rda") # Carrega objeto scenario.set

  Buffet=Portfolio
  PesosBuffet=Weights

  sum(PesosBuffet)
  PesosBuffetNormalizado = PesosBuffet/sum(PesosBuffet)
  CarteiraBuffet = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% Buffet)))
  #PosCovidBuffet = CarteiraBuffet[Datas1Predict,]
  #View(PosCovidBuffet)

  #View(PosCovidBuffet2)
  Specific_RM = as.matrix(CarteiraBuffet) %*% PesosBuffetNormalizado
  Portfolio_with_RM_Original=scenario.set
  RM_Original=RM
  RM_Specific=Name
  RM=Name
  scenario.set[,1]=Specific_RM
  Nomes_todos=c(Name,colnames(scenario.set[,-1]))
  colnames(scenario.set)=Nomes_todos

  save(RM_Original,file='~/RM_Original.rda')
  save(RM_Specific,file='~/RM_Specific.rda')
  save(scenario.set,file='~/scenario.set.rda')
  save(RM, file="~/RM.rda")

  ################################################################################
  #### Asset_Order
  ################################################################################

}
