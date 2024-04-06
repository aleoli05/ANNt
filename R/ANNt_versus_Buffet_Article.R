#' ANNt_versus_Buffet_Article
#' Realize the 1st time series research, Validation: Long Term Testing, defined in "Value investing or quantitative financing: portfolio decision based on a new efficient frontier concept" paper.
#'
#' @param Parameters was defined in paper
#' @examples
#' ANNt_versus_Buffet_Article()
#'
#' @export
ANNt_versus_Buffet_Article <- function() {



################################################################################
### ALEXANDRE SILVA DE OLIVEIRA - MY OPTMIZATION MODELING WITH R
################################################################################
###
### PRINCIPAL REFERENCE:
### ----------------------------------------------------------------------------
### Ronald Hochreiter - http://www.finance-r.com/cpom/
### Morris e Comeau (2020) - Financial Markets and Portfolio Management
################################################################################
# 8296 Grafico das carteiras
# 6665 Tamanho das carteiras
# 7455 Tamanho MF-DFA
# 7476 Tamanho RNAt
################################################################################
##### 1. Setup & Data


# library(webinar.cpom)
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


################# Create Returns Times Series ###########################
# BENCHMARK
BENCHMARK <- c("SP500")

## Importando Simbolos do Excel com pesos relativos no SP500
#SP50008072022 <- read_excel("D:/SP50008072022.xlsx")
#View(SP50008072022)

#View(tickers)
#SP50008072022 = SP50008072022 %>% mutate(Symbol =
#                                        case_when(Symbol == "BRK.B" ~ "BRK-B",
#                                                 Symbol == "BF.B" ~ "BF-B",
#                                              TRUE ~ as.character(Symbol)))

#tickers = c("^GSPC")
#for (i in 2:nrow(SP50008072022)){
#  tickers[i] = as.character(SP50008072022[i,2])
#}

# Create vector of Tickers - Amostra de Modelagem
# 1. ^GSPC - S&P500
# 2. AAPL - Apple Inc.
# 3. MSFT - Microsoft Corporation
# 4. AMZN - Amazon.com, Inc.
# 5. TSLA - Tesla, Inc.
# 6. GOOG - Alphabet Inc.
# 7. GOOGL - Alphabet Inc. (GOOGL)
# 8. GOOG - Alphabet Inc. (GOOG)
# 9. NVDA - NVIDIA Corporation (NVDA)
# 10. BRK.B - Berkshire Hathaway Inc. (BRK.B)
# 11. META - Meta Platforms, Inc.
# 12. UNH - Unitedhealth Group (UNH)
# 13. JNJ - Johnson & Johnson
# 14. JPM - JPMorgan Chase & Co.
# 15. V - Visa Inc.
# 16. PG - Procter & Gamble Company (PG)
# 17. XOM - Exxon Mobil Corporation (XOM)
# 18. HD - The Home Depot, Inc. (HD)
# 19. CVX - Chevron Corporation (CVX)
# 20. MA - Mastercard Incorporated (MA)
# 21. BAC - Bank of America Corporation (BAC)
# 22. PFE - Pfizer, Inc. (PFE)
# 23. ABBV - ABBVIE, Inc. (ABBV)
# 24. AVGO - Broadcom, Inc. (AVGO)
# 25. DIS - The Walt Disney Company (DIS)
# 26. VZ - Verizon Communication Inc. (VZ)
# 27. TMO - Thermo Fisher Scientific (TMO)
# 28. COST - Costco Wholesale Corporation (COST)
# 29. ABT - Abbott Laboratories (ABT)
# 30. ACN - Accenture Plc Class A (ACN)
# 31. MCD - MacDonald?s Corporation (MCD)
# 32. CSCO - Cisco Systems Inc. (CSCO)
# 33. CMCSA - Comcast Coportation Class A (CMCSA)
# 34. KO - Coca-Cola Company (KO)
# 35. MRK - Mark & Co. Inc. (MRK)
# 36. LLY - Eli Lilly and Company (LLY)
# 37. ADBE - Adobe Incorporated (ADBE)
# 38. WMT - Walmart Inc. (WMT)
# 39. CRM - Salesforce Inc. (CRM)
# 40. BMY - Bristol - Myers Squibb Company (BMY)
# 41. DHR - Danaher Corporation (DHR)
# 42. PM - Philip Morris International Inc. (PM)
# 43. INTC - Intel Corporation (INTC)
# 44. WFC - Wells Fargo & Company (WFC)
# 45. NEE - NextEra Energy Inc. (NEE)
# 46. LIN - Linde PLC (LIN)
# 47. T - AT&T Inc. (T)
# 48. QCOM - Qualcomm Incorporated (QCOM)
# 49. TXN - Texas Instruments Incorporated (TXN)
# 50. RTX - Raytheon Tecnologies Corporation (RTX)
# 51. UNP - Union Pacific Corporation (UNP)


# 12. UEEC- United Health Products, Inc.


# 2. AXP - American Express Company
# 3. C - Citigroup, Inc.
# 5.
# 6.
# 7.

# 1. PETRO4.SA - Petrobr?s (B3)
# 2. GGBR4.SA - Gerdau (B3)
# 3. USIM5.SA - Usinas Siderurgicas de Minas Gerais A Pref Shs (B3)
# 4. VALE3.SA - Vale S.A. (B3)
# 5. ITUB4.SA - Ita? (B3)
# 6. BBDC4.SA - Bradesco (B3)
# 7. ABEV3.SA - Ambev S/A (B3)
# 8. BBAS3.SA - Banco do Brasil S/A (B3)
# 9. ELET3.SA - Eletrobr?s (B3)
# 10. JBSS3.SA - JBS (B3)
# 11. WEGE3.SA - Wege (B3)
# 12. SUZB3.SA - Suzano S.A. (B3)

# 10. BTC-USD - Bitcoin USD
# 11.
# 12.
# 13. COKE - Coca-cola Consolidated, Inc.
# 14.
# 15. CL=F - Crude Oil May 22 (CBOT)
# 16. ZS=F - Soybean Futures, Jul-22 (CBOT)


tickers_modelagem <- c("^GSPC", "AAPL", "MSFT", "AMZN", "TSLA", "GOOGL", "GOOG",
                       "NVDA",
                       "BRK-B", "META", "UNH", "JNJ", "JPM", "V", "PG", "XOM", "HD", "MA",
                       "BAC", "PFE", "ABBV", "CVX", "AVGO", "DIS", "VZ", "TMO", "COST",
                       "ABT", "ACN", "MCD", "CSCO", "CMCSA", "KO", "MRK", "LLY",
                       "ADBE", "WMT", "CRM", "BMY", "DHR", "PM", "INTC", "LIN", "T",
                       "QCOM", "TXN", "WFC", "NEE", "RTX", "UNP",
                       "UEEC",
                       "AXP", "C",
                       "PETR4.SA", "GGBR4.SA", "USIM5.SA", "VALE3.SA", "ITUB4.SA",
                       "BBDC4.SA", "ABEV3.SA", "BBAS3.SA", "ELET3.SA", "JBSS3.SA",
                       "WEGE3.SA", "SUZB3.SA",
                       "BTC-USD", "PETZ3.SA", "COKE", "CL=F",
                       "ZS=F")

#### Todos os ativos SP500 em ordem descrescente de pesos 08/07/2022

tickers <- c("^GSPC",
             "AAPL",  "MSFT",  "AMZN",  "GOOGL", "GOOG", "TSLA",
             "BRK-B", "UNH",   "JNJ",   "NVDA",  "META",  "XOM",
             "PG",    "JPM",   "V",     "PFE",   "HD",    "CVX",
             "MA",    "ABBV",  "LLY",   "KO",    "PEP",   "MRK",
             "BAC",   "COST",  "TMO",   "VZ",    "AVGO",  "ABT",
             "MCD",   "ADBE",  "CSCO",  "CMCSA", "DIS",   "ACN",
             "WMT",   "CRM",   "DHR",   "BMY",   "NEE",   "INTC",
             "WFC",   "T",     "QCOM",  "PM",    "TXN",   "RTX",
             "LIN",   "NKE",   "UPS",   "UNP",   "AMGN",  "AMD",
             "IBM",   "CVS",   "SPGI",  "LOW",   "MDT",   "AMT",
             "HON",   "INTU",  "ELV",   "ORCL",  "COP",   "MS",
             "GS",    "LMT",   "NOW",   "SCHW",  "CAT",   "C",
             "SBUX",  "ADP",   "PLD",   "BLK",   "CI",    "PYPL",
             "MDLZ",  "AXP",   "DE",    "ZTS",   "NFLX",  "CB",
             "DUK",   "AMAT",  "TMUS",  "MMC",   "GILD",  "ADI",
             "BA",    "MO",    "SO",    "VRTX", "ISRG",  "CME",
             "CCI",   "MMM",   "BKNG",  "PGR",   "NOC",   "BDX",
             "TJX",   "TGT",   "GE",    "SYK",   "PNC",   "CL",
             "REGN",  "MU",    "D",     "TFC",   "CSX",   "USB",
             "EOG",   "EW",    "ATVI",  "HUM",   "EQIX",  "EL",
             "MRNA",  "WM",    "DG",    "AON",   "LRCX",  "SHW",
             "FIS",   "FDX",   "OXY",   "NSC",   "FISV",  "BSX",
             "ICE",   "CHTR",  "ITW",   "APD",   "ETN",   "GD",
             "CNC",   "SNPS",  "PXD",   "SLB",   "AEP",   "PSA",
             "EMR",   "GM",    "NEM",   "KLAC",  "MCK",   "SRE",
             "LHX",   "MCO",   "F",     "GIS",   "KMB",   "MPC",
             "CDNS",  "MET",   "SYY",   "ORLY",  "VLO",   "EXC",
             "AZO",   "COF",   "FCX",   "ROP",   "ADM",   "IQV",
             "FTNT",  "O",     "AIG",   "TRV",   "NXPI",  "ADSK",
             "HCA",   "PSX",   "STZ",   "ECL",   "APH",   "DOW",
             "CTVA",  "MAR",   "XEL",   "DLTR",  "CMG",   "WMB",
             "TEL",   "WELL",  "PAYX",  "A",     "MSI",   "DLR",
             "PRU",   "MNST",  "ALL",   "SBAC",  "EA",    "CTSH",
             "MSCI",  "AJG",   "YUM",   "AFL",   "CTAS",  "HPQ",
             "JCI",   "DVN",   "BAX",   "KMI",   "ED",    "MCHP",
             "IDXX",  "GPN",   "PH",    "DXCM", "RMD",   "KR",
             "SPG",   "WBD",   "BIIB",  "HSY",   "PEG",   "HLT",
             "ILMN",  "WEC",   "BK",    "CARR",  "TT",    "VICI",
             "IFF",   "OTIS",  "TWTR",  "NUE",   "FAST",  "DD",
             "CMI",   "ES",    "VRSK",  "PCAR",  "MTB",   "TDG",
             "PPG",   "DFS",   "MTD",   "HES",   "ENPH",  "AWK",
             "WBA",   "TROW",  "RSG",   "ROST",  "FRC",   "KHC",
             "HAL",   "AVB",   "KDP",   "BKR",   "AMP",   "KEYS",
             "AME",   "WY",    "ODFL",  "TSN",   "OKE",   "APTV",
             "CBRE",  "GLW",   "SIVB",  "EBAY",  "CPRT",  "EQR",
             "DTE",   "EIX",   "ALB",   "DHI",   "WST",   "ROK",
             "LYB",   "STT",   "CHD",   "TSCO",  "EFX",   "FITB",
             "EXR",   "BALL",  "ANET",  "LH",    "ON",    "AEE",
             "ETR",   "WTW",   "ARE",   "HIG",   "ZBH",   "DRE",
             "ANSS",  "LUV",   "CDW",   "MTCH",  "CTRA",  "FE",
             "ABC",   "WAT",   "STE",   "GWW",   "MKC",   "PPL",
             "LEN",   "VTR",   "NTRS",  "IT",    "ULTA",  "TTWO",
             "FTV",   "CTLT",  "FANG",  "GPC",   "MAA",   "VMC",
             "MLM",   "CMS",   "CEG",   "AMCR",  "DAL",   "ALGN",
             "PWR",   "EPAM",  "PKI",   "K",     "CNP",   "NDAQ",
             "HOLX",  "URI",   "RF",    "CFG",   "TDY",   "HBAN",
             "CINF",  "CLX",   "DOV",   "CF",    "RJF",   "IR",
             "VRSN",  "BR",    "ESS",   "HPE",   "SWK",   "FLT",
             "CAG",   "MPWR",  "MOS",   "EXPD",  "J",     "KEY",
             "MOH",   "ZBRA",  "SWKS",  "DGX",   "IP",    "SEDG",
             "PFG",   "WAB",   "ATO",   "PARA",  "PAYC",  "GRMN",
             "COO",   "MRO",   "POOL",  "KMX",   "FDS",   "INCY",
             "DRI",   "TRMB",  "AKAM",  "STX",   "DPZ",   "TER",
             "SYF",   "VFC",   "CAH",   "EVRG",  "EXPE",  "GNRC",
             "NTAP",  "XYL",   "LNT",   "BBY",   "SJM",   "CPT",
             "TECH",  "LDOS",  "AES",   "IEX",   "IRM",   "PEAK",
             "WRB",   "TYL",   "BRO",   "WDC",   "JBHT",  "LKQ",
             "AVY",   "JKHY",  "UDR",   "HRL",   "TXT",   "NVR",
             "PKG",   "OMC",   "NLOK",  "FMC",   "CHRW",  "BXP",
             "BF-B",  "CBOE",  "MAS",   "VTRS",  "CE",    "KIM",
             "HWM",   "CTXS",  "LVS",   "SBNY",  "ABMD",  "TFX",
             "UAL",   "EMN",   "APA",   "LYV",   "NI",    "L",
             "CRL",   "AAP",   "NDSN",  "HST",   "RE",    "ETSY",
             "PTC",   "BIO",   "IPG",   "QRVO",  "WRK",   "HAS",
             "SNA",   "LW",    "HSIC",  "FOXA",  "MGM",   "PHM" ,
             "TAP",   "MKTX",  "LUMN",  "AIZ",   "CPB",   "CMA",
             "FFIV",  "JNPR",  "NRG",   "GL",    "REG",   "WHR",
             "AAL",   "ALLE",  "CZR",   "RHI",   "HII",   "FBHS",
             "SEE",   "NLSN",  "TPR",   "OGN",   "BWA",   "LNC",
             "ROL",   "PNW",   "RCL",   "AOS",   "PNR",   "ZION",
             "XRAY",  "CCL",  "DXC",   "NWL",   "UHS",   "CDAY",
             "FRT",   "BBWI",  "BEN",   "MHK",   "NWSA",  "WYNN",
             "IVZ",   "PENN",  "DVA",   "ALK",   "NCLH",  "VNO",
             "FOX",   "DISH",  "RL",    "PVH",   "NWS",   "EMBC")



################################################################################

#Calculate Returns: Daily
tickers <- tickers_modelagem
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2010-01-01", to="2023-03-17",
                                            auto.assign=FALSE)[,4])

# Salvando o SP500 em excel
colnames(portfolioPrices) <- str_replace(tickers,".Close","")
portfolioPrices_Teste = portfolioPrices


Datas_portfolio = rownames(as.data.frame(portfolioPrices))
portfolioPrices_Df = mutate(as.data.frame(Datas_portfolio),
                            as.data.frame(portfolioPrices))
write_xlsx(portfolioPrices_Df, "SP500.xlsx")

# Excluindo ativos para matriz retangular

portfolioPrices = portfolioPrices_Teste
Excluir = c( "EMBC", "FOX", "CDAY", "DOW", "CTVA", "NWSA", "NWS", "OTIS", "CEG",
             "OGN", "CARR", "FOXA", "MRNA")
portfolioPrices = portfolioPrices[,!(names(portfolioPrices) %in% Excluir)]
tickers = colnames(portfolioPrices)


#View(portfolioPrices)

# Delete all dates with no prices
#portfolioPrices <- portfolioPrices_Df %>% filter(nrow() >
#                           nrow(rownames("2016-01-04")))
#portfolioPrices <- portfolioPrices[apply(portfolioPrices_Df,1,
#                                       function(x) filter(nrow(x)>1511)),]
#portfolioPrices <- matrix(which(rownames(portfolioPrices)> "2016-01-04"))
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                         function(x) all(!is.na(x))),]
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                         function(x) all(!0)),]
View(portfolioPrices)


# BENCHMARK
BENCHMARK <- c("SP500")
#Renames Columns

tickers <- str_replace(tickers,".SA","")
tickers <- str_replace(tickers,"-","")
tickers <- str_replace(tickers,"=","")
tickers <- str_replace(tickers,"^G","G")
colnames(portfolioPrices) <- tickers
colnames(portfolioPrices)[1] <- BENCHMARK


View(portfolioPrices)

# Calculate Returns: Daily RoC
portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))

scenario.set <- portfolioReturns
scenario.set <- scenario.set[apply(scenario.set,1,
                                   function(x) all(!0)),]
#View(scenario.set)

assets <- ncol(scenario.set)
scenarios <- nrow(scenario.set)

# Plot Charts
cat("\n", paste0(names(scenario.set), "\n"))

chart.Bar(scenario.set$SP500)
charts.PerformanceSummary(scenario.set$SP500)

#########################################










################################################################################
## Morris e Comeau (2020)
## Convers?o das variaveis
# Excesso do retorno em relacao ao benchmark

dados <- scenario.set
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
Resultados_Assim_Curtose = matrix(ncol=nAtivos-1,nrow=4)
Resultados_Assim_Curtose = data.frame(Resultados_Assim_Curtose)
colnames(Resultados_Assim_Curtose)=tickers[-1]
Resultados_Assim_Curtose



################################################################################
#####################################Envelope###################################
################################################################################

for (k in 1:(nAtivos-1)){
  ativo = k+1
  #Envelope

  # Calculo das defasagens para cada ativo
  dat_r <- data.frame(dados[,ativo], Lag(dados[,1],1))
  rownames(dat_r)=rownames(dados)
  colnames(dat_r)[2]="BUFFET"
  defasagem = 5
  for (i in 1:defasagem){
    dat_r[i+2] = Lag(dat_r[1],i)
  }
  ### removendo NAs
  #dat_r = na.fill(dat_r, "extend")
  dat_r = na.omit(dat_r)
  nlinhas = nrow(dat_r)
  View(dat_r)

  ####################### Amostra de Tratamento################################
  # Criando as vari?veis como vetor para treinamento - com datas espec?ficas
  Inicio_data = "2018-01-10"
  Fim_data = "2021-12-30"
  I_data = which(rownames(dat_r)==Inicio_data)
  F_data = which(rownames(dat_r)==Fim_data)
  entradas = as.matrix(dat_r[I_data:F_data,])
  saidas = as.matrix(dat_r[(I_data+1):(F_data+1),1])


  #####
  # Criando as vari?veis como vetor para treinamento -50% dos dados
  #comprimento = ceiling(0.5*nrow(dat_r)) # ceiling arredonda para mais
  #treinamento=as.data.frame(dat_r)
  #nAtivos = ncol(dados)
  #entradas = as.matrix(treinamento[1:comprimento,])
  #saidas = as.matrix(treinamento[2:(comprimento+1),1])
  #I_data = entradas[1]
  #F_data = entradas[length(entradas)]

  #####
  nlinhas <- nrow(entradas)
  ncolunas <- ncol(entradas)

  #############################################################################
  ##Pacote neuralnet()

  library(neuralnet)
  library(zoo)
  library(forecast)
  library(timetk)

  epocas = 7000
  # Fun??o Sigmoide
  sigmoide = function(soma) {
    #return (1/ (1+exp(-soma)))
    #Fun??o Tangente Hiperb?lica
    #return (1-tanh(soma))
    return (tanh(soma))
  }
  colnames(entradas)[1]= "ATIVO"
  nn= neuralnet( ATIVO ~ BUFFET + V3 + V4 + V5 + V6 + V7, data=entradas,
                 hidden = nlinhas, act.fct = "tanh", threshold = 0.01,
                 stepmax=epocas)
  # Plotagem da RNA
  if(nlinhas %% 2 == 0) {
    escondida = nlinhas
  } else {escondida =nlinhas+1}
  nnplot= neuralnet( ATIVO ~ BUFFET + V3 + V4 + V5 + V6 + V7, data=entradas,
                     hidden = escondida, act.fct = "tanh", threshold = 0.01,
                     stepmax=epocas)

  colnames(entradas)[1]=colnames(dados[ativo])
  View(entradas)

  arquivo = colnames(entradas)[1]
  #arquivo = paste(arquivo,"_N.pdf", sep="")
  #str_sub(arquivo, -1) <-"_RNA.pdf"
  #ggsave(arquivo,p1)
  #pdf(file=arquivo, height = 8, width = 9)
  #pdf(file=arquivo)
  # plot(nnplot)
  #dev.off()
  #p1 <- plot(nnplot)
  #p1
  #arquivo <- str_replace(arquivo,"_N","")
  arquivo = paste(arquivo,".pdf", sep="")
  pdf(file=arquivo)
  #dev.print(pdf, file = arquivo)
  #ARQUIVO <- str_replace(arquivo,".pdf",".png")
  #ggsave(ARQUIVO,p1)


  ## Previs?o
  prev = predict(nn, entradas)
  nome = colnames(entradas)[1]
  plot(as.vector(entradas[,1]), type="l", col= "red",
       main = paste("Retornos Amostra de Tratamento - Ativo", xnames = nome))
  legend("topright", legend = c("RNNet", nome), pch = 19,
         col = c("black", "red"))
  lines(prev)

  hist(prev, main = paste("Histograma Fase de Treinamento RNNet Ativo",
                          xnames= nome),
       xlab = paste("Retorno Excedente sobre", xnames = BENCHMARK))
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
                                                                   xnames = BENCHMARK))
  #dev.off()
  #arquivo <- str_replace(arquivo,"pdf","png")
  #png(filename=arquivo)
  hist(Testes$net.result, main = paste("Histograma Train RNA Ativo",
                                       xnames= nome), xlab = paste("Retorno Excedente sobre",
                                                                   xnames = BENCHMARK))
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
  if (mean(prev)>0) {
    ProbabilidadeTmedia =pt(0.0,
                            df=length(prev)-1,ncp = se, lower.tail=FALSE)
    cat("Densidade Assimetrica ? Direita (Negativa)")
  } else {
    ProbabilidadeTmedia =pt(0.0,
                            df=length(prev)-1,ncp = -se, lower.tail=FALSE)
    cat("Densidade Assimetrica ? Esquerda (Positiva)")
  }
  #ProbabilidadeTmedia =pt(mean(prev),
  #                    df=length(prev)-1,lower.tail=TRUE)

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
  pesos0 = matrix(runif(ncolunas*nlinhas, min = 0, max = 1), nrow = ncolunas,
                  ncol = nlinhas, byrow = T)
  pesos1 = matrix(runif(ncolunas*(nlinhas), min = 0, max = 1), nrow = nlinhas,
                  ncol = 1, byrow = T)

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
  epocas = epocas
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
    print(paste('Erro:', mediaAbsoluta))

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
       xlab = paste("Retorno Excedente sobre", xnames = BENCHMARK))
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

  if (kurtosis(camadaSaida)>3) {
    print("Curtose da curva Leptoc?rtica:")
    print(kurtosis(camadaSaida))
  }
  t = 3.373 # p =0.1
  q = -0.4
  ProbtStudent = (q-mean(camadaSaida))/(std.error(camadaSaida))
  #ProbtStudent
  ProbStudent = t*(std.error(camadaSaida)) + mean(camadaSaida)
  #ProbStudent

  # Calculo da probabilidade excesso de retorno >0 c/ deslocamento da curva T
  #ProbabilidadeTmedia =pt(mean(camadaSaida),
  #                       df=length(camadaSaida)-1, lower.tail=FALSE)

  if (mean(camadaSaida)>0) {
    ProbabilidadeTmedia =pt(0.0,
                            df=length(camadaSaida)-1,ncp = se, lower.tail=FALSE)
    cat("Densidade Assimetrica ? Direita (Negativa)")
  } else {
    ProbabilidadeTmedia =pt(0.0,
                            df=length(camadaSaida)-1,ncp = -se, lower.tail=FALSE)
    cat("Densidade Assim?trica ? Esquerda (Positiva)")
  }
  #ProbabilidadeTmedia =pt(mean(camadaSaida),
  #                 df=length(camadaSaida)-1, lower.tail = TRUE)

  Probabilidadest = NULL
  for (l in 1:nlinhas){
    Probabilidadest[l] = pt(camadaSaida[l],
                            df=length(camadaSaida)-1, lower.tail=FALSE)
  }

  # Plot benchmark e a rede neural artificial no teste
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
  #  xlab = paste("Retorno Excedente sobre", xnames = BENCHMARK))

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
  prevPredict = predict(nn, entradasPredict)
  nome = colnames(entradasPredict)[1]

  plot(as.vector(entradasPredict[,1]), type="l", col = "red",
       main = paste("Retornos Amostra de Teste", xnames = nome))
  legend("topright", legend = c("RNA", nome), pch = 19,
         col = c("black", "red"))
  lines(prevPredict)


  hist(prevPredict,
       main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
                    xnames= nome),
       xlab = paste("Retorno Excedente sobre", xnames = BENCHMARK))
  mean(prevPredict)

  TestesPredict = compute(nn, entradasPredict)
  TestesPredict$net.result
  hist(TestesPredict$net.result,
       main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
                    xnames= nome), xlab = paste("Retorno Excedente sobre",
                                                xnames = BENCHMARK))
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
  if (mean(prevPredict)>0) {
    ProbabilidadeTmedia =pt(0.0,
                            df=length(prevPredict)-1,ncp = se,
                            lower.tail=FALSE)
    cat("Densidade Assimetrica ? Direita (Negativa)")
  } else {
    ProbabilidadeTmedia =pt(0.0,
                            df=length(prevPredict)-1,ncp = -se,
                            lower.tail=FALSE)
    cat("Densidade Assimetrica ? Esquerda (Positiva)")
  }
  #ProbabilidadeTmedia =pt(mean(prev),
  #                    df=length(prev)-1,lower.tail=TRUE)

  # Resultados das Probabilidades

  ResProbPosNNetPredict[1,k]= ProbPos
  ResProbTPosNNetPredict[1,k]= ProbabilidadeTmedia

  ####### Carteira Particular#################
  # fed forward
  camadaEntradaPredict = as.matrix(entradasPredict)
  somaSinapse0Predict = camadaEntradaPredict %*% pesos0
  camadaOcultaPredict = sigmoide(somaSinapse0Predict)


  somaSinapse1Predict = camadaOcultaPredict %*% pesos1
  camadaSaidaPredict = sigmoide(somaSinapse1Predict)

  #####################################Envelope###################################
  ################################################################################
  # excede o Tratamento at? o ?ltimo preg?o
  saidasPredicao_test[1,k+1] = dat_r[nrow(dat_r),7]
  saidasPredicao_test[2,k+1] = dat_r[nrow(dat_r),6]
  saidasPredicao_test[3,k+1] = dat_r[nrow(dat_r),5]
  saidasPredicao_test[4,k+1] = dat_r[nrow(dat_r),4]
  saidasPredicao_test[5,k+1] = dat_r[nrow(dat_r),3]
  saidasPredicao_test[6,k+1] = dat_r[nrow(dat_r),2]
  saidasPredicao_test[7,k+1] = dat_r[nrow(dat_r),1]
  entradasPredict_2 = c(saidasPredicao_test[1,k+1],
                        saidasPredicao_test[2,k+1],
                        saidasPredicao_test[3,k+1],
                        saidasPredicao_test[4,k+1],
                        saidasPredicao_test[5,k+1],
                        saidasPredicao_test[6,k+1],
                        saidasPredicao_test[7,k+1])
  camadaEntradaPredict_2 = t(as.matrix(entradasPredict_2))
  somaSinapse0Predict_2 = camadaEntradaPredict_2 %*% pesos0
  camadaOcultaPredict_2 = sigmoide(somaSinapse0Predict_2)


  somaSinapse1Predict_2 = camadaOcultaPredict_2 %*% pesos1
  camadaSaidaPredict_2 = sigmoide(somaSinapse1Predict_2)

  saidasPredicao_test[8,k+1]= camadaSaidaPredict_2
  #F_dataPredict = nrow(dat_r)-1
  #entradasPredict = as.matrix(dat_r[I_dataPredict:F_dataPredict,])
  #saidasPredict = as.matrix(dat_r[(I_dataPredict+1):(F_dataPredict+1),1])


  for (kL in 9:(linhas)){
    #Envelope



    # Calculo das defasagens para cada ativo
    #dat_r <- data.frame(dados[,ativo], Lag(dados[,1],1))
    #colnames(dat_r)[2]="SP500"
    #defasagem = 5
    #for (i in 1:defasagem){
    #  dat_r[i+2] = Lag(dat_r[1],i)
    #}
    ### removendo NAs
    #dat_r = na.fill(dat_r, "extend")
    #dat_r = na.omit(dat_r)
    #nlinhas = nrow(dat_r)
    #View(dat_r)

    ####################### Amostra de Tratamento################################
    # Criando as vari?veis como vetor para treinamento - com datas espec?ficas
    #Inicio_data = "2018-01-10"
    #Fim_data = "2021-05-20"
    #I_data = which(rownames(dat_r)==Inicio_data)
    #F_data = which(rownames(dat_r)==Fim_data)
    #entradas = as.matrix(dat_r[I_data:F_data,])
    #saidas = as.matrix(dat_r[(I_data+1):(F_data+1),1])



    #colnames(entradas)[1]=colnames(dados[,ativo])


    #nome = colnames(entradas)[1]


    ####################### Amostra de Teste ###################################
    # Criando as vari?veis como vetor para teste- com o restantes da amostra que
    # excede o Tratamento at? o ?ltimo preg?o
    #I_dataPredict = F_data+1
    #F_dataPredict = nrow(dat_r)-1
    #entradasPredict = as.matrix(dat_r[I_dataPredict:F_dataPredict,])
    #saidasPredict = as.matrix(dat_r[(I_dataPredict+1):(F_dataPredict+1),1])


    #####
    #nlinhasPredict <- nrow(entradasPredict)
    #ncolunasPredict <- ncol(entradasPredict)

    ####### Carteira Particular#################
    # fed forward
    entradasPredict_2 = c(saidasPredicao_test[kL-7,k+1],
                          saidasPredicao_test[kL-6,k+1],
                          saidasPredicao_test[kL-5,k+1],
                          saidasPredicao_test[kL-4,k+1],
                          saidasPredicao_test[kL-3,k+1],
                          saidasPredicao_test[kL-2,k+1],
                          saidasPredicao_test[kL-1,k+1])
    camadaEntradaPredict_2 = t(as.matrix(entradasPredict_2))
    somaSinapse0Predict_2 = camadaEntradaPredict_2 %*% pesos0
    camadaOcultaPredict_2 = sigmoide(somaSinapse0Predict_2)


    somaSinapse1Predict_2 = camadaOcultaPredict_2 %*% pesos1
    camadaSaidaPredict_2 = sigmoide(somaSinapse1Predict_2)

    saidasPredicao_test[kL,k+1]= camadaSaidaPredict_2

  }

  camadaSaidaPredict = as.matrix(saidasPredicao_test[8:linhas,k+1])
  ################################################################################

  hist(camadaSaidaPredict,
       main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
                    xnames= nome),
       xlab = paste("Retorno Excedente sobre", xnames = BENCHMARK))
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

  if (kurtosis(camadaSaidaPredict)>3) {
    print("Curtose da curva Leptoc?rtica:")
    print(kurtosis(camadaSaidaPredict))
  }
  t = 3.373 # p =0.1
  q = -0.4
  ProbtStudentPredict=(q-mean(camadaSaidaPredict))/(std.error(camadaSaidaPredict))
  #ProbtStudent
  ProbStudentPredict= t*(std.error(camadaSaidaPredict)) + mean(camadaSaidaPredict)
  #ProbStudent

  # Calculo da probabilidade excesso de retorno >0 c/ deslocamento da curva T
  #ProbabilidadeTmedia =pt(mean(camadaSaida),
  #                       df=length(camadaSaida)-1, lower.tail=FALSE)

  if (mean(camadaSaidaPredict)>0) {
    ProbabilidadeTmedia =pt(0.0,
                            df=length(camadaSaidaPredict)-1,ncp = se,
                            lower.tail=FALSE)
    cat("Densidade Assimetrica ? Direita (Negativa)")
  } else {
    ProbabilidadeTmedia =pt(0.0,
                            df=length(camadaSaidaPredict)-1,ncp = -se,
                            lower.tail=FALSE)
    cat("Densidade Assim?trica ? Esquerda (Positiva)")
  }
  #ProbabilidadeTmedia =pt(mean(camadaSaida),
  #                 df=length(camadaSaida)-1, lower.tail = TRUE)

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
  Resultados_Assim_Curtose[1,k]= ProbabilidadeTmedia
  Resultados_Assim_Curtose[2,k]= kurtosis(camadaSaidaPredict)
  Resultados_Assim_Curtose[3,k]= skewness(camadaSaidaPredict)
  Resultados_Assim_Curtose[4,k]= mean(camadaSaidaPredict)

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

## Train
order(ResProbPosNNet)
Test <- sort(ResProbPosNNet, decreasing=TRUE)

ResProbPosNNEtx <- t(Test)
ResProbPosNNEtx <- data.frame(ResProbPosNNEtx)
TesteNNet <- ResProbPosNNEtx %>% dplyr::filter(ResProbPosNNEtx > 0.0)

TesteNNet <- t(TesteNNet)
###

order(ResProbPos)
Test2 <- sort(ResProbPos, decreasing=TRUE)

ResProbPosx <- t(Test2)
ResProbPosx <- data.frame(ResProbPosx)
TestePos <- ResProbPosx %>% dplyr::filter(ResProbPosx>0.0)
TestePos <- t(TestePos)
###

order(ResProbTPosNNet)
TestT <- sort(ResProbTPosNNet, decreasing=TRUE)

ResProbTPosNNEtx <- t(TestT)
ResProbTPosNNEtx <- data.frame(ResProbTPosNNEtx)
TesteTNNet <- ResProbTPosNNEtx %>% dplyr::filter(ResProbTPosNNEtx>0.0)
#TesteTNNet <- filter(ResProbTPosNNEtx, ResProbTPosNNEtx>0.5)
TesteTNNet <- t(TesteTNNet)

###
order(ResProbTPos)
Test2T <- sort(ResProbTPos, decreasing=TRUE)

ResProbTPosx <- t(Test2T)
ResProbTPosx <- data.frame(ResProbTPosx)
TesteTPos <- ResProbTPosx %>% dplyr::filter(ResProbTPosx>0.0)
TesteTPos <- t(TesteTPos)

###
### Test
order(ResProbPosNNetPredict)
TestPredict <- sort(ResProbPosNNetPredict, decreasing=TRUE)

ResProbPosNNEtxPredict <- t(TestPredict)
ResProbPosNNEtxPredict <- data.frame(ResProbPosNNEtxPredict)
TesteNNetPredict <- ResProbPosNNEtxPredict %>%
  dplyr::filter(ResProbPosNNEtxPredict>0.0)
TesteNNetPredict <- t(TesteNNetPredict)
###

order(ResProbPosPredict)
Test2Predict <- sort(ResProbPosPredict, decreasing=TRUE)

ResProbPosxPredict <- t(Test2Predict)
ResProbPosxPredict <- data.frame(ResProbPosxPredict)
TestePosPredict <- ResProbPosxPredict %>% dplyr::filter(ResProbPosxPredict>0.0)
TestePosPredict <- t(TestePosPredict)
###

order(ResProbTPosNNetPredict)
TestTPredict <- sort(ResProbTPosNNetPredict, decreasing=TRUE)

ResProbTPosNNEtxPredict <- t(TestTPredict)
ResProbTPosNNEtxPredict <- data.frame(ResProbTPosNNEtxPredict)
TesteTNNetPredict <- ResProbTPosNNEtxPredict %>%
  dplyr::filter(ResProbTPosNNEtxPredict>0.0)
#TesteTNNetPredict<-filter(ResProbTPosNNEtxPredict, ResProbTPosNNEtxPredict>0.5)
TesteTNNetPredict <- t(TesteTNNetPredict)
###
#### RNA-t Particular para armazenar em T8
order(ResProbTPosPredict)
Test2TPredict <- sort(ResProbTPosPredict, decreasing=TRUE)

ResProbTPosxPredict <- t(Test2TPredict)
ResProbTPosxPredict <- data.frame(ResProbTPosxPredict)
TesteTPosPredict <- ResProbTPosxPredict %>% dplyr::filter(ResProbTPosxPredict>0.0)
TesteTPosPredict <- t(TesteTPosPredict)

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


###############################



















################################################################################
# Comparativo com a Carteira de Warren Buffet
# Carteira de Buffet - 7 ativos - 80%
scenario.set = data.frame(scenario.set)

### Short term Analysis
Datas1Predict = rownames(scenario.set)[
  (which(rownames(scenario.set)=="2021-12-31")):(nrow(scenario.set)-1)]
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))

### Long Term Analysis
Datas1Predict = rownames(scenario.set)[
  (which(rownames(scenario.set)=="2020-01-21")):
    (which(rownames(scenario.set)=="2022-08-04"))]
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,]))


PosCovidSP500 = as.matrix(portfolioReturns[Datas1Predict,1])
Buffet = c("AAPL", "BAC", "KO", "AXP", "CVX", "KHC", "OXY") # PORTFOLIO_2022
PesosBuffet = c( 0.414, 0.102, 0.073, 0.068, 0.068, 0.037, 0.033) # PORTFOLIO_2022
Buffet = c("AAPL", "BAC", "CVX", "KO", "AXP", "KHC", "OXY", "MCO") # PORTFOLIO_2023
PesosBuffet = c( 0.441, 0.089, 0.079, 0.075, 0.074, 0.038, 0.038, 0.023) # PORTFOLIO_2023
sum(PesosBuffet)
PesosBuffetNormalizado = PesosBuffet/sum(PesosBuffet)
CarteiraBuffet = as.data.frame(scenario.set) %>%
  dplyr::select(which((colnames(scenario.set) %in% Buffet)))
PosCovidBuffet = CarteiraBuffet[Datas1Predict,]
View(PosCovidBuffet)
#AAPL <- getSymbols.yahoo("AAPL", from="2010-01-01",
#                                         auto.assign=FALSE)[,4]
#AAPLretornos<- na.omit(ROC(AAPL, type="discrete"))
#colnames(AAPLretornos)="AAPL"
#PosCovidAAPLRetornos = AAPLretornos[Datas1Predict]
#PosCovidBuffet2 = cbind(PosCovidBuffet, PosCovidAAPLRetornos)
#View(PosCovidBuffet2)

#AAPL <- getSymbols.yahoo("AAPL", from="2018-01-09",
#       auto.assign=FALSE)[,4]
#AAPLretornos<- na.omit(ROC(AAPL, type="discrete"))
#colnames(AAPLretornos)="AAPL"
#PosCovidAAPLRetornos = AAPLretornos[Datas1Predict]

TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))
#TodosAtivosPredict1 = cbind(as.data.frame(PosCovidAAPLRetornos),TodosAtivosPredict)
#TodosAtivosPredict = TodosAtivosPredict1

#View(PosCovidBuffet2)
RetornosMediosBuffet = as.matrix(PosCovidBuffet) %*% PesosBuffetNormalizado

# Carteira RNA NNet dist t com pesos iguais para Comparação
#T76_Carteira = c("SNPS", "BIIB", "GNRC", "TPR", "DG", "VTRS", "COST")

#Carteira_T6 = as.data.frame(scenario.set) %>%
#  dplyr::select(which((colnames(scenario.set) %in% T6_Carteira)))
#T6 = Carteira_T6[Datas1Predict,]


CarteiraComparativa = colnames(T8[1:8])
C_Net_T_comparativa = as.data.frame(scenario.set) %>%
  dplyr::select(which((colnames(scenario.set) %in% CarteiraComparativa)))
C_Net_T_comparativa = C_Net_T_comparativa[Datas1Predict,]
PesosComparativos = c(rep(1/8,8))
Media_C_Net_T_Comparativa = as.matrix(C_Net_T_comparativa) %*% PesosComparativos

# Carteira de Markovitz de Minima Variância obtida a partir de todos ativos
TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))
pesos_todosPredict <- round(tseries::portfolio.optim(TodosAtivosPredict)$pw, 2)
RetornoMedioMArkovitz = TodosAtivosPredict%*% pesos_todosPredict

#### Carteira de M?dia Vari?ncia de Markowitz
library(CVXR)
Pesos_mean_variance_portfolio <- function(X) {
  mu  <- colMeans(X)
  # compute mean vector
  Sigma <- cov(X)
  # compute the SCM
  # design mean-variance portfolio
  w <-Variable(nrow(Sigma))
  prob<-Problem(Maximize(t(mu)%*% w-0.5 * quad_form(w, Sigma)),
                constraints = list(w>=0,sum(w)==1))
  result  <- solve(prob)
  return( as.vector(result$getValue(w)))
}

###############
symbols = colnames(TodosAtivosPredict)
max_exp_return_portfolio <- PortfolioAnalytics::portfolio.spec(assets = symbols)
max_exp_return_portfolio <- PortfolioAnalytics::add.constraint(
  portfolio = max_exp_return_portfolio,
  type = "full_investment",

)
max_exp_return_portfolio <- PortfolioAnalytics::add.constraint(
  portfolio = max_exp_return_portfolio,
  type = "box", min = 0.01, max = 0.9
)

# Add objective to maximize mean returns
max_exp_return_portfolio <- PortfolioAnalytics::add.objective(
  portfolio = max_exp_return_portfolio,
  # Maximize expected returns
  type = "return",
  # A character corresponding to a function name, mean()
  name = "mean"
)

dias = rownames(as.data.frame(TodosAtivosPredict))
TodosAtivosPredict_xts = mutate(as.data.frame(dias),
                                as.data.frame(TodosAtivosPredict))
TodosAtivosPredict_xts = as.timeSeries(TodosAtivosPredict_xts)

global_max_portfolio <- PortfolioAnalytics::optimize.portfolio(
  R = TodosAtivosPredict_xts,
  portfolio = max_exp_return_portfolio,
  # This defaults to the "glpk" solver
  optimize_method = "glpk",
  # Return additional information on the path or portfolios searched
  trace = TRUE
)

optimize.portfolio(R = TodosAtivosPredict, portfolio = max_exp_return_portfolio, optimize_method = "ROI",
                   trace = TRUE)


################33
Pesos_MeanVP <- Pesos_mean_variance_portfolio(TodosAtivosPredict)
RetornoMedioMean_Variance_Mkv = TodosAtivosPredict%*% Pesos_MeanVP
mean(RetornoMedioMean_Variance_Mkv)
sd(RetornoMedioMean_Variance_Mkv)

# Carteira RNA NNet dist T com pesos de Markovitz  para Comparação
pesos_MarkovitzNNet_T <- round(tseries::portfolio.optim(
  as.matrix(C_Net_T_comparativa))$pw, 2)
Ret_Medio_RNA_T_Mkv = as.matrix(C_Net_T_comparativa) %*% pesos_MarkovitzNNet_T


##### Carteira de Sharpe para todos os Ativos
## Optmization
symbols = colnames(TodosAtivosPredict)
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
maxSRport.rp <- optimize.portfolio(R=TodosAtivosPredict,
                                   portfolio = port1,
                                   optimize_method = "random",
                                   search_size = 2000,
                                   maxSR=TRUE, trace = TRUE)
maxSRport.rp

maxSR.weight.rp <- extractWeights(maxSRport.rp)

### Retornos carteira Sharpe todos os ativos
RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% maxSR.weight.rp




### Carteira Sharpe RNAt
## Optmization
symbols_RNAt = colnames(C_Net_T_comparativa)
init.portf <- portfolio.spec(assets = symbols_RNAt)
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



maxSRport.rp.RNAt<- optimize.portfolio(R=C_Net_T_comparativa,
                                       portfolio = port1,
                                       optimize_method = "random",
                                       search_size = 2000,
                                       maxSR=TRUE, trace = TRUE)
maxSRport.rp.RNAt

maxSR.weight.rp.RNAt <- extractWeights(maxSRport.rp.RNAt)

### Retornos carteira Sharpe RNAt
RetornoMedioMaxIS_RNAt = as.matrix(C_Net_T_comparativa)%*% maxSR.weight.rp.RNAt



all.returns <- TodosAtivosPredict
## set up portfolio with objetive and constraints
n.assets <- length(colnames(all.returns))
port.sec <- portfolio.spec(assets = colnames(all.returns))
port.sec <- add.objective(portfolio = port.sec, type = "risk", name = "StdDev")
port.sec <- add.objective(portfolio = port.sec, type = "return", name = "mean")
port.sec <- add.constraint(portfolio = port.sec, type = "full_investiment")
port.sec <- add.constraint(portfolio = port.sec, type = "box", min = 0, max = 1)

# map off efficient frontier (for variance risk)
eff.frontier <- create.EfficientFrontier(R = all.returns, portfolio = port.sec,
                                         n.portfolio = 2000, type = "mean-StdDev")

# Daily Sharpe ratio
rf=(1+0.0571)^(1/252)-1
sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf)/eff.frontier$frontier[,"StdDev"]
max.sharpe.ratio <- sharpe.ratios[sharpe.ratios==max(sharpe.ratios)]
optimal.port.name <- names(max.sharpe.ratio)
optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

n.trading.days.per.year <- 1

print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
print(sprintf("Optimal E(port return): %f", optimal.mean*sqrt(n.trading.days.per.year)))
mean_sharpe = optimal.mean*sqrt(n.trading.days.per.year)
print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
sd_sharpe <- optimal.sd*sqrt(n.trading.days.per.year)
print("Optimal weights")
weight_test <- eff.frontier$frontier[optimal.port.name,(1:n.assets)+3]
weight_test <- round(weight_test,4)
weight_Sharpe= weight_test[which(weight_test !=0)]
weight_Sharpe

### Retornos carteira Sharpe todos os ativos
#RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% maxSR.weight.rp
RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% weight_test

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
rf=(1+0.0571)^(1/252)-1
sharpe.ratios_RNA_t <- (eff.frontier_RNA_t$frontier[,"mean"]-rf)/eff.frontier_RNA_t$frontier[,"StdDev"]
max.sharpe.ratio_RNA_t <- sharpe.ratios_RNA_t[sharpe.ratios_RNA_t==max(sharpe.ratios_RNA_t)]
optimal.port.name.RNAt <- names(max.sharpe.ratio_RNA_t)
optimal.mean.RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,"mean"]
optimal.sd.RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,"StdDev"]

n.trading.days.per.year.RNAt <- 1

print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio_RNA_t*sqrt(n.trading.days.per.year.RNAt)))
print(sprintf("Optimal E(port return): %f", optimal.mean.RNAt*sqrt(n.trading.days.per.year.RNAt)))
mean_sharpe_RNA_t = optimal.mean.RNAt*sqrt(n.trading.days.per.year.RNAt)
print(sprintf("Optimal sd(port return): %f", optimal.sd.RNAt*sqrt(n.trading.days.per.year.RNAt)))
sd_sharpe_RNA_t <- optimal.sd.RNAt*sqrt(n.trading.days.per.year.RNAt)
print("Optimal weights")
weight_test_RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,(1:n.assets.RNAt)+3]
weight_test_RNAt <- round(weight_test_RNAt,4)
weight_Sharpe_RNA_t= weight_test_RNAt[which(weight_test_RNAt !=0)]
weight_Sharpe_RNA_t

RetornoMedioMaxIS_RNAt = as.matrix(C_Net_T_comparativa)%*% weight_test_RNAt


##

# Geração da Matriz de comparação dos Retornos
Comparativo_RETORNOS = matrix(nrow=length(RetornosMediosBuffet), ncol=7)
Comparativo_RETORNOS[,1] = RetornosMediosBuffet
Comparativo_RETORNOS[,2] = PosCovidSP500
Comparativo_RETORNOS[,3] = Media_C_Net_T_Comparativa
Comparativo_RETORNOS[,4] = RetornoMedioMArkovitz
Comparativo_RETORNOS[,5] = Ret_Medio_RNA_T_Mkv
Comparativo_RETORNOS[,6] = RetornoMedioMaxIS
Comparativo_RETORNOS[,7] = RetornoMedioMaxIS_RNAt
#Comparativo_RETORNOS[,6] = RetornoMedioMean_Variance_Mkv

colnames(Comparativo_RETORNOS)= c("Buffet","SP500", "RNAt_Eq", "Markowitz",
                                  "RNAt_Mkv", "MaxSharpe", "RNAt_Sharpe")
rownames(Comparativo_RETORNOS) = rownames(PosCovidBuffet)
Datas_Comparativo_RETORNOS = rownames(as.data.frame(Comparativo_RETORNOS))
Comparativos_RETORNOS_Df = mutate(as.data.frame(Datas_Comparativo_RETORNOS),
                                  as.data.frame(Comparativo_RETORNOS))
write_xlsx(Comparativos_RETORNOS_Df, "RetornosRNAxBUFFET_Sharpe_Curto_Prazo.xlsx")
write_xlsx(Comparativos_RETORNOS_Df, "RetornosRNAxBUFFET_Sharpe_Longo_Prazo.xlsx")

################################################################################





#### Backtesting

## Medias dos retornos
Media_RetornosMediosBuffet = sum(RetornosMediosBuffet)/length((RetornosMediosBuffet))
Media_SP500 <- sum(PosCovidSP500)/length((PosCovidSP500))
Media_Media_C_Net_T_Comparativa = sum(Media_C_Net_T_Comparativa)/length(Media_C_Net_T_Comparativa)
Media_RetornoMedioMArkovitz = sum(RetornoMedioMArkovitz)/length(RetornoMedioMArkovitz)
Media_Ret_Medio_RNA_Mkv = sum(Ret_Medio_RNA_T_Mkv)/length(Ret_Medio_RNA_T_Mkv)
#Media_Ret_Mean_Var = mean(RetornoMedioMean_Variance_Mkv)
Media_RetornoMedioMaxIS = mean(RetornoMedioMaxIS)
Media_RetornoMedioMaxIS_RNAt = mean(RetornoMedioMaxIS_RNAt)

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
Rf = 0.0426
IS = function(X) {
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

Alfa = function(X,Y,Rf) {
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
Rf = 0.0426
ITreynor = function(X,Y,Rf) {
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
sumbacktest <- matrix(nrow=7, ncol=11)
sumbacktest[1,1]= round(Media_RetornosMediosBuffet,4)*100
sumbacktest[2,1]= round(Media_SP500,4)*100
sumbacktest[3,1]= round(Media_Media_C_Net_T_Comparativa,4)*100
sumbacktest[4,1]= round(Media_RetornoMedioMArkovitz,4)*100
sumbacktest[5,1]= round(Media_Ret_Medio_RNA_Mkv,4)*100
sumbacktest[6,1]= round(Media_RetornoMedioMaxIS,4)*100
sumbacktest[7,1]= round(Media_RetornoMedioMaxIS_RNAt,4)*100
#sumbacktest[8,1]= round(Media_Ret_Mean_Var,4)*100

sumbacktest[1,2]= round(AVERAGE_Return(RetornosMediosBuffet),4)*100
sumbacktest[2,2]= round(AVERAGE_Return(PosCovidSP500),4)*100
sumbacktest[3,2]= round(AVERAGE_Return(Media_C_Net_T_Comparativa),4)*100
sumbacktest[4,2]= round(AVERAGE_Return(RetornoMedioMArkovitz),4)*100
sumbacktest[5,2]= round(AVERAGE_Return(Ret_Medio_RNA_T_Mkv),4)*100
sumbacktest[6,2]= round(AVERAGE_Return(RetornoMedioMaxIS),4)*100
sumbacktest[7,2]= round(AVERAGE_Return(RetornoMedioMaxIS_RNAt),4)*100
#sumbacktest[8,2]= round(AVERAGE_Return(RetornoMedioMean_Variance_Mkv),4)*100

sumbacktest[1,3]= round(CUMULATIVE_Return(RetornosMediosBuffet),4)*100
sumbacktest[2,3]= round(CUMULATIVE_Return(PosCovidSP500),4)*100
sumbacktest[3,3]= round(CUMULATIVE_Return(Media_C_Net_T_Comparativa),4)*100
sumbacktest[4,3]= round(CUMULATIVE_Return(RetornoMedioMArkovitz),4)*100
sumbacktest[5,3]= round(CUMULATIVE_Return(Ret_Medio_RNA_T_Mkv),4)*100
sumbacktest[6,3]= round(CUMULATIVE_Return(RetornoMedioMaxIS),4)*100
sumbacktest[7,3]= round(CUMULATIVE_Return(RetornoMedioMaxIS_RNAt),4)*100
#sumbacktest[8,3]= round(CUMULATIVE_Return(RetornoMedioMean_Variance_Mkv),4)*100

sumbacktest[1,4]= round(ANNUALIZED_VOLATILITY(RetornosMediosBuffet),4)*100
sumbacktest[2,4]= round(ANNUALIZED_VOLATILITY(PosCovidSP500),4)*100
sumbacktest[3,4]= round(ANNUALIZED_VOLATILITY(Media_C_Net_T_Comparativa),4)*100
sumbacktest[4,4]= round(ANNUALIZED_VOLATILITY(RetornoMedioMArkovitz),4)*100
sumbacktest[5,4]= round(ANNUALIZED_VOLATILITY(Ret_Medio_RNA_T_Mkv),4)*100
sumbacktest[6,4]= round(ANNUALIZED_VOLATILITY(RetornoMedioMaxIS),4)*100
sumbacktest[7,4]= round(ANNUALIZED_VOLATILITY(RetornoMedioMaxIS_RNAt),4)*100
#sumbacktest[8,4]= round(ANNUALIZED_VOLATILITY(RetornoMedioMean_Variance_Mkv),4)*100

sumbacktest[1,5]= round(Var_95_(RetornosMediosBuffet),4)*100
sumbacktest[2,5]= round(Var_95_(PosCovidSP500),4)*100
sumbacktest[3,5]= round(Var_95_(Media_C_Net_T_Comparativa),4)*100
sumbacktest[4,5]= round(Var_95_(RetornoMedioMArkovitz),4)*100
sumbacktest[5,5]= round(Var_95_(Ret_Medio_RNA_T_Mkv),4)*100
sumbacktest[6,5]= round(Var_95_(RetornoMedioMaxIS),4)*100
sumbacktest[7,5]= round(Var_95_(RetornoMedioMaxIS_RNAt),4)*100
#sumbacktest[8,5]= round(Var_95_(RetornoMedioMean_Variance_Mkv),4)*100

sumbacktest[1,6]= round(CVar_95(RetornosMediosBuffet),4)*100
sumbacktest[2,6]= round(CVar_95(PosCovidSP500),4)*100
sumbacktest[3,6]= round(CVar_95(Media_C_Net_T_Comparativa),4)*100
sumbacktest[4,6]= round(CVar_95(RetornoMedioMArkovitz),4)*100
sumbacktest[5,6]= round(CVar_95(Ret_Medio_RNA_T_Mkv),4)*100
sumbacktest[6,6]= round(CVar_95(RetornoMedioMaxIS),4)*100
sumbacktest[7,6]= round(CVar_95(RetornoMedioMaxIS_RNAt),4)*100
#sumbacktest[8,6]= round(CVar_95(RetornoMedioMean_Variance_Mkv),4)*100

sumbacktest[1,7]= round(IS(RetornosMediosBuffet),2)
sumbacktest[2,7]= round(IS(PosCovidSP500),2)
sumbacktest[3,7]= round(IS(Media_C_Net_T_Comparativa),2)
sumbacktest[4,7]= round(IS(RetornoMedioMArkovitz),2)
sumbacktest[5,7]= round(IS(Ret_Medio_RNA_T_Mkv),2)
sumbacktest[6,7]= round(IS(RetornoMedioMaxIS),2)
sumbacktest[7,7]= round(IS(RetornoMedioMaxIS_RNAt),2)
#sumbacktest[8,7]= round(IS(RetornoMedioMean_Variance_Mkv),4)

sumbacktest[1,8]= round(SortinoRatio_my(RetornosMediosBuffet),2)
sumbacktest[2,8]= round(SortinoRatio_my(PosCovidSP500),2)
sumbacktest[3,8]= round(SortinoRatio_my(Media_C_Net_T_Comparativa),2)
sumbacktest[4,8]= round(SortinoRatio_my(RetornoMedioMArkovitz),2)
sumbacktest[5,8]= round(SortinoRatio_my(Ret_Medio_RNA_T_Mkv),2)
sumbacktest[6,8]= round(SortinoRatio_my(RetornoMedioMaxIS),2)
sumbacktest[7,8]= round(SortinoRatio_my(RetornoMedioMaxIS_RNAt),2)
#sumbacktest[8,8]= round(SortinoRatio_my(RetornoMedioMean_Variance_Mkv),4)

sumbacktest[1,9]= round(Beta(RetornosMediosBuffet, PosCovidSP500),2)
sumbacktest[2,9]= round(Beta(PosCovidSP500, PosCovidSP500),2)
sumbacktest[3,9]= round(Beta(Media_C_Net_T_Comparativa, PosCovidSP500),2)
sumbacktest[4,9]= round(Beta(RetornoMedioMArkovitz, PosCovidSP500),2)
sumbacktest[5,9]= round(Beta(Ret_Medio_RNA_T_Mkv, PosCovidSP500),2)
sumbacktest[6,9]= round(Beta(RetornoMedioMaxIS, PosCovidSP500),2)
sumbacktest[7,9]= round(Beta(RetornoMedioMaxIS_RNAt, PosCovidSP500),2)
sumbacktest[7,9]= round(Beta(RetornoMedioMaxIS_RNAt, PosCovidSP500),2)
#sumbacktest[8,9]= round(SortinoRatio_my(RetornoMedioMean_Variance_Mkv),4)

sumbacktest[1,10]= round(Alfa(RetornosMediosBuffet, PosCovidSP500, 0.0426)*100,2)
sumbacktest[2,10]= round(Alfa(PosCovidSP500, PosCovidSP500, 0.0426)*100,2)
sumbacktest[3,10]= round(Alfa(Media_C_Net_T_Comparativa, PosCovidSP500, 0.0426)*100,2)
sumbacktest[4,10]= round(Alfa(RetornoMedioMArkovitz, PosCovidSP500, 0.0426)*100,2)
sumbacktest[5,10]= round(Alfa(Ret_Medio_RNA_T_Mkv, PosCovidSP500, 0.0426)*100,2)
sumbacktest[6,10]= round(Alfa(RetornoMedioMaxIS, PosCovidSP500, 0.0426)*100,2)
sumbacktest[7,10]= round(Alfa(RetornoMedioMaxIS_RNAt, PosCovidSP500, 0.0426)*100,2)
#sumbacktest[8,10]= round(Alfa(RetornoMedioMean_Variance_Mkv, PosCovidSP500, 0.0426)*100,2)


sumbacktest[1,11]= round(ITreynor(RetornosMediosBuffet, PosCovidSP500, 0.0426)*100,2)
sumbacktest[2,11]= round(ITreynor(PosCovidSP500, PosCovidSP500, 0.0426)*100,2)
sumbacktest[3,11]= round(ITreynor(Media_C_Net_T_Comparativa, PosCovidSP500, 0.0426)*100,2)
sumbacktest[4,11]= round(ITreynor(RetornoMedioMArkovitz, PosCovidSP500, 0.0426)*100,2)
sumbacktest[5,11]= round(ITreynor(Ret_Medio_RNA_T_Mkv, PosCovidSP500, 0.0426)*100,2)
sumbacktest[6,11]= round(ITreynor(RetornoMedioMaxIS, PosCovidSP500, 0.0426)*100,2)
sumbacktest[7,11]= round(ITreynor(RetornoMedioMaxIS_RNAt, PosCovidSP500, 0.0426)*100,2)
#sumbacktest[8,11]= round(ITreynor(RetornoMedioMean_Variance_Mkv, PosCovidSP500, 0.0426)*100,2)

rownames(sumbacktest)= c("Buffet","SP500", "ANNt_Eq", "Markowitz", "ANNt_Mkv",
                         "MaxSharpe", "ANNt_Sharpe")
colnames(sumbacktest) = c("Average Return (% a.d.)","Annualized Return (% a.a.)",
                          "Cumulative Return (% a.p.)", "Annualized Volatility (% a.a.)",
                          "VaR 95% (% a.d.)", "CVaR 95% (% a.d.)",
                          "Sharpe Ratio (Dimensionless)", "Sortino Ratio",
                          "CAPM Beta", "Alfa Jensen (% a.d.)", "Treynor (%a.d.)")
View(sumbacktest)
Portfolio = rownames(as.data.frame(sumbacktest))
SUMBACKTEST_Df = mutate(as.data.frame(Portfolio),
                        as.data.frame(sumbacktest))
write_xlsx(SUMBACKTEST_Df, "SUMBACKTEST_Buffet_Sharpe.xlsx")

# Geração da Matriz de comparação dos Retornos Acumulados
Comparativo = matrix(nrow=length(RetornosMediosBuffet), ncol=7)
Comparativo[1,1] = RetornosMediosBuffet[1,]
Comparativo[1,2] = PosCovidSP500[1,]
Comparativo[1,3] = Media_C_Net_T_Comparativa[1,]
Comparativo[1,4] = RetornoMedioMArkovitz[1,]
Comparativo[1,5] = Ret_Medio_RNA_T_Mkv [1,]
Comparativo[1,6] = RetornoMedioMaxIS [1,]
Comparativo[1,7] = RetornoMedioMaxIS_RNAt [1,]

for(i in 2:length(RetornosMediosBuffet)) {
  Comparativo[i,1] = (Comparativo[i-1,1]+1)*(RetornosMediosBuffet[i,]+1)-1
  Comparativo[i,2] = (as.matrix(Comparativo[i-1,2])+1)*
    (as.matrix(PosCovidSP500[i,])+1)-1
  Comparativo[i,3] = (as.matrix(Comparativo[i-1,3])+1)*
    (as.matrix(Media_C_Net_T_Comparativa [i,])+1)-1
  Comparativo[i,4] = (as.matrix(Comparativo[i-1,4])+1)*
    (as.matrix(RetornoMedioMArkovitz [i,])+1)-1
  Comparativo[i,5] = (as.matrix(Comparativo[i-1,5])+1)*
    (as.matrix(Ret_Medio_RNA_T_Mkv[i,])+1)-1
  Comparativo[i,6] = (as.matrix(Comparativo[i-1,6])+1)*
    (as.matrix(RetornoMedioMaxIS[i,])+1)-1
  Comparativo[i,7] = (as.matrix(Comparativo[i-1,7])+1)*
    (as.matrix(RetornoMedioMaxIS_RNAt[i,])+1)-1
}
colnames(Comparativo)= c("Buffet","SP500", "ANNt_Eq", "Markowitz", "ANNt_Mkv",
                         "MaxSharpe", "ANNt_Sharpe")
rownames(Comparativo) = rownames(PosCovidBuffet)
#################################################################################

















##Gráfico Comparativo dos Retornos Acumulados das Carteiras

Eixo_X = rownames(PosCovidBuffet)
nline = nrow(Comparativo)
Comparativo = as.data.frame(Comparativo)
nline = nrow(Comparativo)
pdf("Comparativo.pdf")
cores <- c("black", "red", "blue", "green", "darkgreen", "brown", "gray")
matplot(cbind(Comparativo$SP500,
              Comparativo$Buffet,
              Comparativo$ANNt_Eq,
              Comparativo$ANNt_Mkv,
              Comparativo$Markovitz,
              Comparativo$MaxSharpe,
              Comparativo$ANNt_Sharpe),
        type = "l", lwd = 1, xaxt = "n", col = cores,
        xlab = paste("Periodo"),
        ylab = "Retornos Acumulados",
        main = paste("Comparativo dos Retornos Acumulados"))
legend("bottomright", cex= 0.7, legend = c("SP500",
                                           "Buffet",
                                           "ANNt_Eq",
                                           "ANNt_Mkw",
                                           "ANNt_Sharpe",
                                           "Markowitz",
                                           "Sharpe"),
       lty = 2,
       lwd = 3,
       bty = "o",
       col = cores)
axis(1, 1:nline, Eixo_X)
#axis(1, 1:nline, las=1)
#axis(1, paste(rownames(Comparativo)))
#text(nline/2, min(Comparativo), labels=(paste("
#                   Período de teste:", xlab= Inicio_dataPredict, "a",
#                                         xlab = Fim_dataPredict)), cex= 0.8)
dev.off()


RetornosMediosBuffet = as.xts(RetornosMediosBuffet)
colnames(PosCovidSP500)="SP500"
rownames(PosCovidSP500)= Eixo_X
PosCovidSP500 = as.xts(PosCovidSP500)
Media_C_Net_T_Comparativa = as.xts(Media_C_Net_T_Comparativa)

pdf("Cartas_Comparativas.pdf")
Comparativo = as.xts(Comparativo)
charts.PerformanceSummary(Comparativo$SP500)
charts.PerformanceSummary(Comparativo$Buffet)
charts.PerformanceSummary(Comparativo$RNAt_Eq)
charts.PerformanceSummary(Comparativo$Markovitz)
charts.PerformanceSummary(Comparativo$RNAt_Mkv)
Legenda=colnames(Comparativo)
plot(Comparativo)


dev.off()

### New Grafic

pdf("New_grafic_Comparativo.pdf")
png(file="New_grafic_Comparativo_Buffet_Sharpe.png", width=1920, height=1920, res=296, family = "A")
par(#mfrow=c(2,2),
  #mar=c(2,2,2,2),
  oma=c(1,2,1,1))

library("ggplot2")
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A", cex=0.8)

Eixo = c(1:nrow(Comparativo))
Eixo_X = rownames(as.data.frame(Comparativo))
Comparativo2 = as.data.frame(Comparativo)
#Eixo_X2 = c(1,
#            round(nrow(Comparativo)/4,0),
#            round(nrow(Comparativo)/2,0),
#            round(nrow(Comparativo)*3/4,0),
#            nrow(Comparativo))
if(nrow(Comparativo)>200) {Eixo_X2 = c(1, 100, 200, 300, 400, 500, 600)
} else {Eixo_X2 = c(1, 50, 100, 149, 200, 250, 300)}
Eixo_X3 = rownames(Comparativo2[Eixo_X2,])
Inicio_data = rownames(Comparativo2[1,])
Fim_data = rownames(Comparativo2[nrow(Comparativo2),])
TestComparativo = cbind(as.data.frame(Comparativo), Eixo)
Retornos=TestComparativo$SP500
Periodos=TestComparativo$Eixo
z = TestComparativo$Buffet
w = TestComparativo$ANNt_Eq
t = TestComparativo$ANNt_Mkv
v = TestComparativo$ANNt_Sharpe
s = TestComparativo$Markowitz
u = TestComparativo$MaxSharpe
cores <- c("black", "red", "blue", "green", "darkgreen", "brown", "gray")
plot(Periodos, Retornos,
     type ="l",
     xaxt = "n",
     ylab = "Cumulative Returns",
     xlab = "Period",
     las =1,
     #xaxp = c(1,nline, 5),
     ylim = c(min(Comparativo), max(Comparativo)))
lines(z, col = c("red"))
lines(w, col = c("blue"))
lines(t, col = c("green"))
lines(v, col = c("darkgreen"))
lines(s, col = c("brown"))
lines(u, col = c("gray"))
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
#title(main = "Comparativo", font.main = 1, line = 1.5)
#title(main = paste("Comparativo           ",
#                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
title("Comparative")
title(main = paste(
  xlab= Inicio_data,"/", xlab= Fim_data),
  line = 0.5,
  cex = 0.5,
  font.main = 1)
legend("topleft", cex= 1.0, legend = c(    "SP500",
                                           "Buffet",
                                           "ANNt_Eq",
                                           "ANNt_Mkw",
                                           "ANNt_Sharpe",
                                           "Markowitz",
                                           "Sharpe"),
       lty = 2,
       lwd = 3,
       #bty = "o",
       bty = "n",
       col = cores)
#box.col = "white")

dev.off()




}









