#'OCA 2023
#'Generate the results of Oliveira, Ceretta & Albrecht (2023)
#'@description
#'For more details see: Oliveira, A. S., Ceretta, P. S., & Albrecht, P. (2023). Performance comparison of multifractal techniques and artificial neural networks in the construction of investment portfolios. Finance Research Letters, 55, 103814.
#'@param () No require parameters
#'@examples
#'Data_OCA2023()
#'
#'@author Alexandre Silva de Oliveira
#'@references Oliveira, A. S., Ceretta, P. S., & Albrecht, P. (2023). Performance comparison of multifractal techniques and artificial neural networks in the construction of investment portfolios. Finance Research Letters, 55, 103814.
#'@export
Data_OCA2023<-function(){
  library(readxl)
  library(readr)

  download.file("https://github.com/aleoli05/ANNt/raw/main/Data_/Comparativo_OCA2023_5_Assets.rda",destfile ="~/Comparativo_OCA2023_5_Assets.rda")
  #download.file("https://github.com/aleoli05/ANNt/blob/e13557ee41979bdf316d00cacd207730ce843902/Data_/Comparativo_OCA2023_5_Assets.xlsx?raw=true",destfile ="~/Comparativo_OCA2023_5_Assets.xlsx")
  #download.file("https://github.com/aleoli05/ANNt/raw/main/Data_/Comparativo_OCA2023_5_Assets.xlsx?raw=true",destfile ="~/Comp.xlsx")
  download.file("https://github.com/aleoli05/ANNt/raw/main/Data_/SUMBACKTEST_OCA2023_5_Assets.rda",destfile ="~/SUMBACKTEST_OCA2023_5_Assets.rda")
  #download.file("https://github.com/aleoli05/ANNt/blob/a6af38a2857db2fb5a70edb247ae0abf45ab72e2/Data_/SUMBACKTEST_OCA2023_5_Assets.xlsx?raw=true",destfile ="~/SUMBACKTEST_OCA2023_5_Assets.xlsx")
  #download.file("https://github.com/aleoli05/ANNt/raw/main/Data_/SUMBACKTEST_OCA2023_5_Assets.xlsx?raw=true",destfile ="~/SUMBACKTEST_OCA2023_5_Assets.xlsx")
  download.file("https://github.com/aleoli05/ANNt/raw/main/Data_/Weights2.rda",destfile ="~/Weights.rda")

  #download.file("https://github.com/GabauerDavid/ConnectednessApproach/raw/main/data/bcg2022.rda",destfile ="~/bcg2022.rda")
  #load('~/bcg2022.rda')
  #download.file("https://github.com/aleoli05/ANNt/raw/main/Data_/Comparativo_OCA2023_5_Assets(2).rda?raw=true",destfile ="~/C.rda")
  #load('~/C.rda')


  #load('~/SUMBACKTEST_OCA2023_5_Assets.rda')
  #URL1="https://github.com/aleoli05/ANNt/tree/main/Comparativo_OCA2023_5_Assets.xlsx"
  #URL2="https://github.com/aleoli05/ANNt/tree/main/SUMBACKTEST_OCA2023_5_Assets.xlsx"
  #Comparativo_OCA2023_5_Assets=read_excel("~/Comparativo_OCA2023_5_Assets.xlsx")
  #Comparativo_OCA2023_5_Assets=read_xls("~/Comparativo_OCA2023_5_Assets.xlsx")
  #SUMBACKTEST_OCA2023_5_Assets=read_xlsx("~/SUMBACKTEST_OCA2023_5_Assets.xlsx")

  load('~/Comparativo_OCA2023_5_Assets.rda')
  Comparativo=as.data.frame(Comparativo_OCA2023_5_Assets)
  rownames(Comparativo)=Comparativo[,1]
  Comparativo=Comparativo[,-1]
  View(Comparativo_OCA2023_5_Assets)

  load('~/SUMBACKTEST_OCA2023_5_Assets.rda')
  View(SUMBACKTEST_OCA2023_5_Assets)

  load('~/Weights.rda')
  print(Weights)

  N_Assets=5
  Final_Date_Testing='2022-07-12'
  Until_Date =''

  ###Gráfico Comparativo dos Retornos Acumulados das Carteiras

  options(warn=-1)
  Eixo_X = rownames(Comparativo[,1])
  nline = nrow(Comparativo)
  Comparativo = as.data.frame(Comparativo)
  nline = nrow(Comparativo)



  #########################
  #Until_Date=rownames(Comparativo[nrow(Comparativo),])
  #Comparativo=Comparativo
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo[nrow(Comparativo),])
  }

  Corte= which(rownames(as.data.frame(Comparativo))==as.Date(Until_Date))
  Coparativo_Backup = Comparativo
  Comparativo=Comparativo[1:Corte,]

  ydev=dev.list()
  if(class(ydev)!="NULL"){
    dev.off()
  }

  png(file="~/Graphic_Cumulative_Returns.png", width=1920, height=1920, res=296, family = "A")
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
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo)>100) {Eixo_X2 = c(1, 50, 100, 149, 200, 250, 300)
    }else{Eixo_C2 = c(1,50,100)}}
  Eixo_X3 = rownames(Comparativo2[Eixo_X2,])
  Inicio_data = rownames(Comparativo2[1,])
  Fim_data = rownames(Comparativo2[nrow(Comparativo2),])
  #Fim_data = "2023-03-16"
  TestComparativo = cbind(as.data.frame(Comparativo), Eixo)
  Retornos=TestComparativo$SP500
  Periodos=TestComparativo$Eixo
  s = TestComparativo$MARKOWITZ
  u = TestComparativo$SHARPE
  z = TestComparativo$MF_MKW
  p = TestComparativo$MF_SHARPE
  w = TestComparativo$ANNt_EQ
  t = TestComparativo$ANNt_MKW
  q = TestComparativo$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Cumulative Returns",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo), max(Comparativo)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(z, col = c("red"))
  lines(p, col = c("purple"))
  lines(w, col = c("blue"))
  lines(t, col = c("green"))
  lines(q, col = c("darkgreen"))
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
  #title(main = "Carteiras RNAt e MF-DFA com 5 Ativos", font.main = 1, line = 1.5)
  #title(main = paste("Comparativo           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  title(paste("MF-DFA and ANNt Portfolios:", N_Assets, "Assets"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend("topleft",
         #"bottomright",
         legend = c("SP500", "MARKOWITZ", "SHARPE", "MF_MKW", "MF_SHARPE",
                    "ANNt_Eq",
                    "ANNt_MKW", "ANNt_SHARPE"),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  ydev=dev.list()
  if(class(ydev)!="NULL"){
    dev.off()
  }
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

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
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo)>100) {Eixo_X2 = c(1, 50, 100, 149, 200, 250, 300)
    }else{Eixo_C2 = c(1,50,100)}}
  Eixo_X3 = rownames(Comparativo2[Eixo_X2,])
  Inicio_data = rownames(Comparativo2[1,])
  Fim_data = rownames(Comparativo2[nrow(Comparativo2),])
  #Fim_data = "2023-03-16"
  TestComparativo = cbind(as.data.frame(Comparativo), Eixo)
  Retornos=TestComparativo$SP500
  Periodos=TestComparativo$Eixo
  s = TestComparativo$MARKOWITZ
  u = TestComparativo$SHARPE
  z = TestComparativo$MF_MKW
  p = TestComparativo$MF_SHARPE
  w = TestComparativo$ANNt_EQ
  t = TestComparativo$ANNt_MKW
  q = TestComparativo$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Cumulative Returns",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo), max(Comparativo)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(z, col = c("red"))
  lines(p, col = c("purple"))
  lines(w, col = c("blue"))
  lines(t, col = c("green"))
  lines(q, col = c("darkgreen"))
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
  #title(main = "Carteiras RNAt e MF-DFA com 5 Ativos", font.main = 1, line = 1.5)
  #title(main = paste("Comparativo           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  title(paste("MF-DFA and ANNt Portfolios:", N_Assets, "Assets"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend("topleft",
         #"bottomright",
         legend = c("SP500", "MARKOWITZ", "SHARPE", "MF_MKW", "MF_SHARPE",
                    "ANNt_Eq",
                    "ANNt_MKW", "ANNt_SHARPE"),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")

}
