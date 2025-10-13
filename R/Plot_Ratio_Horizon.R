#' Plot_Ratio_Horizon
#'@description
#' Analyzes ratios chart of portfolios with different investment horizons.
#' You need to run the Investment_Horizon command first.

#'@param Ratio:
#' Rm - Average_Return - Means of Returns from each portfolio;
#' Annualized_Returns - This is the standard ratio;
#' RCum - Cumulative_Returns;
#' Annualized_Volatility - Annualized Volatility;
#' Var - Variance with 95% confidence;
#' CVar - Conditional Variance 95% confidence;
#' Sharpe - Sharpe Ratio;
#' Alpha - Jensen's Alpha;
#' Beta - Covariance between portfolio and market proxy over market proxy Variance;
#' Sortino - Sortino Ratio;
#' Treynor - Treynor Ratio.
#'
#'@param Legend_position:
#'topleft;
#'topright;
#'bottomleft;
#'bottomrigh;
#'bottom;
#'top;
#'left;
#'right.
#'@param Compare with:
#' RM - Return of Market proxy;
#' MARKOWITZ;
#' SHARPE;
#' MF_EQ;
#' MF_MKW;
#' MF_SHARPE;
#' ANNt-EQ;
#' ANNt_MKW;
#' ANNt_SHARPE;
#' Magic_EQ;
#' Magic_MKW;
#' Magic_SHARPE;
#' Graham_EQ;
#' Graham_MKW;
#' Graham_SHARPE.
#' @param Until_Date Period´s length to plot.
#'@examples
#' Plot_Ratio_Horizon(Ratio="Annualized_Returns")

#'@export
Plot_Ratio_Horizon <-function(Ratio="Annualized_Returns",Legend_position="topleft",
                              Compare="RM", Until_Date=''){

################################################################################
# Graphic Annualized Returns
  library(stringr)
  if(Compare=='RM'){
    load('~/RM.rda')
    Compare=RM
  }
Plot_Annualized_Returns_Horizon <-function(){
  load('~/Comparativo_RETORNOS_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  options(warn=-1)
  Eixo_X = rownames(Comparativo_RETORNOS_Horizon_Anual[,1])
  nline = nrow(Comparativo_RETORNOS_Horizon_Anual)


  #########################
  #Until_Date=rownames(Comparativo_RETORNOS_Horizon_Anual)[nrow(Comparativo_RETORNOS_Horizon_Anual)]
  #Comparativo_RETORNOS_Horizon_Anual=Comparativo_RETORNOS_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_RETORNOS_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_RETORNOS_Horizon_Anual[nrow(Comparativo_RETORNOS_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_RETORNOS_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_RETORNOS_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Comparativo_RETORNOS_Horizon_Anual = Comparativo_RETORNOS_Horizon_Anual[rev(seq_len(nrow(Comparativo_RETORNOS_Horizon_Anual))),]
  Comparativo_RETORNOS_Horizon_Anual = as.data.frame(Comparativo_RETORNOS_Horizon_Anual)

  Corte= which(rownames(as.data.frame(Comparativo_RETORNOS_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_RETORNOS_Horizon_Anual
  Comparativo_RETORNOS_Horizon_Anual=Comparativo_RETORNOS_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]

  View(Comparativo_RETORNOS_Horizon_Anual)
  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_RETORNOS_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Max_Return')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_RETORNOS_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.max(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_Returns_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################

  png(file="~/Graphic_Annualized_Returns_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_RETORNOS_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_RETORNOS_Horizon_Anual))
  Comparativo_RETORNOS_Horizon_Anual2 = as.data.frame(Comparativo_RETORNOS_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_RETORNOS_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_RETORNOS_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_RETORNOS_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_RETORNOS_Horizon_Anual))
  if(nrow(Comparativo_RETORNOS_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_RETORNOS_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_RETORNOS_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_RETORNOS_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_RETORNOS_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_RETORNOS_Horizon_Anual2[nrow(Comparativo_RETORNOS_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_RETORNOS_Horizon_Anual = cbind(as.data.frame(Comparativo_RETORNOS_Horizon_Anual), Eixo)
  Retornos=TestComparativo_RETORNOS_Horizon_Anual[,1]
  Periodos=TestComparativo_RETORNOS_Horizon_Anual$Eixo
  s = TestComparativo_RETORNOS_Horizon_Anual$MARKOWITZ
  u = TestComparativo_RETORNOS_Horizon_Anual$SHARPE
  h = TestComparativo_RETORNOS_Horizon_Anual$MF_EQ
  z = TestComparativo_RETORNOS_Horizon_Anual$MF_MKW
  p = TestComparativo_RETORNOS_Horizon_Anual$MF_SHARPE
  w = TestComparativo_RETORNOS_Horizon_Anual$ANNt_EQ
  t = TestComparativo_RETORNOS_Horizon_Anual$ANNt_MKW
  q = TestComparativo_RETORNOS_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Annualized Returns",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_RETORNOS_Horizon_Anual), max(Comparativo_RETORNOS_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_RETORNOS_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Annualized Returns over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Annualized Returns over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_RETORNOS_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_RETORNOS_Horizon_Anual))
  Comparativo_RETORNOS_Horizon_Anual2 = as.data.frame(Comparativo_RETORNOS_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_RETORNOS_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_RETORNOS_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_RETORNOS_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_RETORNOS_Horizon_Anual))
  if(nrow(Comparativo_RETORNOS_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_RETORNOS_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_RETORNOS_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_RETORNOS_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_RETORNOS_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_RETORNOS_Horizon_Anual2[nrow(Comparativo_RETORNOS_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_RETORNOS_Horizon_Anual = cbind(as.data.frame(Comparativo_RETORNOS_Horizon_Anual), Eixo)
  Retornos=TestComparativo_RETORNOS_Horizon_Anual[,1]
  Periodos=TestComparativo_RETORNOS_Horizon_Anual$Eixo
  s = TestComparativo_RETORNOS_Horizon_Anual$MARKOWITZ
  u = TestComparativo_RETORNOS_Horizon_Anual$SHARPE
  h = TestComparativo_RETORNOS_Horizon_Anual$MF_EQ
  z = TestComparativo_RETORNOS_Horizon_Anual$MF_MKW
  p = TestComparativo_RETORNOS_Horizon_Anual$MF_SHARPE
  w = TestComparativo_RETORNOS_Horizon_Anual$ANNt_EQ
  t = TestComparativo_RETORNOS_Horizon_Anual$ANNt_MKW
  q = TestComparativo_RETORNOS_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Annualized Returns",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_RETORNOS_Horizon_Anual), max(Comparativo_RETORNOS_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_RETORNOS_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Annualized Returns over the", RM,"Investment Horizon", N_Assets, "Assets"))
  title(paste("Annualized Returns over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
# Graphic Volatility Annualized

Plot_Annualized_Volatility_Horizon <-function(){
  load('~/Comparativo_Volatility_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  options(warn=-1)
  Eixo_X = rownames(Comparativo_Volatility_Horizon_Anual[,1])
  nline = nrow(Comparativo_Volatility_Horizon_Anual)
  Comparativo_Volatility_Horizon_Anual = Comparativo_Volatility_Horizon_Anual[rev(seq_len(nrow(Comparativo_Volatility_Horizon_Anual))),]
  Comparativo_Volatility_Horizon_Anual = as.data.frame(Comparativo_Volatility_Horizon_Anual)
  nline = nrow(Comparativo_Volatility_Horizon_Anual)



  #########################
  Until_Date=rownames(Comparativo_Volatility_Horizon_Anual)[nrow(Comparativo_Volatility_Horizon_Anual)]
  #Comparativo_Volatility_Horizon_Anual=Comparativo_Volatility_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_Volatility_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_Volatility_Horizon_Anual[nrow(Comparativo_Volatility_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_Volatility_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_Volatility_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Corte= which(rownames(as.data.frame(Comparativo_Volatility_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_Volatility_Horizon_Anual
  Comparativo_Volatility_Horizon_Anual=Comparativo_Volatility_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]

  View(Comparativo_Volatility_Horizon_Anual)
  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_Volatility_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Min_Volatility')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_Volatility_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.min(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_Volatility_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################

  png(file="~/Graphic_Annualized_Volatility_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Volatility_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Volatility_Horizon_Anual))
  Comparativo_Volatility_Horizon_Anual2 = as.data.frame(Comparativo_Volatility_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Volatility_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Volatility_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Volatility_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Volatility_Horizon_Anual))
  if(nrow(Comparativo_Volatility_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Volatility_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Volatility_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Volatility_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Volatility_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Volatility_Horizon_Anual2[nrow(Comparativo_Volatility_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Volatility_Horizon_Anual = cbind(as.data.frame(Comparativo_Volatility_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Volatility_Horizon_Anual[,1]
  Periodos=TestComparativo_Volatility_Horizon_Anual$Eixo
  s = TestComparativo_Volatility_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Volatility_Horizon_Anual$SHARPE
  h = TestComparativo_Volatility_Horizon_Anual$MF_EQ
  z = TestComparativo_Volatility_Horizon_Anual$MF_MKW
  p = TestComparativo_Volatility_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Volatility_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Volatility_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Volatility_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Annualized Volatility",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Volatility_Horizon_Anual), max(Comparativo_Volatility_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_Volatility_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Annualized Volatility of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Annualized Volatility of Portfolios over the", RM,"Investment Horizon"))

   #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Volatility_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Volatility_Horizon_Anual))
  Comparativo_Volatility_Horizon_Anual2 = as.data.frame(Comparativo_Volatility_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Volatility_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Volatility_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Volatility_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Volatility_Horizon_Anual))
  if(nrow(Comparativo_Volatility_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Volatility_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Volatility_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Volatility_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Volatility_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Volatility_Horizon_Anual2[nrow(Comparativo_Volatility_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Volatility_Horizon_Anual = cbind(as.data.frame(Comparativo_Volatility_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Volatility_Horizon_Anual[,1]
  Periodos=TestComparativo_Volatility_Horizon_Anual$Eixo
  s = TestComparativo_Volatility_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Volatility_Horizon_Anual$SHARPE
  h = TestComparativo_Volatility_Horizon_Anual$MF_EQ
  z = TestComparativo_Volatility_Horizon_Anual$MF_MKW
  p = TestComparativo_Volatility_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Volatility_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Volatility_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Volatility_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Annualized Volatility",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Volatility_Horizon_Anual), max(Comparativo_Volatility_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_Volatility_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Annualized Volatility of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Annualized Volatility of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
# Graphic Sharpe

Plot_Annualized_Sharpe_Horizon <-function(){
  load('~/Comparativo_Sharpe_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  options(warn=-1)
  Eixo_X = rownames(Comparativo_Sharpe_Horizon_Anual[,1])
  nline = nrow(Comparativo_Sharpe_Horizon_Anual)
  Comparativo_Sharpe_Horizon_Anual = Comparativo_Sharpe_Horizon_Anual[rev(seq_len(nrow(Comparativo_Sharpe_Horizon_Anual))),]
  Comparativo_Sharpe_Horizon_Anual = as.data.frame(Comparativo_Sharpe_Horizon_Anual)
  nline = nrow(Comparativo_Sharpe_Horizon_Anual)



  #########################
  Until_Date=rownames(Comparativo_Sharpe_Horizon_Anual)[nrow(Comparativo_Sharpe_Horizon_Anual)]
  #Comparativo_Sharpe_Horizon_Anual=Comparativo_Sharpe_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_Sharpe_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_Sharpe_Horizon_Anual[nrow(Comparativo_Sharpe_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_Sharpe_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_Sharpe_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Corte= which(rownames(as.data.frame(Comparativo_Sharpe_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_Sharpe_Horizon_Anual
  Comparativo_Sharpe_Horizon_Anual=Comparativo_Sharpe_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]

  View(Comparativo_Sharpe_Horizon_Anual)
  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_Sharpe_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Max_Sharpe')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_Sharpe_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.max(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_Sharpe_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################

  png(file="~/Graphic_Sharpe_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Sharpe_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Sharpe_Horizon_Anual))
  Comparativo_Sharpe_Horizon_Anual2 = as.data.frame(Comparativo_Sharpe_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Sharpe_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Sharpe_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Sharpe_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Sharpe_Horizon_Anual))
  if(nrow(Comparativo_Sharpe_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Sharpe_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Sharpe_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Sharpe_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Sharpe_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Sharpe_Horizon_Anual2[nrow(Comparativo_Sharpe_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Sharpe_Horizon_Anual = cbind(as.data.frame(Comparativo_Sharpe_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Sharpe_Horizon_Anual[,1]
  Periodos=TestComparativo_Sharpe_Horizon_Anual$Eixo
  s = TestComparativo_Sharpe_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Sharpe_Horizon_Anual$SHARPE
  h = TestComparativo_Sharpe_Horizon_Anual$MF_EQ
  z = TestComparativo_Sharpe_Horizon_Anual$MF_MKW
  p = TestComparativo_Sharpe_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Sharpe_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Sharpe_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Sharpe_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Sharpe Ratio",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Sharpe_Horizon_Anual), max(Comparativo_Sharpe_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_Sharpe_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Sharpe Ratio of Portfolio over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Sharpe Ratio of Portfolio over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Sharpe_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Sharpe_Horizon_Anual))
  Comparativo_Sharpe_Horizon_Anual2 = as.data.frame(Comparativo_Sharpe_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Sharpe_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Sharpe_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Sharpe_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Sharpe_Horizon_Anual))
  if(nrow(Comparativo_Sharpe_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Sharpe_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Sharpe_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Sharpe_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Sharpe_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Sharpe_Horizon_Anual2[nrow(Comparativo_Sharpe_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Sharpe_Horizon_Anual = cbind(as.data.frame(Comparativo_Sharpe_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Sharpe_Horizon_Anual[,1]
  Periodos=TestComparativo_Sharpe_Horizon_Anual$Eixo
  s = TestComparativo_Sharpe_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Sharpe_Horizon_Anual$SHARPE
  h = TestComparativo_Sharpe_Horizon_Anual$MF_EQ
  z = TestComparativo_Sharpe_Horizon_Anual$MF_MKW
  p = TestComparativo_Sharpe_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Sharpe_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Sharpe_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Sharpe_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Sharpe Ratio",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Sharpe_Horizon_Anual), max(Comparativo_Sharpe_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_Sharpe_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Sharpe Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Sharpe Ratio of Portfolios over the", RM,"Investment Horizon"))
    #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
# Graphic Jensen's Alpha

Plot_Annualized_Alpha_Horizon <-function(){
  load('~/Comparativo_Alpha_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  options(warn=-1)
  Eixo_X = rownames(Comparativo_Alpha_Horizon_Anual[,1])
  nline = nrow(Comparativo_Alpha_Horizon_Anual)
  Comparativo_Alpha_Horizon_Anual = Comparativo_Alpha_Horizon_Anual[rev(seq_len(nrow(Comparativo_Alpha_Horizon_Anual))),]
  Comparativo_Alpha_Horizon_Anual = as.data.frame(Comparativo_Alpha_Horizon_Anual)
  nline = nrow(Comparativo_Alpha_Horizon_Anual)



  #########################
  Until_Date=rownames(Comparativo_Alpha_Horizon_Anual)[nrow(Comparativo_Alpha_Horizon_Anual)]
  #Comparativo_Alpha_Horizon_Anual=Comparativo_Alpha_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_Alpha_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_Alpha_Horizon_Anual[nrow(Comparativo_Alpha_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_Alpha_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_Alpha_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Corte= which(rownames(as.data.frame(Comparativo_Alpha_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_Alpha_Horizon_Anual
  Comparativo_Alpha_Horizon_Anual=Comparativo_Alpha_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]


  View(Comparativo_Alpha_Horizon_Anual)
  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_Alpha_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Max_Alpha')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_Alpha_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.max(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_Alpha_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################

  png(file="~/Graphic_Annualized_Alpha_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Alpha_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Alpha_Horizon_Anual))
  Comparativo_Alpha_Horizon_Anual2 = as.data.frame(Comparativo_Alpha_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Alpha_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Alpha_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Alpha_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Alpha_Horizon_Anual))
  if(nrow(Comparativo_Alpha_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Alpha_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Alpha_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Alpha_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Alpha_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Alpha_Horizon_Anual2[nrow(Comparativo_Alpha_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Alpha_Horizon_Anual = cbind(as.data.frame(Comparativo_Alpha_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Alpha_Horizon_Anual[,1]
  Periodos=TestComparativo_Alpha_Horizon_Anual$Eixo
  s = TestComparativo_Alpha_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Alpha_Horizon_Anual$SHARPE
  h = TestComparativo_Alpha_Horizon_Anual$MF_EQ
  z = TestComparativo_Alpha_Horizon_Anual$MF_MKW
  p = TestComparativo_Alpha_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Alpha_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Alpha_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Alpha_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Alpha Ratio",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Alpha_Horizon_Anual), max(Comparativo_Alpha_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_Alpha_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Jensen's Alpha Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Jensen's Alpha Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Alpha_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Alpha_Horizon_Anual))
  Comparativo_Alpha_Horizon_Anual2 = as.data.frame(Comparativo_Alpha_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Alpha_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Alpha_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Alpha_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Alpha_Horizon_Anual))
  if(nrow(Comparativo_Alpha_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Alpha_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Alpha_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Alpha_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Alpha_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Alpha_Horizon_Anual2[nrow(Comparativo_Alpha_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Alpha_Horizon_Anual = cbind(as.data.frame(Comparativo_Alpha_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Alpha_Horizon_Anual[,1]
  Periodos=TestComparativo_Alpha_Horizon_Anual$Eixo
  s = TestComparativo_Alpha_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Alpha_Horizon_Anual$SHARPE
  h = TestComparativo_Alpha_Horizon_Anual$MF_EQ
  z = TestComparativo_Alpha_Horizon_Anual$MF_MKW
  p = TestComparativo_Alpha_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Alpha_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Alpha_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Alpha_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Alpha Ratio",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Alpha_Horizon_Anual), max(Comparativo_Alpha_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_Alpha_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Jensen's Alpha Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Jensen's Alpha Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
# Graphic Beta

Plot_Annualized_Beta_Horizon <-function(){
  load('~/Comparativo_Beta_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  options(warn=-1)
  Eixo_X = rownames(Comparativo_Beta_Horizon_Anual[,1])
  nline = nrow(Comparativo_Beta_Horizon_Anual)
  Comparativo_Beta_Horizon_Anual = Comparativo_Beta_Horizon_Anual[rev(seq_len(nrow(Comparativo_Beta_Horizon_Anual))),]
  Comparativo_Beta_Horizon_Anual = as.data.frame(Comparativo_Beta_Horizon_Anual)
  nline = nrow(Comparativo_Beta_Horizon_Anual)



  #########################
  Until_Date=rownames(Comparativo_Beta_Horizon_Anual)[nrow(Comparativo_Beta_Horizon_Anual)]
  #Comparativo_Beta_Horizon_Anual=Comparativo_Beta_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_Beta_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_Beta_Horizon_Anual[nrow(Comparativo_Beta_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_Beta_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_Beta_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Corte= which(rownames(as.data.frame(Comparativo_Beta_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_Beta_Horizon_Anual
  Comparativo_Beta_Horizon_Anual=Comparativo_Beta_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]


  View(Comparativo_Beta_Horizon_Anual)
  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_Beta_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Min_Beta')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_Beta_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.min(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_Beta_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################

  png(file="~/Graphic_Annualized_Beta_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Beta_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Beta_Horizon_Anual))
  Comparativo_Beta_Horizon_Anual2 = as.data.frame(Comparativo_Beta_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Beta_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Beta_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Beta_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Beta_Horizon_Anual))
  if(nrow(Comparativo_Beta_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Beta_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Beta_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Beta_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Beta_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Beta_Horizon_Anual2[nrow(Comparativo_Beta_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Beta_Horizon_Anual = cbind(as.data.frame(Comparativo_Beta_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Beta_Horizon_Anual[,1]
  Periodos=TestComparativo_Beta_Horizon_Anual$Eixo
  s = TestComparativo_Beta_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Beta_Horizon_Anual$SHARPE
  h = TestComparativo_Beta_Horizon_Anual$MF_EQ
  z = TestComparativo_Beta_Horizon_Anual$MF_MKW
  p = TestComparativo_Beta_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Beta_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Beta_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Beta_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Beta Ratio",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Beta_Horizon_Anual), max(Comparativo_Beta_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_Beta_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Beta Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Beta Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Beta_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Beta_Horizon_Anual))
  Comparativo_Beta_Horizon_Anual2 = as.data.frame(Comparativo_Beta_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Beta_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Beta_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Beta_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Beta_Horizon_Anual))
  if(nrow(Comparativo_Beta_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Beta_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Beta_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Beta_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Beta_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Beta_Horizon_Anual2[nrow(Comparativo_Beta_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Beta_Horizon_Anual = cbind(as.data.frame(Comparativo_Beta_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Beta_Horizon_Anual[,1]
  Periodos=TestComparativo_Beta_Horizon_Anual$Eixo
  s = TestComparativo_Beta_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Beta_Horizon_Anual$SHARPE
  h = TestComparativo_Beta_Horizon_Anual$MF_EQ
  z = TestComparativo_Beta_Horizon_Anual$MF_MKW
  p = TestComparativo_Beta_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Beta_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Beta_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Beta_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Beta Ratio",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Beta_Horizon_Anual), max(Comparativo_Beta_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_Beta_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Beta Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Beta Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
# Graphic Sortino

Plot_Annualized_Sortino_Horizon <-function(){
  load('~/Comparativo_Sortino_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  options(warn=-1)
  Eixo_X = rownames(Comparativo_Sortino_Horizon_Anual[,1])
  nline = nrow(Comparativo_Sortino_Horizon_Anual)
  Comparativo_Sortino_Horizon_Anual = Comparativo_Sortino_Horizon_Anual[rev(seq_len(nrow(Comparativo_Sortino_Horizon_Anual))),]
  Comparativo_Sortino_Horizon_Anual = as.data.frame(Comparativo_Sortino_Horizon_Anual)
  nline = nrow(Comparativo_Sortino_Horizon_Anual)



  #########################
  Until_Date=rownames(Comparativo_Sortino_Horizon_Anual)[nrow(Comparativo_Sortino_Horizon_Anual)]
  #Comparativo_Sortino_Horizon_Anual=Comparativo_Sortino_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_Sortino_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_Sortino_Horizon_Anual[nrow(Comparativo_Sortino_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_Sortino_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_Sortino_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Corte= which(rownames(as.data.frame(Comparativo_Sortino_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_Sortino_Horizon_Anual
  Comparativo_Sortino_Horizon_Anual=Comparativo_Sortino_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]

  View(Comparativo_Sortino_Horizon_Anual)

  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_Sortino_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Max_Sortino')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_Sortino_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.max(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_Sortino_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################


  png(file="~/Graphic_Annualized_Sortino_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Sortino_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Sortino_Horizon_Anual))
  Comparativo_Sortino_Horizon_Anual2 = as.data.frame(Comparativo_Sortino_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Sortino_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Sortino_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Sortino_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Sortino_Horizon_Anual))
  if(nrow(Comparativo_Sortino_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Sortino_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Sortino_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Sortino_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Sortino_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Sortino_Horizon_Anual2[nrow(Comparativo_Sortino_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Sortino_Horizon_Anual = cbind(as.data.frame(Comparativo_Sortino_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Sortino_Horizon_Anual[,1]
  Periodos=TestComparativo_Sortino_Horizon_Anual$Eixo
  s = TestComparativo_Sortino_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Sortino_Horizon_Anual$SHARPE
  h = TestComparativo_Sortino_Horizon_Anual$MF_EQ
  z = TestComparativo_Sortino_Horizon_Anual$MF_MKW
  p = TestComparativo_Sortino_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Sortino_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Sortino_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Sortino_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Sortino Ratio",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Sortino_Horizon_Anual), max(Comparativo_Sortino_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_Sortino_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Sortino Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Sortino Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Sortino_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Sortino_Horizon_Anual))
  Comparativo_Sortino_Horizon_Anual2 = as.data.frame(Comparativo_Sortino_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Sortino_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Sortino_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Sortino_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Sortino_Horizon_Anual))
  if(nrow(Comparativo_Sortino_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Sortino_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Sortino_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Sortino_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Sortino_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Sortino_Horizon_Anual2[nrow(Comparativo_Sortino_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Sortino_Horizon_Anual = cbind(as.data.frame(Comparativo_Sortino_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Sortino_Horizon_Anual[,1]
  Periodos=TestComparativo_Sortino_Horizon_Anual$Eixo
  s = TestComparativo_Sortino_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Sortino_Horizon_Anual$SHARPE
  h = TestComparativo_Sortino_Horizon_Anual$MF_EQ
  z = TestComparativo_Sortino_Horizon_Anual$MF_MKW
  p = TestComparativo_Sortino_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Sortino_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Sortino_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Sortino_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Sortino Ratio",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Sortino_Horizon_Anual), max(Comparativo_Sortino_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_Sortino_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Sortino Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Sortino Ratio of Portfolios over the", RM,"Investment Horizon"))
    #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
# Graphic Treynor

Plot_Annualized_Treynor_Horizon <-function(){
  load('~/Comparativo_Treynor_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  options(warn=-1)
  Eixo_X = rownames(Comparativo_Treynor_Horizon_Anual[,1])
  nline = nrow(Comparativo_Treynor_Horizon_Anual)
  Comparativo_Treynor_Horizon_Anual = Comparativo_Treynor_Horizon_Anual[rev(seq_len(nrow(Comparativo_Treynor_Horizon_Anual))),]
  Comparativo_Treynor_Horizon_Anual = as.data.frame(Comparativo_Treynor_Horizon_Anual)
  nline = nrow(Comparativo_Treynor_Horizon_Anual)



  #########################
  Until_Date=rownames(Comparativo_Treynor_Horizon_Anual)[nrow(Comparativo_Treynor_Horizon_Anual)]
  #Comparativo_Treynor_Horizon_Anual=Comparativo_Treynor_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_Treynor_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_Treynor_Horizon_Anual[nrow(Comparativo_Treynor_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_Treynor_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_Treynor_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Corte= which(rownames(as.data.frame(Comparativo_Treynor_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_Treynor_Horizon_Anual
  Comparativo_Treynor_Horizon_Anual=Comparativo_Treynor_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]


  View(Comparativo_Treynor_Horizon_Anual)

  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_Treynor_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Max_Treynor')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_Treynor_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.max(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_Treynor_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################

  png(file="~/Graphic_Annualized_Treynor_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Treynor_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Treynor_Horizon_Anual))
  Comparativo_Treynor_Horizon_Anual2 = as.data.frame(Comparativo_Treynor_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Treynor_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Treynor_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Treynor_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Treynor_Horizon_Anual))
  if(nrow(Comparativo_Treynor_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Treynor_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Treynor_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Treynor_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Treynor_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Treynor_Horizon_Anual2[nrow(Comparativo_Treynor_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Treynor_Horizon_Anual = cbind(as.data.frame(Comparativo_Treynor_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Treynor_Horizon_Anual[,1]
  Periodos=TestComparativo_Treynor_Horizon_Anual$Eixo
  s = TestComparativo_Treynor_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Treynor_Horizon_Anual$SHARPE
  h = TestComparativo_Treynor_Horizon_Anual$MF_EQ
  z = TestComparativo_Treynor_Horizon_Anual$MF_MKW
  p = TestComparativo_Treynor_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Treynor_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Treynor_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Treynor_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Treynor Ratio",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Treynor_Horizon_Anual), max(Comparativo_Treynor_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_Treynor_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Treynor Ratio of Portfolios over the",RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Treynor Ratio of Portfolios over the",RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Treynor_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Treynor_Horizon_Anual))
  Comparativo_Treynor_Horizon_Anual2 = as.data.frame(Comparativo_Treynor_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Treynor_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Treynor_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Treynor_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Treynor_Horizon_Anual))
  if(nrow(Comparativo_Treynor_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Treynor_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Treynor_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Treynor_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Treynor_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Treynor_Horizon_Anual2[nrow(Comparativo_Treynor_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Treynor_Horizon_Anual = cbind(as.data.frame(Comparativo_Treynor_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Treynor_Horizon_Anual[,1]
  Periodos=TestComparativo_Treynor_Horizon_Anual$Eixo
  s = TestComparativo_Treynor_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Treynor_Horizon_Anual$SHARPE
  h = TestComparativo_Treynor_Horizon_Anual$MF_EQ
  z = TestComparativo_Treynor_Horizon_Anual$MF_MKW
  p = TestComparativo_Treynor_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Treynor_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Treynor_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Treynor_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Treynor Ratio",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Treynor_Horizon_Anual), max(Comparativo_Treynor_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_Treynor_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Treynor Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Treynor Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
# Graphic Var

Plot_Annualized_Var_Horizon <-function(){
  load('~/Comparativo_Var_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  options(warn=-1)
  Eixo_X = rownames(Comparativo_Var_Horizon_Anual[,1])
  nline = nrow(Comparativo_Var_Horizon_Anual)
  Comparativo_Var_Horizon_Anual = Comparativo_Var_Horizon_Anual[rev(seq_len(nrow(Comparativo_Var_Horizon_Anual))),]
  Comparativo_Var_Horizon_Anual = as.data.frame(Comparativo_Var_Horizon_Anual)
  nline = nrow(Comparativo_Var_Horizon_Anual)



  #########################
  Until_Date=rownames(Comparativo_Var_Horizon_Anual)[nrow(Comparativo_Var_Horizon_Anual)]
  #Comparativo_Var_Horizon_Anual=Comparativo_Var_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_Var_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_Var_Horizon_Anual[nrow(Comparativo_Var_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_Var_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_Var_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Corte= which(rownames(as.data.frame(Comparativo_Var_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_Var_Horizon_Anual
  Comparativo_Var_Horizon_Anual=Comparativo_Var_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]


  View(Comparativo_Var_Horizon_Anual)
  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_Var_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Min_Var')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_Var_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.min(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_Var_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################

  png(file="~/Graphic_Annualized_Var_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Var_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Var_Horizon_Anual))
  Comparativo_Var_Horizon_Anual2 = as.data.frame(Comparativo_Var_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Var_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Var_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Var_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Var_Horizon_Anual))
  if(nrow(Comparativo_Var_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Var_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Var_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Var_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Var_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Var_Horizon_Anual2[nrow(Comparativo_Var_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Var_Horizon_Anual = cbind(as.data.frame(Comparativo_Var_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Var_Horizon_Anual[,1]
  Periodos=TestComparativo_Var_Horizon_Anual$Eixo
  s = TestComparativo_Var_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Var_Horizon_Anual$SHARPE
  h = TestComparativo_Var_Horizon_Anual$MF_EQ
  z = TestComparativo_Var_Horizon_Anual$MF_MKW
  p = TestComparativo_Var_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Var_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Var_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Var_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Var Ratio",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Var_Horizon_Anual), max(Comparativo_Var_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_Var_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Var Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Var Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Var_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Var_Horizon_Anual))
  Comparativo_Var_Horizon_Anual2 = as.data.frame(Comparativo_Var_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Var_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Var_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Var_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Var_Horizon_Anual))
  if(nrow(Comparativo_Var_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Var_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Var_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Var_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Var_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Var_Horizon_Anual2[nrow(Comparativo_Var_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Var_Horizon_Anual = cbind(as.data.frame(Comparativo_Var_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Var_Horizon_Anual[,1]
  Periodos=TestComparativo_Var_Horizon_Anual$Eixo
  s = TestComparativo_Var_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Var_Horizon_Anual$SHARPE
  h = TestComparativo_Var_Horizon_Anual$MF_EQ
  z = TestComparativo_Var_Horizon_Anual$MF_MKW
  p = TestComparativo_Var_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Var_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Var_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Var_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Var Ratio",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Var_Horizon_Anual), max(Comparativo_Var_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_Var_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Var Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Var Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
# Graphic CVar

Plot_Annualized_CVar_Horizon <-function(){
  load('~/Comparativo_CVar_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  options(warn=-1)
  Eixo_X = rownames(Comparativo_CVar_Horizon_Anual[,1])
  nline = nrow(Comparativo_CVar_Horizon_Anual)
  Comparativo_CVar_Horizon_Anual = Comparativo_CVar_Horizon_Anual[rev(seq_len(nrow(Comparativo_CVar_Horizon_Anual))),]
  Comparativo_CVar_Horizon_Anual = as.data.frame(Comparativo_CVar_Horizon_Anual)
  nline = nrow(Comparativo_CVar_Horizon_Anual)



  #########################
  Until_Date=rownames(Comparativo_CVar_Horizon_Anual)[nrow(Comparativo_CVar_Horizon_Anual)]
  #Comparativo_CVar_Horizon_Anual=Comparativo_CVar_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_CVar_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_CVar_Horizon_Anual[nrow(Comparativo_CVar_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_CVar_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_CVar_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Corte= which(rownames(as.data.frame(Comparativo_CVar_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_CVar_Horizon_Anual
  Comparativo_CVar_Horizon_Anual=Comparativo_CVar_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]


  View(Comparativo_CVar_Horizon_Anual)
  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_CVar_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Min_CVar')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_CVar_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.min(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_CVar_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################

  png(file="~/Graphic_Annualized_CVar_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_CVar_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_CVar_Horizon_Anual))
  Comparativo_CVar_Horizon_Anual2 = as.data.frame(Comparativo_CVar_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_CVar_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_CVar_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_CVar_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_CVar_Horizon_Anual))
  if(nrow(Comparativo_CVar_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_CVar_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_CVar_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_CVar_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_CVar_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_CVar_Horizon_Anual2[nrow(Comparativo_CVar_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_CVar_Horizon_Anual = cbind(as.data.frame(Comparativo_CVar_Horizon_Anual), Eixo)
  Retornos=TestComparativo_CVar_Horizon_Anual[,1]
  Periodos=TestComparativo_CVar_Horizon_Anual$Eixo
  s = TestComparativo_CVar_Horizon_Anual$MARKOWITZ
  u = TestComparativo_CVar_Horizon_Anual$SHARPE
  h = TestComparativo_CVar_Horizon_Anual$MF_EQ
  z = TestComparativo_CVar_Horizon_Anual$MF_MKW
  p = TestComparativo_CVar_Horizon_Anual$MF_SHARPE
  w = TestComparativo_CVar_Horizon_Anual$ANNt_EQ
  t = TestComparativo_CVar_Horizon_Anual$ANNt_MKW
  q = TestComparativo_CVar_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "CVar Ratio",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_CVar_Horizon_Anual), max(Comparativo_CVar_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_CVar_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("CVar Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("CVar Ratio of Portfolios over the", RM,"Investment Horizon"))
    #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_CVar_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_CVar_Horizon_Anual))
  Comparativo_CVar_Horizon_Anual2 = as.data.frame(Comparativo_CVar_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_CVar_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_CVar_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_CVar_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_CVar_Horizon_Anual))
  if(nrow(Comparativo_CVar_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_CVar_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_CVar_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_CVar_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_CVar_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_CVar_Horizon_Anual2[nrow(Comparativo_CVar_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_CVar_Horizon_Anual = cbind(as.data.frame(Comparativo_CVar_Horizon_Anual), Eixo)
  Retornos=TestComparativo_CVar_Horizon_Anual[,1]
  Periodos=TestComparativo_CVar_Horizon_Anual$Eixo
  s = TestComparativo_CVar_Horizon_Anual$MARKOWITZ
  u = TestComparativo_CVar_Horizon_Anual$SHARPE
  h = TestComparativo_CVar_Horizon_Anual$MF_EQ
  z = TestComparativo_CVar_Horizon_Anual$MF_MKW
  p = TestComparativo_CVar_Horizon_Anual$MF_SHARPE
  w = TestComparativo_CVar_Horizon_Anual$ANNt_EQ
  t = TestComparativo_CVar_Horizon_Anual$ANNt_MKW
  q = TestComparativo_CVar_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "CVar Ratio",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_CVar_Horizon_Anual), max(Comparativo_CVar_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_CVar_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("CVar Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("CVar Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
# Graphic RCum

Plot_Annualized_RCum_Horizon <-function(){
  load('~/Comparativo_RCum_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  options(warn=-1)
  Eixo_X = rownames(Comparativo_RCum_Horizon_Anual[,1])
  nline = nrow(Comparativo_RCum_Horizon_Anual)
  Comparativo_RCum_Horizon_Anual = Comparativo_RCum_Horizon_Anual[rev(seq_len(nrow(Comparativo_RCum_Horizon_Anual))),]
  Comparativo_RCum_Horizon_Anual = as.data.frame(Comparativo_RCum_Horizon_Anual)
  nline = nrow(Comparativo_RCum_Horizon_Anual)



  #########################
  Until_Date=rownames(Comparativo_RCum_Horizon_Anual)[nrow(Comparativo_RCum_Horizon_Anual)]
  #Comparativo_RCum_Horizon_Anual=Comparativo_RCum_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_RCum_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_RCum_Horizon_Anual[nrow(Comparativo_RCum_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_RCum_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_RCum_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Corte= which(rownames(as.data.frame(Comparativo_RCum_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_RCum_Horizon_Anual
  Comparativo_RCum_Horizon_Anual=Comparativo_RCum_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]


  View(Comparativo_RCum_Horizon_Anual)
  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_RCum_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Max_RCum')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_RCum_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.max(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_RCum_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################

  png(file="~/Graphic_RCum_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_RCum_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_RCum_Horizon_Anual))
  Comparativo_RCum_Horizon_Anual2 = as.data.frame(Comparativo_RCum_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_RCum_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_RCum_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_RCum_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_RCum_Horizon_Anual))
  if(nrow(Comparativo_RCum_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_RCum_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_RCum_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_RCum_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_RCum_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_RCum_Horizon_Anual2[nrow(Comparativo_RCum_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_RCum_Horizon_Anual = cbind(as.data.frame(Comparativo_RCum_Horizon_Anual), Eixo)
  Retornos=TestComparativo_RCum_Horizon_Anual[,1]
  Periodos=TestComparativo_RCum_Horizon_Anual$Eixo
  s = TestComparativo_RCum_Horizon_Anual$MARKOWITZ
  u = TestComparativo_RCum_Horizon_Anual$SHARPE
  h = TestComparativo_RCum_Horizon_Anual$MF_EQ
  z = TestComparativo_RCum_Horizon_Anual$MF_MKW
  p = TestComparativo_RCum_Horizon_Anual$MF_SHARPE
  w = TestComparativo_RCum_Horizon_Anual$ANNt_EQ
  t = TestComparativo_RCum_Horizon_Anual$ANNt_MKW
  q = TestComparativo_RCum_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Cumulative Return Ratio",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_RCum_Horizon_Anual), max(Comparativo_RCum_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_RCum_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Cumulative Return Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Cumulative Return Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_RCum_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_RCum_Horizon_Anual))
  Comparativo_RCum_Horizon_Anual2 = as.data.frame(Comparativo_RCum_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_RCum_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_RCum_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_RCum_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_RCum_Horizon_Anual))
  if(nrow(Comparativo_RCum_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_RCum_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_RCum_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_RCum_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_RCum_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_RCum_Horizon_Anual2[nrow(Comparativo_RCum_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_RCum_Horizon_Anual = cbind(as.data.frame(Comparativo_RCum_Horizon_Anual), Eixo)
  Retornos=TestComparativo_RCum_Horizon_Anual[,1]
  Periodos=TestComparativo_RCum_Horizon_Anual$Eixo
  s = TestComparativo_RCum_Horizon_Anual$MARKOWITZ
  u = TestComparativo_RCum_Horizon_Anual$SHARPE
  h = TestComparativo_RCum_Horizon_Anual$MF_EQ
  z = TestComparativo_RCum_Horizon_Anual$MF_MKW
  p = TestComparativo_RCum_Horizon_Anual$MF_SHARPE
  w = TestComparativo_RCum_Horizon_Anual$ANNt_EQ
  t = TestComparativo_RCum_Horizon_Anual$ANNt_MKW
  q = TestComparativo_RCum_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Cumulative Return Ratio",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_RCum_Horizon_Anual), max(Comparativo_RCum_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_RCum_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Cumulative Return Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Cumulative Return Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
# Graphic Rm

Plot_Annualized_Rm_Horizon <-function(){
  load('~/Comparativo_Rm_Horizon_Anual.rda')
  load('~/N_Assets.rda')
  load('~/RM.rda')
  load('~/Type_ANN.rda')
  ANN_EQ = paste(Type_ANN,'_EQ', sep='')
  ANN_MKW = paste(Type_ANN,'_MKW', sep='')
  ANN_SHARPE = paste(Type_ANN,'_SHARPE', sep='')

  ydev=dev.list()
  if(class(ydev)!="NULL"){
    dev.off()
  }else{print('Starting Plot_Ratio_Horizon Command')}
  dev.capabilities()

  options(warn=-1)
  Eixo_X = rownames(Comparativo_Rm_Horizon_Anual[,1])
  nline = nrow(Comparativo_Rm_Horizon_Anual)
  Comparativo_Rm_Horizon_Anual = Comparativo_Rm_Horizon_Anual[rev(seq_len(nrow(Comparativo_Rm_Horizon_Anual))),]
  Comparativo_Rm_Horizon_Anual = as.data.frame(Comparativo_Rm_Horizon_Anual)
  nline = nrow(Comparativo_Rm_Horizon_Anual)



  #########################
  Until_Date=rownames(Comparativo_Rm_Horizon_Anual)[nrow(Comparativo_Rm_Horizon_Anual)]
  #Comparativo_Rm_Horizon_Anual=Comparativo_Rm_Horizon_Anual
  #  Corte=as.numeric(nrow(as.data.frame(Comparativo_Rm_Horizon_Anual)))

  if(Until_Date ==('')){
    #Until_Date = Final_Date_Testing
    Until_Date = rownames(Comparativo_Rm_Horizon_Anual[nrow(Comparativo_Rm_Horizon_Anual),])
  }

  if(length(which(rownames(Comparativo_Rm_Horizon_Anual)==Until_Date))==0){
    while(length(which(rownames(Comparativo_Rm_Horizon_Anual)==Until_Date))==0){
      dia=as.Date(Until_Date)
      new_day=dia+1
      Until_Date = as.character(new_day)
    }
  }

  Corte= which(rownames(as.data.frame(Comparativo_Rm_Horizon_Anual))==Until_Date)
  Coparativo_Backup = Comparativo_Rm_Horizon_Anual
  Comparativo_Rm_Horizon_Anual=Comparativo_Rm_Horizon_Anual[Corte:nrow(Comparativo_RETORNOS_Horizon_Anual),]

  View(Comparativo_Rm_Horizon_Anual)


  ######### Contador de Vitorias #################################################

  Base_Dif=t(Comparativo_Rm_Horizon_Anual)
  nc = ncol(Base_Dif)+2
  nr = nrow(Base_Dif)+1
  Analyzis=matrix(nrow=nr,ncol=nc)
  rownames(Analyzis)=c(rownames(Base_Dif),'Max_Rm')
  cnames=c(colnames(Base_Dif),'Vitories','Mean_Excedent')
  colnames(Analyzis)=cnames
  n=which(rownames(Analyzis)==Compare)
  Compar = Comparativo_Rm_Horizon_Anual[,n]


  for(i in 1:nrow(Base_Dif)){
    Vitories = 0
    Analyzis[i,1:ncol(Base_Dif)]=round(Base_Dif[i,]-Compar,2)
    for(j in 1:ncol(Base_Dif)){
      if(Analyzis[i,j]>0){
        Vitories=Vitories+1
      }
    }
    Analyzis[i,j+1]=Vitories
    Analyzis[i,j+2]=round(mean(Analyzis[i,1:ncol(Base_Dif)]),2)
  }
  for(j in 1:(ncol(Base_Dif)+2)){
    Analyzis[nrow(Analyzis),j]=rownames(Analyzis)[which.max(Analyzis[,j])]
  }
  Analyzis=data.frame(Analyzis)
  colnames(Analyzis)=cnames
  Nome_Compare=paste('~/Analyzis_Rm_Horizon_over_',Compare,'.rda',sep='')
  save(Analyzis, file=Nome_Compare)
  print(Analyzis)
  #################################################################################

  png(file="~/Graphic_Rm_Horizon.png", width=1920, height=1920, res=296, family = "A")
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,2,1,1))

  library("ggplot2")


  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Rm_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Rm_Horizon_Anual))
  Comparativo_Rm_Horizon_Anual2 = as.data.frame(Comparativo_Rm_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Rm_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Rm_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Rm_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Rm_Horizon_Anual))
  if(nrow(Comparativo_Rm_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Rm_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Rm_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Rm_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Rm_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Rm_Horizon_Anual2[nrow(Comparativo_Rm_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Rm_Horizon_Anual = cbind(as.data.frame(Comparativo_Rm_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Rm_Horizon_Anual[,1]
  Periodos=TestComparativo_Rm_Horizon_Anual$Eixo
  s = TestComparativo_Rm_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Rm_Horizon_Anual$SHARPE
  h = TestComparativo_Rm_Horizon_Anual$MF_EQ
  z = TestComparativo_Rm_Horizon_Anual$MF_MKW
  p = TestComparativo_Rm_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Rm_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Rm_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Rm_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Average Return Ratio",
       xlab = "Period",
       las =1,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Rm_Horizon_Anual), max(Comparativo_Rm_Horizon_Anual)))
  lines(s, col = c("brown"))
  lines(u, col = c("gray"))
  lines(h, col = c("yellow"))
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
  #title(main = paste("Comparativo_Rm_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Average Return Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Average Return Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE", "MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.8,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple","blue",
                 "green",
                 "darkgreen"))


  dev.off()
  ################################################################################
  #Presentation
  par(#mfrow=c(2,2),
    #mar=c(2,2,2,2),
    oma=c(1,1,1,1))

  library("ggplot2")
  windowsFonts(A=windowsFont("Times New Roman"))
  par(family="A", cex=0.8)

  Eixo = c(1:nrow(Comparativo_Rm_Horizon_Anual))
  Eixo_X = rownames(as.data.frame(Comparativo_Rm_Horizon_Anual))
  Comparativo_Rm_Horizon_Anual2 = as.data.frame(Comparativo_Rm_Horizon_Anual)
  #Eixo_X2 = c(1,
  #            round(nrow(Comparativo_Rm_Horizon_Anual)/4,0),
  #            round(nrow(Comparativo_Rm_Horizon_Anual)/2,0),
  #            round(nrow(Comparativo_Rm_Horizon_Anual)*3/4,0),
  #            nrow(Comparativo_Rm_Horizon_Anual))
  if(nrow(Comparativo_Rm_Horizon_Anual)>12) {Eixo_X2 = c(1, 5, 10, 15, 20, 25, 30)
  #} else{Eixo_X2 = c(1, 50, 100, 149)}
  } else{
    if(nrow(Comparativo_Rm_Horizon_Anual)>6) {Eixo_X2 = c(1, 3, 5, 7, 9, 11, 13)
    }else{Eixo_X2 = c(1:nrow(Comparativo_Rm_Horizon_Anual))}}
  Eixo_X3 = rownames(Comparativo_Rm_Horizon_Anual2[Eixo_X2,])
  Eixo_X3 = str_replace(Eixo_X3,"NA","")
  Inicio_data = rownames(Comparativo_Rm_Horizon_Anual2[1,])
  Fim_data = rownames(Comparativo_Rm_Horizon_Anual2[nrow(Comparativo_Rm_Horizon_Anual2),])
  #Fim_data = "2023-03-16"
  TestComparativo_Rm_Horizon_Anual = cbind(as.data.frame(Comparativo_Rm_Horizon_Anual), Eixo)
  Retornos=TestComparativo_Rm_Horizon_Anual[,1]
  Periodos=TestComparativo_Rm_Horizon_Anual$Eixo
  s = TestComparativo_Rm_Horizon_Anual$MARKOWITZ
  u = TestComparativo_Rm_Horizon_Anual$SHARPE
  h = TestComparativo_Rm_Horizon_Anual$MF_EQ
  z = TestComparativo_Rm_Horizon_Anual$MF_MKW
  p = TestComparativo_Rm_Horizon_Anual$MF_SHARPE
  w = TestComparativo_Rm_Horizon_Anual$ANNt_EQ
  t = TestComparativo_Rm_Horizon_Anual$ANNt_MKW
  q = TestComparativo_Rm_Horizon_Anual$ANNt_SHARPE
  plot(Periodos, Retornos,
       type ="l",
       xaxt = "n",
       ylab = "Average Return Ratio",
       xlab = "Period",
       las =1,
       lwd =2,
       #xaxp = c(1,nline, 5),
       ylim = c(min(Comparativo_Rm_Horizon_Anual), max(Comparativo_Rm_Horizon_Anual)))
  lines(s, col = c("brown"), lwd=2)
  lines(u, col = c("gray"), lwd=2)
  lines(h, col = c("yellow"), lwd=2)
  lines(z, col = c("red"), lwd=2)
  lines(p, col = c("purple"), lwd=2)
  lines(w, col = c("blue"), lwd=2)
  lines(t, col = c("green"), lwd=2)
  lines(q, col = c("darkgreen"), lwd=2)
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
  #title(main = paste("Comparativo_Rm_Horizon_Anual           ",
  #                 xlab= Inicio_data,"/", xlab= Fim_data), font.main=1, line=1.5)
  #title(paste("Average Return Ratio of Portfolios over the", RM,"Investment Horizon:", N_Assets, "Assets"))
  title(paste("Average Return Ratio of Portfolios over the", RM,"Investment Horizon"))
  #title(main = paste(
  # xlab= Inicio_data,"/", xlab= Fim_data),
  #line = 0.5,
  #cex = 0.5,
  #font.main = 1)


  ## Contador de vit?rias Buffet
  Contador_MF_DFA = matrix(nrow=149)
  legend(paste(Legend_position),
         #"bottomright",
         legend = c(RM, "MARKOWITZ", "SHARPE","MF_EQ", "MF_MKW", "MF_SHARPE",
                    ANN_EQ,
                    ANN_MKW, ANN_SHARPE),
         cex = 0.6,
         lty = 1,
         #bty = "o",
         bty = "n",
         lwd = 3,
         col = c("black", "brown", "gray", "yellow", "red",
                 "purple",
                 "blue",
                 "green",
                 "darkgreen"))
  save(Until_Date, file="~/Until_Date.rda")
}
################################################################################
################################################################################
## Graphic Select

if(Ratio=="Annualized_Returns"){
  Plot_Annualized_Returns_Horizon()
 }
if(Ratio=="Annualized_Volatility"){
  Plot_Annualized_Volatility_Horizon()
}
if(Ratio=="Sharpe"){
  Plot_Annualized_Sharpe_Horizon()
}
if(Ratio=="Alpha"){
  Plot_Annualized_Alpha_Horizon()
}
if(Ratio=="Beta"){
  Plot_Annualized_Beta_Horizon()
}
if(Ratio=="Sortino"){
  Plot_Annualized_Sortino_Horizon()
}
if(Ratio=="Treynor"){
  Plot_Annualized_Treynor_Horizon()
}
if(Ratio=="Var"){
  Plot_Annualized_Var_Horizon()
}
if(Ratio=="CVar"){
  Plot_Annualized_CVar_Horizon()
}
if(Ratio=="RCum"){
  Plot_Annualized_RCum_Horizon()
}
if(Ratio=="Rm"){
  Plot_Annualized_Rm_Horizon()
}


################################################################################
}
