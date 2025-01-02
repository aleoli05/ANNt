#' Plot New efficient frontier:
#' generate the new efficient frontier
#' @description
#' This chart presents the assets most likely to outperform the benchmark.
#' The selection of assets must occur from right to left, as indicated by the arrow.
#' Allows you to select the assets that should be included in the portfolio to obtain high performance.
#' @param () No require parameters
#' @examples
#' Plot_New_efficient_frontier()
#' @export
Plot_New_efficient_frontier<-function(){

#### Fronteira Eficiente de Probabilidade
#### Cores dos gr?ficos
load('~/Base_Palomar.rda')
load('~/Rf.rda')
load('~/N_Assets.rda')
load('~/max_frame.rda')
load('~/fronteiraEficiente.rda')
load('~/Type_ANNt.rda')
load('~/Mkw.rda')
load('~/RNAt.rda')
load('~/MF.rda')
load('~/Sharpe.rda')
load('~/riscosAlvo.rda')
load('~/retornoAlvos.rda')
load('~/Classificacao_MFractal.rda')
load('~/Comparativo_RETORNOS.rda')
load('~/Initial_Analysis_Date.rda')
load('~/Final_Analysis_Date.rda')

ydev=dev.list()
if(class(ydev)!="NULL"){
  dev.off()
}else{print('Starting Plot__New_efficient_frontier Command')}
dev.capabilities()

attach(as.data.frame(Base_Palomar))
attach(as.data.frame(Comparativo_RETORNOS))
Inicio_data = Initial_Analysis_Date
Fim_data = Final_Analysis_Date

#### Fronteira Eficiente por Desvio

sd_sharpe = sd(as.data.frame(Comparativo_RETORNOS)$SHARPE)
mean_sharpe = mean(as.data.frame(Comparativo_RETORNOS)$SHARPE)
attach(as.data.frame(Base_Palomar))
attach(as.data.frame(Comparativo_RETORNOS))

x1 = (sd_sharpe+0.05)/9
xn = NULL
xn[1]=-0.02
for (i in 2:20){xn[i] = x1*i}
yn= (mean_sharpe-((1+Rf)^(1/252)-1))/sd_sharpe*xn +((1+Rf)^(1/252)-1)

########################################################################################
ydev=dev.list()
if(class(ydev)!="NULL"){
dev.off()
}
op <- par(new = TRUE)
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A")


png(file="~/New_Efficient_Frontier.png", width=1920, height=1200, res=296, family = "A")
plot(Base_Palomar[4,],Base_Palomar[1,],
     ylab="Return", xlab="Return Probability > Return Benchmark",
     col="lightgrey",
     family="A",
     #cex.lab = 0.8,
     #cex.axis = 0.8,
     xlim = c(0.4, 0.570))
title("New Efficient Frontier (NEF)")
title(main = paste(
  xlab= Inicio_data,"/", xlab= Fim_data),
  #xlab= Inicio_data,"/", xlab= "2023-03-17"),
  line = 0.5,
  cex = 0.5,
  font.main = 1)
#text(x=Base_Palomar[4,],y=Base_Palomar[1,],
#    labels = colnames(Base_Palomar),
#   col=cores,
#  cex = 0.6,
# adj = -0.2)
points(Mkw[4,],Mkw[1,],#,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="brown",
       #xlim = c(0.01, 0.07))
)
text(x=Mkw[4,],y=Mkw[1,],
     labels = colnames(Mkw),
     col="brown",
     cex = 0.6,
     adj = -0.2
)
points(Sharpe[4,],Sharpe[1,],#,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="darkblue",
       #xlim = c(0.01, 0.07))
)
text(x=Sharpe[4,],y=Sharpe[1,],
     labels = colnames(Sharpe),
     col="darkblue",
     cex = 0.6,
     adj = -0.2
)
points(MF[4,],MF[1,],#,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="red",
       #xlim = c(0.01, 0.07))
)
text(x=MF[4,],y=MF[1,],
     labels = colnames(MF),
     col="red",
     cex = 0.6,
     adj = -0.2
)
points(RNAt[4,],RNAt[1,],#,main="Eficient Frontier for Desviation",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="darkgreen",
       #xlim = c(0.01, 0.07))
)
text(x=RNAt[4,],y=RNAt[1,],
     labels = colnames(RNAt),
     col="darkgreen",
     cex = 0.6,
     adj = -0.2
)
lines(x=c(rep(max(Base_Palomar[4,]), length(yn))),y=yn,
      #,main="Fronteira Eficiente por Desvio",
      #ylab="Retorno", xlab="Desvio-Padr?o",
      col="black",
      #xlim = c(0.01, 0.07),
      lty=1
)
text(x=(max(Base_Palomar[4,])+0.02), y=mean(Base_Palomar[1,])+0.0002,
     labels = "NEF",
     col="black",
     cex = 0.6,
     adj = 0
)
arrows(x0 = (max(Base_Palomar[4,])+0.03), y0 = mean(Base_Palomar[1,]),
       x1 = max(Base_Palomar[4,]), y1 = mean(Base_Palomar[1,]),
       length = 0.1)
legend(x="topleft",
       legend=c("SP500", "MF_SHARPE", "ANNt",
                "MARKOWITZ","SHARPE"),
       text.col = c("black","red","darkgreen", "brown", "darkblue"),
       pch = 1,
       col=c("black","red","darkgreen", "brown", "darkblue"),
       bty = "n",
       cex = 0.6
)
dev.off()

################################################################################

op <- par(new = TRUE)
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A")

plot(Base_Palomar[4,],Base_Palomar[1,],
     ylab="Return", xlab="Return Probability > Return Benchmark",
     col="lightgrey",
     family="A",
     #cex.lab = 0.8,
     #cex.axis = 0.8,
     xlim = c(0.4, 0.560))
title("New Efficient Frontier (NEF)")
title(main = paste(
  xlab= Inicio_data,"/", xlab= Fim_data),
  #xlab= Inicio_data,"/", xlab= "2023-03-17"),
  line = 0.5,
  cex = 0.5,
  font.main = 1)
#text(x=Base_Palomar[4,],y=Base_Palomar[1,],
#    labels = colnames(Base_Palomar),
#   col=cores,
#  cex = 0.6,
# adj = -0.2)
points(Mkw[4,],Mkw[1,],#,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="brown",
       #xlim = c(0.01, 0.07))
)
text(x=Mkw[4,],y=Mkw[1,],
     labels = colnames(Mkw),
     col="brown",
     cex = 0.6,
     adj = -0.2
)
points(Sharpe[4,],Sharpe[1,],#,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="darkblue",
       #xlim = c(0.01, 0.07))
)
text(x=Sharpe[4,],y=Sharpe[1,],
     labels = colnames(Sharpe),
     col="darkblue",
     cex = 0.6,
     adj = -0.2
)
points(MF[4,],MF[1,],#,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="red",
       #xlim = c(0.01, 0.07))
)
text(x=MF[4,],y=MF[1,],
     labels = colnames(MF),
     col="red",
     cex = 0.6,
     adj = -0.2
)
points(RNAt[4,],RNAt[1,],#,main="Eficient Frontier for Desviation",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="darkgreen",
       #xlim = c(0.01, 0.07))
)
text(x=RNAt[4,],y=RNAt[1,],
     labels = colnames(RNAt),
     col="darkgreen",
     cex = 0.6,
     adj = -0.2
)
lines(x=c(rep(max(Base_Palomar[4,]), length(yn))),y=yn,
      #,main="Fronteira Eficiente por Desvio",
      #ylab="Retorno", xlab="Desvio-Padr?o",
      col="black",
      #xlim = c(0.01, 0.07),
      lty=1
)
text(x=(max(Base_Palomar[4,])+0.02), y=mean(Base_Palomar[1,])+0.0002,
     labels = "NEF",
     col="black",
     cex = 0.6,
     adj = 0
)
arrows(x0 = (max(Base_Palomar[4,])+0.03), y0 = mean(Base_Palomar[1,]),
       x1 = max(Base_Palomar[4,]), y1 = mean(Base_Palomar[1,]),
       length = 0.1)
legend(x="topleft",
       legend=c("SP500", "MF_SHARPE", "ANNt",
                "MARKOWITZ","SHARPE"),
       text.col = c("black","red","darkgreen", "brown", "darkblue"),
       pch = 1,
       col=c("black","red","darkgreen", "brown", "darkblue"),
       bty = "n",
       cex = 0.6
)




}
