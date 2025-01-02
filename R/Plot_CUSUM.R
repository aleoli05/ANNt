#' Plot_CUSUM
#' Prot graphic Cumulative Sum of Returns
#' @param Change Magnitude of change in the cumulative sum measured as the accepted standard deviation (sd) amount. Default is 1 sd
#' @param Control Control limit as a multiple of the sd. Default is 5 sd
#' @examples
#' Plot_CUSUM(1,5)
#' @export
Plot_CUSUM <- function(Change,Control){

  if(Change==''){
    Change=1
  }
  if(Control==''){
    Control=5
  }
#### Gr?fico CUSUM
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
load('~/Comparativo.rda')
load('~/Initial_Analysis_Date.rda')
load('~/Final_Analysis_Date.rda')
load('~/Final_Date_Testing.rda')

ydev=dev.list()
if(class(ydev)!="NULL"){
dev.off()
}else{print('Starting Plot_CUSUM Command')}
################################################################################
#### CUSUM
RetornoMedioMaxIS_RNAt=as.data.frame(Comparativo_RETORNOS)$ANNt_SHARPE
Number_obs=length(RetornoMedioMaxIS_RNAt)/1
Tabela_CUSUM = matrix(ncol=9,nrow=Number_obs)
Tabela_CUSUM = data.frame(Tabela_CUSUM)
colnames(Tabela_CUSUM)=c("Observation","Return", "Xi-Average+k", "SumCum+", "Average-k-Xi",
                         "Sumcum-", "LCS", "LCI", "Limites_Grafico")
Mudanca = Change
k= Mudanca/2*sd(RetornoMedioMaxIS_RNAt)
h= Control*(sd(RetornoMedioMaxIS_RNAt))
Limites_grafico = 2*Control*(sd(RetornoMedioMaxIS_RNAt))
Variacao_grafico = (2*Limites_grafico)/Number_obs

for (j in 1:(Number_obs)){
  if ((j-1)==0) {
    Anterior_1=0
    Anterior_2=0
  }
  else {
    Anterior_1 = Tabela_CUSUM[(j-1),4]
    Anterior_2 = Tabela_CUSUM[(j-1),6]
  }
  Tabela_CUSUM[j,1]= j
  Tabela_CUSUM[j,2]= RetornoMedioMaxIS_RNAt[j]
  Tabela_CUSUM[j,3]= RetornoMedioMaxIS_RNAt[j]-mean(RetornoMedioMaxIS_RNAt)+k
  Tabela_CUSUM[j,4]= max(0,Tabela_CUSUM[j,3]+Anterior_1)
  Tabela_CUSUM[j,5]= mean(RetornoMedioMaxIS_RNAt)-k-RetornoMedioMaxIS_RNAt[j]
  Tabela_CUSUM[j,6]= -max(0,Tabela_CUSUM[j,5]+Anterior_2)
  Tabela_CUSUM[j,7]= mean(RetornoMedioMaxIS_RNAt)+h
  Tabela_CUSUM[j,8]= mean(RetornoMedioMaxIS_RNAt)-h
  Tabela_CUSUM[j,9]= -Limites_grafico+j*Variacao_grafico
}

View(Tabela_CUSUM)
################################################################################

op <- par(new = TRUE)
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A")

png(file="~/Plot_CUSUM.png", width=1920, height=1200, res=296, family = "A")
cores=c("darkgreen","red", "blue","darkred")
Eixo = c(1:nrow(Comparativo))
Eixo_X = rownames(as.data.frame(Comparativo_RETORNOS))
Comparativo2 = as.data.frame(Comparativo_RETORNOS)
#Eixo_X2 = c(1,
#            round(nrow(Comparativo)/4,0),
#            round(nrow(Comparativo)/2,0),
#            round(nrow(Comparativo)*3/4,0),
#            nrow(Comparativo))
if(nrow(Comparativo_RETORNOS)>200) {Eixo_X2 = c(1, 100, 200, 300, 400, 500, 600)
} else {Eixo_X2 = c(1, 50, 100, 149, 200, 250, 300)}
Eixo_X3 = rownames(Comparativo2[Eixo_X2,])
Inicio_data = rownames(Comparativo2[1,])
Fim_data = rownames(Comparativo2[Number_obs,])
TestComparativo = cbind(as.data.frame(Comparativo_RETORNOS), Eixo)
Retornos=TestComparativo$SP500
Periodos=TestComparativo$Eixo


plot(Tabela_CUSUM[,1],Tabela_CUSUM[,9],
     ylab="Cumulative Sum", xlab="Observation",
     type = "n",
     #xaxt = "n",
     xlim = c(0, nrow(Tabela_CUSUM)),
     ylim = c(min(Tabela_CUSUM[,9]), max(Tabela_CUSUM[,9])))

lines(Tabela_CUSUM[,1],Tabela_CUSUM[,4],
      ylab="Cumulative Sum", xlab="Observation",
      type = "b",
      col="darkgreen",
      pch=19,
      #cex.lab = 0.8,
      #cex.axis = 0.8,
      cex=0.6
)
lines(Tabela_CUSUM[,1],Tabela_CUSUM[,6],
      ylab="Return", xlab="Observation",
      type = "b",
      col="red",
      pch=19,
      #cex.lab = 0.8,
      #cex.axis = 0.8,
      cex=0.6
)
lines (Tabela_CUSUM[,1],Tabela_CUSUM[,7],
       ylab="Return", xlab="Observation",
       type = "l",
       col="blue",
       #cex.lab = 0.8,
       #cex.axis = 0.8,
)
lines(Tabela_CUSUM[,1],Tabela_CUSUM[,8],
      ylab="Return", xlab="Observation",
      type = "l",
      col="darkred",
      #cex.lab = 0.8,
      #cex.axis = 0.8,
)
text(x=max(Tabela_CUSUM[,1]-20), y=max(Tabela_CUSUM[,9]),
     label=paste("H =",Control), cex = 0.6
)
#title("Graphic Cumulative Sum of Returns")
title(paste("Graphic Cumulative Sum of Returns:", N_Assets, "Assets"))
title(main = paste(
  xlab= Inicio_data,"/", xlab= Fim_data),
  #xlab= Inicio_data,"/", xlab= "2023-03-17"),
  line = 0.5,
  cex = 0.5,
  font.main = 1)
#text(x=Base_Palomar[4,],y=Base_Palomar[1,],
#
legend("bottomright", cex= 0.6, legend = c(    "CuSum+",
                                               "CuSum-",
                                               "ULC",
                                               "LLC"),
       pch = 19,
       #lwd = 3,
       #bty = "o",
       bty = "n",
       col = cores)
#axis(1, at=(Eixo_X2), label = Eixo_X3)
#axis(4, las=1)
#box.col = "white")
dev.off()

################################################################################

cores=c("darkgreen","red", "blue","darkred")
Eixo = c(1:nrow(Comparativo))
Eixo_X = rownames(as.data.frame(Comparativo_RETORNOS))
Comparativo2 = as.data.frame(Comparativo_RETORNOS)
#Eixo_X2 = c(1,
#            round(nrow(Comparativo)/4,0),
#            round(nrow(Comparativo)/2,0),
#            round(nrow(Comparativo)*3/4,0),
#            nrow(Comparativo))
if(nrow(Comparativo_RETORNOS)>200) {Eixo_X2 = c(1, 100, 200, 300, 400, 500, 600)
} else {Eixo_X2 = c(1, 50, 100, 149, 200, 250, 300)}
Eixo_X3 = rownames(Comparativo2[Eixo_X2,])
Inicio_data = rownames(Comparativo2[1,])
Fim_data = rownames(Comparativo2[Number_obs,])
TestComparativo = cbind(as.data.frame(Comparativo_RETORNOS), Eixo)
Retornos=TestComparativo[1]
Periodos=TestComparativo$Eixo

op <- par(new = TRUE)
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A")
plot(Tabela_CUSUM[,1],Tabela_CUSUM[,9],
     ylab="Cumulative Sum", xlab="Observation",
     type = "n",
     #xaxt = "n",
     xlim = c(0, nrow(Tabela_CUSUM)),
     ylim = c(min(Tabela_CUSUM[,9]), max(Tabela_CUSUM[,9])))

lines(Tabela_CUSUM[,1],Tabela_CUSUM[,4],
      ylab="Cumulative Sum", xlab="Observation",
      type = "b",
      col="darkgreen",
      pch=19,
      #cex.lab = 0.8,
      #cex.axis = 0.8,
      cex=0.6
)
lines(Tabela_CUSUM[,1],Tabela_CUSUM[,6],
      ylab="Return", xlab="Observation",
      type = "b",
      col="red",
      pch=19,
      #cex.lab = 0.8,
      #cex.axis = 0.8,
      cex=0.6
)
lines (Tabela_CUSUM[,1],Tabela_CUSUM[,7],
       ylab="Return", xlab="Observation",
       type = "l",
       col="blue",
       #cex.lab = 0.8,
       #cex.axis = 0.8,
)
lines(Tabela_CUSUM[,1],Tabela_CUSUM[,8],
      ylab="Return", xlab="Observation",
      type = "l",
      col="darkred",
      #cex.lab = 0.8,
      #cex.axis = 0.8,
)
text(x=max(Tabela_CUSUM[,1]-20), y=max(Tabela_CUSUM[,9]),
     label=paste("H =",Control), cex = 0.6
)
#title("Graphic Cumulative Sum of Returns")
title(paste("Graphic Cumulative Sum of Returns:", N_Assets, "Assets"))
title(main = paste(
  xlab= Inicio_data,"/", xlab= Fim_data),
  #xlab= Inicio_data,"/", xlab= "2023-03-17"),
  line = 0.5,
  cex = 0.5,
  font.main = 1)
#text(x=Base_Palomar[4,],y=Base_Palomar[1,],
#
legend("bottomright", cex= 0.6, legend = c(    "CuSum+",
                                               "CuSum-",
                                               "ULC",
                                               "LLC"),
       pch = 19,
       #lwd = 3,
       #bty = "o",
       bty = "n",
       col = cores)
#axis(1, at=(Eixo_X2), label = Eixo_X3)
#axis(4, las=1)
#box.col = "white")


}
