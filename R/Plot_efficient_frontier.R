#' Plot_efficient_frontier
#' Generate the efficient frontier graph
#' @param No require parameters
#' @examples
#' Plot_efficient_frontier()
#' @export
Plot_efficient_frontier <- function(){



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
load('~/mean_sharpe.rda')
load('~/sd_sharpe.rda')
load('~/GMV_Return.rda')
load('~/GMV_sd.rda')
load('~/sd_MKW.rda')
load('~/mean_MKW.rda')
load('~/RM.rda')

ydev=dev.list()
if(class(ydev)!="NULL"){
  dev.off()
}else{print('Starting Plot_efficient_frontier Command')}

Inicio_data = Initial_Analysis_Date
Fim_data = Final_Analysis_Date

 #if (ncol(Base_Palomar)>10){
  #GMV_Return=1
#  GMV_sd=1
#}


#sd_sharpe = sd(as.data.frame(Comparativo_RETORNOS)$SHARPE)
#mean_sharpe = mean(as.data.frame(Comparativo_RETORNOS)$SHARPE)
attach(as.data.frame(Base_Palomar))
attach(as.data.frame(Comparativo_RETORNOS))

cores = NULL
for (i in 1:ncol(Base_Palomar)){
  cores[i] = "grey"}
MF_DFA = as.data.frame(Classificacao_MFractal)[1:N_Assets]
ANNt = Type_ANNt[,1:N_Assets]
max_frame = as.data.frame(max_frame)
Sharpe_ativos = t(filter(max_frame,max_frame>0))
Base_Palomar_frame = as.data.frame(Base_Palomar)
for(i in 1:ncol(Base_Palomar)){
  for(j in 1:ncol(ANNt)){
    if (colnames(as.data.frame(Base_Palomar_frame[i]))==colnames(ANNt[j])){
      cores[i]="red"
    }
    if (colnames(as.data.frame(Base_Palomar_frame[i]))==colnames(MF_DFA[j])){
      cores[i]="blue"
    }
  }
}


###############################################################################
#### Cores dos gr?ficos
#cores = NULL
#for (i in 1:ncol(Base_Palomar)){
#  cores[i] = "grey"}
##MF_DFA = Classificacao_MFractal[1:5]
#ANNt = Type_ANNt[,1:N_Assets]
##max_frame = as.data.frame(maxSR.weight.rp)
#max_frame = as.data.frame(weight_Sharpe)
#assets_max_frame <- str_replace(rownames(max_frame),"w.","")
#rownames(max_frame)=assets_max_frame

#Sharpe_ativos = t(filter(max_frame,max_frame>0))
#Base_Palomar_frame = as.data.frame(Base_Palomar)
#for(i in 1:ncol(Base_Palomar)){
 # for(j in 1:ncol(ANNt)){
  #  if (colnames(as.data.frame(Base_Palomar_frame[i]))==colnames(ANNt[j])){
   #   cores[i]="red"
  #  }
   # if (colnames(as.data.frame(Base_Palomar_frame[i]))==colnames(BF[j])){
    #  cores[i]="blue"
  #  }
  #}
#}

################################################################################

#### Fronteira Eficiente por Desvio
x1 = (sd_sharpe+0.05)/9
xn = NULL
xn[1]=0
for (i in 2:10){xn[i] = x1*i}
yn= (mean_sharpe-((1+Rf)^(1/252)-1))/sd_sharpe*xn +((1+Rf)^(1/252)-1)

op <- par(new = TRUE)
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A")


############################################################################################
ydev=dev.list()
if(class(ydev)!="NULL"){
dev.off()
}
par(mfrow=c(2,2), mar=c(4,4,1,1), oma=c(1,2,2,1))
#par(mfrow=c(1,2), oma=(c(4,1,1,1)))
limite_y=NULL
if(max(riscosAlvo)<0.01){
  limite_max_x=1.1*(max(riscosAlvo))
  limite_min_x=0.5*min(riscosAlvo)
}else {
  limite_min_x=0.001
  limite_max_x=0.055}
png(file="~/Efficiente_frontier.png", width=1920, height=1200, res=296, family = "A")

plot(Base_Palomar[2,],Base_Palomar[1,],
     ylab="Return", xlab="Standard Deviation",
     col= "lightgray",
     family="A",
     #col= cores,
     #cex.lab = 0.8,
     #cex.axis = 0.8,
     xlim = c(limite_min_x, limite_max_x))
#title("Efficient Frontier", line =1.5)
title(paste("Efficient Frontier:", N_Assets, "Assets"),line = 1.5)
title(main = paste(
  xlab= Inicio_data,"/", xlab= Fim_data),
  #xlab= Inicio_data,"/", xlab= "2023-03-17"),
  line = 0.5,
  cex = 0.5,
  font.main = 1)
# text(x=Base_Palomar[2,],y=Base_Palomar[1,],
#   labels = colnames(Base_Palomar),
#  col=cores,
# cex = 0.6,
#adj = -0.2)
lines(x=riscosAlvo, y= retornoAlvos)
#points(Mkw[2,],Mkw[1,],#,main="Fronteira Eficiente por Desvio",
#       #ylab="Retorno", xlab="Desvio-Padr?o",
#       col="brown",
#       #xlim = c(0.01, 0.07))
#)
#text(x=Mkw[2,],y=Mkw[1,],
#     labels = colnames(Mkw),
#     col="brown",
#     cex = 0.6,
#     adj = -0.2
#)
points(Sharpe[2,],Sharpe[1,],#,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="black",
       #xlim = c(0.01, 0.07))
)
#text(x=Sharpe[2,],y=Sharpe[1,],
#     labels = colnames(Sharpe),
#     col="darkblue",
#     cex = 0.6,
#     adj = -0.2
#)
#points(max_i_sharpe_[,492],max_i_sharpe_[,491],#,main="Fronteira Eficiente por Desvio",
#ylab="Retorno", xlab="Desvio-Padr?o",
#       col="red",
#xlim = c(0.01, 0.07))
#)
#text(x=max_i_sharpe_[,492],y=max_i_sharpe_[,491],
#     labels = colnames(Sharpe),
#     col="red",
#     cex = 0.6,
#     adj = -0.2
#)
points(MF[2,],MF[1,],#,main="Efficient Frontier",
       #ylab="Return", xlab="Standard Deviation",
       col="purple",
       #xlim = c(0.01, 0.07))
)
#text(x=MF[2,],y=MF[1,],
#     labels = colnames(MF),
#     col="red",
#     cex = 0.6,
#     adj = -0.2
#)
points(sd(MF_MKW),mean(MF_MKW),
       #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="red",
       #xlim = c(0.01, 0.07),
       pch=19
)
#text(x=sd(MF_MKW), y=mean(MF_MKW),
#     labels = "MF_MKW",
#     col="red",
#     font = 2,
#     cex = 0.6,
#     adj = -0.2
#)
points(sd(MF_SHARPE),mean(MF_SHARPE),
       #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="purple",
       #xlim = c(0.01, 0.07),
       pch=19
)
#text(x=sd(MF_SHARPE), y=mean(MF_SHARPE),
#     labels = "MF_SHARPE",
#     col="purple",
#     font = 2,
#     cex = 0.6,
#     adj = -0.2
#)
points(sd(ANNt_EQ),mean(ANNt_EQ),
       #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="blue",
       #xlim = c(0.01, 0.07),
       pch=19
)
#text(x=sd(ANNt_EQ), y=mean(ANNt_EQ),
#     labels = "ANNt_EQ",
#     col="blue",
#     font = 2,
#     cex = 0.6,
#     adj = -0.2
#)
points(sd(ANNt_MKW),mean(ANNt_MKW),
       #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="green",
       #xlim = c(0.01, 0.07),
       pch=19
)
#text(x=sd(ANNt_MKW), y=mean(ANNt_MKW),
#     labels = "ANNt_MKW",
#     col="green",
#     font = 2,
#     cex = 0.6,
#     adj = -0.2
#)
points(RNAt[2,],RNAt[1,],#,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="darkgreen",
       #xlim = c(0.01, 0.07))
)
#text(x=RNAt[2,],y=RNAt[1,],
#     labels = colnames(RNAt),
#     col="darkgreen",
#     cex = 0.6,
#     adj = -0.2
#)

lines(x=xn,y=yn,
      #,main="Fronteira Eficiente por Desvio",
      #ylab="Retorno", xlab="Desvio-Padr?o",
      col="black",
      #xlim = c(0.01, 0.07),
      lty=1
)
text(x=0, y=(1+Rf)^(1/252)-1,
     labels = "RF",
     col="black",
     font = 2,
     cex = 0.6,
     adj = -0.2
)
points(sd_sharpe,mean_sharpe, col="darkgray", pch = 19)
#text(sd_sharpe,mean_sharpe,
#     labels = "SHARPE",
#     col="darkgray",
#     cex = 0.6,
#     font=2,
#     adj = -0.2
#)
colnames(as.data.frame(Comparativo_RETORNOS)[,1])
points(sd(Comparativo_RETORNOS[,1]),mean(Comparativo_RETORNOS[,1]), col="black", pch = 19)
text(sd(Comparativo_RETORNOS[,1]),mean(Comparativo_RETORNOS[,1]),
     labels = colnames(as.data.frame(Comparativo_RETORNOS)[1]),
     col="black",
     cex = 0.6,
     font=2,
     adj = -0.2
)
#points(sd(RetornoMedioMaxIS),Media_RetornoMedioMaxIS, col="red")
#text(sd(RetornoMedioMaxIS),Media_RetornoMedioMaxIS,
#     labels = "Sharpe",
#     col="red",
#     cex = 0.6,
#     adj = -0.2
#)
points(sd_MKW, mean_MKW, col="brown", pch=19)
#text(sd(MARKOWITZ),mean(MARKOWITZ),
#     labels = "MARKOWITZ",
#     col="brown",
#     cex = 0.6,
#     font =2,
#     adj = -0.2
#)
points(sd(ANNt_SHARPE),mean(ANNt_SHARPE), col="darkgreen", pch=19)
text(sd(ANNt_SHARPE),mean(ANNt_SHARPE),
     labels = "ANN-t_SHARPE",
     col="darkgreen",
     font=2,
     cex = 0.6,
     adj = -0.2
)
points(x=GMV_sd,y=GMV_Return, col="orange", pch=19)
text(x=GMV_sd, y=GMV_Return,
     labels = "GMV",
     col="orange",
     font = 2,
     cex = 0.6,
     adj = 1.2
)

legend(x="topright",
       #legend=c(colnames(as.data.frame(Comparativo_RETORNOS)[1]), "MF_SHARPE", "ANNt_EQ" , "ANNt_MKW","ANNt_SHARPE",
       #        "MARKOWITZ","SHARPE"),
       legend = c(RM, "MARKOWITZ", "SHARPE", "MF_MKW", "MF_SHARPE",
                  "ANNt_Eq",
                  "ANNt_MKW", "ANNt_SHARPE", "GMV"),
       #text.col = c("black","red","blue","green","darkgreen", "brown", "darkblue"),
       text.col=c("black", "brown", "darkgray", "red", "purple","blue",  "green", "darkgreen","orange"),
       text.font=2,
       pch = 19,
       #col=c("black","red","blue","green","darkgreen", "brown", "darkblue"),
       col=c("black", "brown", "darkgray", "red", "purple","blue",  "green", "darkgreen","orange"),
       bty = "n",
       cex = 0.6
)

dev.off()



############################################################################################

windowsFonts(A=windowsFont("Times New Roman"))
par(family="A")
par(mfrow=c(1,1), mar=c(4,4,2.5,1), oma=c(1,2,2,1))
plot(Base_Palomar[2,],Base_Palomar[1,],
     ylab="Return", xlab="Standard Deviation",
     col= "lightgray",
     family="A",
     #col= cores,
     #cex.lab = 0.8,
     #cex.axis = 0.8,
     xlim = c(limite_min_x, limite_max_x))
#title("Efficient Frontier", line =1.5)
title(paste("Efficient Frontier:", N_Assets, "Assets"),line = 1.5)
title(main = paste(
  xlab= Inicio_data,"/", xlab= Fim_data),
  #xlab= Inicio_data,"/", xlab= "2023-03-17"),
  line = 0.5,
  cex = 0.5,
  font.main = 1)
# text(x=Base_Palomar[2,],y=Base_Palomar[1,],
#   labels = colnames(Base_Palomar),
#  col=cores,
# cex = 0.6,
#adj = -0.2)
lines(x=riscosAlvo, y= retornoAlvos)
#points(Mkw[2,],Mkw[1,],#,main="Fronteira Eficiente por Desvio",
#       #ylab="Retorno", xlab="Desvio-Padr?o",
#       col="brown",
#       #xlim = c(0.01, 0.07))
#)
#text(x=Mkw[2,],y=Mkw[1,],
#     labels = colnames(Mkw),
#     col="brown",
#     cex = 0.6,
#     adj = -0.2
#)
points(Sharpe[2,],Sharpe[1,],#,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="black",
       #xlim = c(0.01, 0.07))
)
#text(x=Sharpe[2,],y=Sharpe[1,],
#     labels = colnames(Sharpe),
#     col="darkblue",
#     cex = 0.6,
#     adj = -0.2
#)
#points(max_i_sharpe_[,492],max_i_sharpe_[,491],#,main="Fronteira Eficiente por Desvio",
#ylab="Retorno", xlab="Desvio-Padr?o",
#       col="red",
#xlim = c(0.01, 0.07))
#)
#text(x=max_i_sharpe_[,492],y=max_i_sharpe_[,491],
#     labels = colnames(Sharpe),
#     col="red",
#     cex = 0.6,
#     adj = -0.2
#)
points(MF[2,],MF[1,],#,main="Efficient Frontier",
       #ylab="Return", xlab="Standard Deviation",
       col="purple",
       #xlim = c(0.01, 0.07))
)
#text(x=MF[2,],y=MF[1,],
#     labels = colnames(MF),
#     col="red",
#     cex = 0.6,
#     adj = -0.2
#)
points(sd(MF_MKW),mean(MF_MKW),
       #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="red",
       #xlim = c(0.01, 0.07),
       pch=19
)
#text(x=sd(MF_MKW), y=mean(MF_MKW),
#     labels = "MF_MKW",
#     col="red",
#     font = 2,
#     cex = 0.6,
#     adj = -0.2
#)
points(sd(MF_SHARPE),mean(MF_SHARPE),
       #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="purple",
       #xlim = c(0.01, 0.07),
       pch=19
)
#text(x=sd(MF_SHARPE), y=mean(MF_SHARPE),
#     labels = "MF_SHARPE",
#     col="purple",
#     font = 2,
#     cex = 0.6,
#     adj = -0.2
#)
points(sd(ANNt_EQ),mean(ANNt_EQ),
       #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="blue",
       #xlim = c(0.01, 0.07),
       pch=19
)
#text(x=sd(ANNt_EQ), y=mean(ANNt_EQ),
#     labels = "ANNt_EQ",
#     col="blue",
#     font = 2,
#     cex = 0.6,
#     adj = -0.2
#)
points(sd(ANNt_MKW),mean(ANNt_MKW),
       #,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="green",
       #xlim = c(0.01, 0.07),
       pch=19
)
#text(x=sd(ANNt_MKW), y=mean(ANNt_MKW),
#     labels = "ANNt_MKW",
#     col="green",
#     font = 2,
#     cex = 0.6,
#     adj = -0.2
#)
points(RNAt[2,],RNAt[1,],#,main="Fronteira Eficiente por Desvio",
       #ylab="Retorno", xlab="Desvio-Padr?o",
       col="darkgreen",
       #xlim = c(0.01, 0.07))
)
#text(x=RNAt[2,],y=RNAt[1,],
#     labels = colnames(RNAt),
#     col="darkgreen",
#     cex = 0.6,
#     adj = -0.2
#)

lines(x=xn,y=yn,
      #,main="Fronteira Eficiente por Desvio",
      #ylab="Retorno", xlab="Desvio-Padr?o",
      col="black",
      #xlim = c(0.01, 0.07),
      lty=1
)
text(x=0, y=(1+Rf)^(1/252)-1,
     labels = "RF",
     col="black",
     font = 2,
     cex = 0.6,
     adj = -0.2
)
points(sd_sharpe,mean_sharpe, col="darkgray", pch = 19)
#text(sd_sharpe,mean_sharpe,
#     labels = "SHARPE",
#     col="darkgray",
#     cex = 0.6,
#     font=2,
#     adj = -0.2
#)
colnames(as.data.frame(Comparativo_RETORNOS)[,1])
points(sd(Comparativo_RETORNOS[,1]),mean(Comparativo_RETORNOS[,1]), col="black", pch = 19)
text(sd(Comparativo_RETORNOS[,1]),mean(Comparativo_RETORNOS[,1]),
     labels = colnames(as.data.frame(Comparativo_RETORNOS)[1]),
     col="black",
     cex = 0.6,
     font=2,
     adj = -0.2
)
#points(sd(RetornoMedioMaxIS),Media_RetornoMedioMaxIS, col="red")
#text(sd(RetornoMedioMaxIS),Media_RetornoMedioMaxIS,
#     labels = "Sharpe",
#     col="red",
#     cex = 0.6,
#     adj = -0.2
#)
points(sd(MARKOWITZ),mean(MARKOWITZ), col="brown", pch=19)
#text(sd(MARKOWITZ),mean(MARKOWITZ),
#     labels = "MARKOWITZ",
#     col="brown",
#     cex = 0.6,
#     font =2,
#     adj = -0.2
#)
points(sd(ANNt_SHARPE),mean(ANNt_SHARPE), col="darkgreen", pch=19)
text(sd(ANNt_SHARPE),mean(ANNt_SHARPE),
     labels = "ANNt_SHARPE",
     col="darkgreen",
     font=2,
     cex = 0.6,
     adj = -0.2
)

points(x=GMV_sd,y=GMV_Return, col="orange", pch=19)
text(x=GMV_sd, y=GMV_Return,
     labels = "GMV",
     col="orange",
     font = 2,
     cex = 0.6,
     adj = 1.2
)

legend(x="topright",
       #legend=c(colnames(as.data.frame(Comparativo_RETORNOS)[1]), "MF_SHARPE", "ANNt_EQ" , "ANNt_MKW","ANNt_SHARPE",
       #        "MARKOWITZ","SHARPE"),
       legend = c(RM, "MARKOWITZ", "SHARPE", "MF_MKW", "MF_SHARPE",
                  "ANNt_Eq",
                  "ANNt_MKW", "ANNt_SHARPE", "GMV"),
       text.font=2,
       #text.col = c("black","red","blue","green","darkgreen", "brown", "darkblue"),
       text.col=c("black", "brown", "darkgray", "red", "purple","blue",  "green", "darkgreen","orange"),
       pch = 19,
       #col=c("black","red","blue","green","darkgreen", "brown", "darkblue"),
       col=c("black", "brown", "darkgray", "red", "purple","blue",  "green", "darkgreen","orange"),
       bty = "n",
       cex = 0.6
)




}
