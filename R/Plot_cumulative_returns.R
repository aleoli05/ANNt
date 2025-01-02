#' Plot_Cumulative_Returns
#' Present Cumulative Returns Graph
#'@param Until_Date if '' is the sys.Date()
#'@examples
#'Until_Date <- '2023-09-15'
#'Plot_Cumulative_Returns('')


#'@export
Plot_Cumulative_Returns <- function(Until_Date) {

##############################################################################

library(stringr)
load('~/Comparativo.rda')
  load('~/N_Assets.rda')
  load('~/Final_Date_Testing.rda')
  load('~/RM.rda')
attach(as.data.frame(Comparativo))

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

if(length(which(rownames(Comparativo)==Until_Date))==0){
  while(length(which(rownames(Comparativo)==Until_Date))==0){
    dia=as.Date(Until_Date)
    new_day=dia-1
    Until_Date = as.character(new_day)
  }
}

Corte= which(rownames(as.data.frame(Comparativo))==as.Date(Until_Date))
Coparativo_Backup = Comparativo
Comparativo=Comparativo[1:Corte,]



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
if(nrow(Comparativo)>300) {Eixo_X2 = c(1, 100, 200, 300, 400, 500, 600)
#} else{Eixo_X2 = c(1, 50, 100, 149)}
} else{
  if(nrow(Comparativo)>100) {Eixo_X2 = c(1, 50, 100, 150, 200, 250, 300)
  }else{Eixo_X2 = c(1,25,50,75,100)}}
Eixo_X3 = rownames(Comparativo2[Eixo_X2,])
Eixo_X3 = str_replace(Eixo_X3,"NA","")
Inicio_data = rownames(Comparativo2[1,])
Fim_data = rownames(Comparativo2[nrow(Comparativo2),])
#Fim_data = "2023-03-16"
TestComparativo = cbind(as.data.frame(Comparativo), Eixo)
Retornos=TestComparativo[,1]
Periodos=TestComparativo$Eixo
s = TestComparativo$MARKOWITZ
u = TestComparativo$SHARPE
z = TestComparativo$MF_MKW
p = TestComparativo$MF_SHARPE
w = TestComparativo$ANNt_EQ
t = TestComparativo$ANNt_MKW
q = TestComparativo$ANNt_SHARPE
plot(Periodos, Retornos,
     family="A",
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
title(paste("ANNt and Others Portfolios:", N_Assets, "Assets"))
#title(main = paste(
# xlab= Inicio_data,"/", xlab= Fim_data),
#line = 0.5,
#cex = 0.5,
#font.main = 1)


## Contador de vit?rias Buffet
Contador_MF_DFA = matrix(nrow=149)
legend("topleft",
       #"bottomright",
       legend = c(RM, "MARKOWITZ", "SHARPE", "MF_MKW", "MF_SHARPE",
                  "ANNt_EQ",
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
if(nrow(Comparativo)>300) {Eixo_X2 = c(1, 100, 200, 300, 400, 500, 600)
#} else{Eixo_X2 = c(1, 50, 100, 149)}
} else{
  if(nrow(Comparativo)>100) {Eixo_X2 = c(1, 50, 100, 150, 200, 250, 300)
  }else{Eixo_C2 = c(1,25,50,75,100)}}
Eixo_X3 = rownames(Comparativo2[Eixo_X2,])
Eixo_X3 = str_replace(Eixo_X3,"NA","")
Inicio_data = rownames(Comparativo2[1,])
Fim_data = rownames(Comparativo2[nrow(Comparativo2),])
#Fim_data = "2023-03-16"
TestComparativo = cbind(as.data.frame(Comparativo), Eixo)
Retornos=TestComparativo[,1]
Periodos=TestComparativo$Eixo
s = TestComparativo$MARKOWITZ
u = TestComparativo$SHARPE
z = TestComparativo$MF_MKW
p = TestComparativo$MF_SHARPE
w = TestComparativo$ANNt_EQ
t = TestComparativo$ANNt_MKW
q = TestComparativo$ANNt_SHARPE
plot(Periodos, Retornos,
     family="A",
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
title(paste("ANNt and Others Portfolios:", N_Assets, "Assets"))
#title(main = paste(
# xlab= Inicio_data,"/", xlab= Fim_data),
#line = 0.5,
#cex = 0.5,
#font.main = 1)


## Contador de vit?rias Buffet
Contador_MF_DFA = matrix(nrow=149)
legend("topleft",
       #"bottomright",
       legend = c(RM, "MARKOWITZ", "SHARPE", "MF_MKW", "MF_SHARPE",
                  "ANNt_EQ",
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
dev.off()

}
