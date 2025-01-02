#' Weights_Investment_Horizon
#'@description
#' Return with weights of the portfolio selected. It is necessary to run the Investiment_Horizon command before.
#'@param Portfolio: MF_EQ; MF_MKW; MKW; ANNt_EQ; ANNt_MKW; Sharpe; MF_Sharpe or ANNt_Sharpe
#'@examples
#' Weights_Investment_Horizon(Portfolio='Sharpe')

#'@export
Weights_Investment_Horizon <-function(Portfolio='Sharpe'){

  ydev=dev.list()
  if(class(ydev)!="NULL"){
    dev.off()
  }

  if(Portfolio=='MF_EQ'){
    load('~/Weights_MF_EQ_Horizon.rda')
    hist=Weights_MF_EQ_Horizon
    View(Weights_MF_EQ_Horizon)
  }
  if(Portfolio=='MF_MKW'){
    load('~/Weights_MF_MKW_Horizon.rda')
    hist=Weights_MF_MKW_Horizon
    View(Weights_MF_MKW_Horizon)
  }
  if(Portfolio=='MKW'){
    load('~/Weights_MKW_Horizon.rda')
    hist=Weights_MKW_Horizon
    View(Weights_MKW_Horizon)
  }
    if(Portfolio=='ANNt_EQ'){
      load('~/Weights_ANNt_EQ_Horizon.rda')
      hist=Weights_ANNt_EQ_Horizon
      View(Weights_ANNt_EQ_Horizon)
    }
      if(Portfolio=='ANNt_MKW'){
        load('~/Weights_ANNt_MKW_Horizon.rda')
        hist=Weights_ANNt_MKW_Horizon
        View(Weights_ANNt_MKW_Horizon)
      }
        if(Portfolio=='Sharpe'){
          load('~/Weights_Sharpe_Horizon.rda')
          hist=Weights_Sharpe_Horizon
          View(Weights_Sharpe_Horizon)
        }
          if(Portfolio=='MF_Sharpe'){
            load('~/Weights_MF_Sharpe_Horizon.rda')
            hist=Weights_MF_Sharpe_Horizon
            View(Weights_MF_Sharpe_Horizon)
          }
            if(Portfolio=='ANNt_Sharpe'){
              load('~/Weights_ANNt_Sharpe_Horizon.rda')
              hist=Weights_ANNt_Sharpe_Horizon
              View(Weights_ANNt_Sharpe_Horizon)
            }
  library(dplyr)
  options(warn=-1)
  Histograma = NULL
  contador=NULL
  ct=seq(from = 3, to = nrow(hist)-1, by = 2)
  for (j in ct){
    for (i in 3:ncol(hist)){
      contador= c(hist[j,i],hist[j+1,i])
      Histograma <- rbind(Histograma,contador)
    }
  }
  Histograma = as.data.frame(na.omit(Histograma))
  #View(Histograma)
  Tabela = table(Histograma[,1])
  Medias=Histograma %>% group_by(V1) %>% summarise(mean(as.numeric(V2)))
  #View(Medias)
  Tab_Freq = cbind(Tabela,Medias)
  Nomes = c('Asset','Frequency','Mean Weight')
  Tab_Freq_ = Tab_Freq[,-3]
  colnames(Tab_Freq_)=Nomes
  Tab_Freq_$`Mean Weight`=round(Tab_Freq_$`Mean Weight`,2)
  #Tab_Freq_=sort(Tab_Freq_$Frequency, decreasing = TRUE)
  #Tab_Freq_2=Tab_Freq_ %>% arrange(desc(Tab_Freq_$Frequency))
  Tab_Freq_2= Tab_Freq_[order(Tab_Freq_$Frequency, decreasing=TRUE),]
  #View(Tab_Freq_2)
  library(ggplot2)
  Mean=round(Tab_Freq_2$`Mean Weight`,2)
  Assets =c(Tab_Freq_2$Asset)
  #ggplot(Tab_Freq_2, aes(y=Frequency,x=Assets, fill=Assets))+
  # geom_bar(stat='identity')+
  #  geom_text(geom = 'text', color = 'black',
  #             aes(label=Mean),
  #             position = position_stack(vjust = 0.5))

  ggplot(Tab_Freq_2, aes(y=Frequency,x=reorder(Asset,Frequency,decreasing=TRUE),, fill=Assets))+
    geom_bar(stat='identity')+
    geom_text(geom = 'text', color = 'black',
              aes(label=Mean),
              position = position_stack(vjust = 0.5))+
    ggtitle('Frequency and Weights of the Assets')

  #barplot(Tab_Freq_$Frequency, names.arg =Assets)
  #barplot(Tab_Freq_$Frequency, horiz=TRUE, names.arg =Assets, cex.names=0.5)


}
