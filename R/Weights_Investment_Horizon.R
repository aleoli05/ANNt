#' Weights_Investment_Horizon
#'@description
#' Return with weights of the portfolio selected. It is necessary to run the Investiment_Horizon command before.
#'@param Portfolio: MF_EQ; MF_MKW; MKW; ANNt_EQ; ANNt_MKW; Sharpe; MF_Sharpe or ANNt_Sharpe
#'@examples
#' Weights_Investment_Horizon(Portfolio='Sharpe')

#'@export
Weights_Investment_Horizon <-function(Portfolio='Sharpe'){
  if(Portfolio=='MF_EQ'){
    load('~/Weights_MF_EQ_Horizon.rda')
    View(Weights_MF_EQ_Horizon)
  }
  if(Portfolio=='MF_MKW'){
    load('~/Weights_MF_MKW_Horizon.rda')
    View(Weights_MF_MKW_Horizon)
  }
  if(Portfolio=='MKW'){
    load('~/Weights_MKW_Horizon.rda')
    View(Weights_MKW_Horizon)
  }
    if(Portfolio=='ANNt_EQ'){
      load('~/Weights_ANNt_EQ_Horizon.rda')
      View(Weights_ANNt_EQ_Horizon)
    }
      if(Portfolio=='ANNt_MKW'){
        load('~/Weights_ANNt_MKW_Horizon.rda')
        View(Weights_ANNt_MKW_Horizon)
      }
        if(Portfolio=='Sharpe'){
          load('~/Weights_Sharpe_Horizon.rda')
          View(Weights_Sharpe_Horizon)
        }
          if(Portfolio=='MF_Sharpe'){
            load('~/Weights_MF_Sharpe_Horizon.rda')
            View(Weights_MF_Sharpe_Horizon)
          }
            if(Portfolio=='ANNt_Sharpe'){
              load('~/Weights_ANNt_Sharpe_Horizon.rda')
              View(Weights_ANNt_Sharpe_Horizon)
            }
}
