#'LSTM_RNN
#'Command that calculate the Long Short Term Recurent Neural Network
#'@param Tickers data frame with return time series
#'@param Lookback Sequence length to Long Short Term
#' @param Inicial_Date Series start Date (Must be 7 periods greater than the analyzed series)
#' @param Date_Training Series finish training date
#' @param Final_Date_Training Series end Date (If '' is the System Date)
#' @param Input_dim Input the dimension of the RNN
#' @param Hidden_dim Input the dimension of the hidden layer
#' @param Num_layers Number of layers
#' @param Output_dim Output dimension of the RNN
#' @param Num_epochs Number of the epochs
#' @param Import Import assets series
#' @param Metric is the "Return" of the asset or the "Excess_Return" of the asset in relation to the benchmark
#' @param Plot Develop the network learning graph
#' @param View_Metrics "True" or "False" for view realtime plot of training metrics
#' @param Verbose Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch)
#'@examples
#'Tickers <-c('AAPL')
#'Lookback <- 8
#'Initial_Date_Training <- c('2018-01-03')
#'Final_Date_Training <- c('2022-12-29')
#'Final_Date_Testing <- c('')
#'Input_dim <- 1
#'Hidden_dim <- 32
#'Num_layers <- 2
#'Output_dim <- 1
#'Num_epochs <- 100
#'Import <- 'Yes'
#'Metric <- 'Return'
#'LSTM_RNN(Tickers, Lookback, Initial_Date_Training=c('2018-01-03'),Final_Date_Training=c('2022-12-29'),Final_Date_Testing=c(''),Input_dim = 1,Hidden_dim = 32,Num_layers = 2,Output_dim = 1,Num_epochs = 100,Import='Yes', Metric='Return')
#'@export
LSTM_RNN <- function(Tickers='AAPL', Lookback=8, Initial_Date_Training=c('2018-01-03'),
                     Final_Date_Training=c('2022-12-29'),
                     Final_Date_Testing=c(''),
                     Input_dim = 1,
                     Hidden_dim = 32,
                     Num_layers = 2,
                     Output_dim = 1,
                     Num_epochs = 100,
                     Import ='Yes',
                     Metric = 'Return',
                     Plot = 'Yes',
                     View_Metrics='TRUE',
                     Verbose=1){
  require(quantmod)
  require(keras)
  require(reticulate)
  library(quantmod)
  library(keras)
  library(reticulate)
  if (!requireNamespace("tensorflow", quietly = TRUE)) {
    install_keras()
  }
  if (!requireNamespace("tensorflow", quietly = TRUE)) {
    install_keras()
  }
  library(tensorflow)

  lookback=Lookback
  if(Import=='Yes'){
        RM='^GSPC'
        Initial_Date <-Initial_Date_Training
        Final_Date <- Final_Date_Testing
      Assets_series (Tickers,RM, Initial_Date, Final_Date,'daily', Exclude_ticket=Exclude)
  }
  load('~/scenario.set.rda')
  scenario = as.data.frame(scenario.set)

    specific = which(colnames(scenario.set)==Tickers)

    if (Metric == 'Return'){
    stock=as.data.frame(scenario.set[,specific])
    } else{
      if(Metric == 'Excess_Return'){
        stock=as.data.frame(scenario.set[,specific]-scenario.set[,1])
      }
    }
    nomes=colnames(scenario.set)
    nome=nomes[specific]
    colnames(stock)=nome
    View(stock)


  if(length(which(rownames(as.data.frame(scenario.set))==Initial_Date_Training))==0){
    while(length(which(rownames(as.data.frame(scenario.set))==Initial_Date_Training))==0){
      dia=as.Date(Initial_Date_Training)
      new_day=dia+1
      Initial_Date_Training = as.character(new_day)
    }
  }

  if(length(which(rownames(as.data.frame(scenario.set))==Final_Date_Training))==0){
    while(length(which(rownames(as.data.frame(scenario.set))==Final_Date_Training))==0){
      dia=as.Date(Final_Date_Training)
      new_day=dia-1
      Final_Date_Training = as.character(new_day)
    }
  }

  if(Final_Date_Testing==('')){
    Final_Date_Testing=Sys.Date()
  }

  if(length(which(rownames(as.data.frame(scenario.set))==Final_Date_Testing))==0){
    while(length(which(rownames(as.data.frame(scenario.set))==Final_Date_Testing))==0){
      dia=as.Date(Final_Date_Testing)
      new_day=dia-1
      Final_Date_Testing = as.character(new_day)
    }
  }
  Inicio_train = which(row.names(scenario)==Initial_Date_Training)
  Final_train = which(row.names(scenario)==Final_Date_Training)
  Final_test = which(row.names(scenario)==Final_Date_Testing)

  split_data <- function(stock, lookback) {
    data_raw <- as.matrix(stock) # convert to matrix
    data <- array(dim = c(0, lookback, ncol(data_raw)))

    # create all possible sequences of length lookback
    for (index in 1:(nrow(data_raw) - lookback)) {
      data <- rbind(data, data_raw[index:(index + lookback - 1), ])
    }
    #Inicio_train = which(row.names(scenario.set)==Initial_Date_Training)
    #Final_train = which(row.names(scenario.set)==Final_Date_Training)
    #Final_test = which(row.names(scenario.set)==Final_Date_Testing)

    x_train <- data[Inicio_train:Final_train, 1:(lookback - 1), drop = FALSE]
    y_train <- data[Inicio_train:Final_train, lookback, drop = FALSE]

    x_test <- data[(Final_train + 1):Final_test-lookback, 1:(lookback - 1), drop = FALSE]
    y_test <- data[(Final_train + 1):Final_test-lookback, lookback, drop = FALSE]

    return(list(x_train = x_train, y_train = y_train,
                x_test = x_test, y_test = y_test))
  }
  #divide data into train and test
  #lookback <- 8 # choose sequence length
  stock_scale = scale(stock)
  split_data <- split_data(stock_scale, lookback) # assuming "stock" is a data frame
  x_train <- split_data$x_train
  y_train <- split_data$y_train
  x_test <- split_data$x_test
  y_test <- split_data$y_test

  cat(paste('x_train.shape = ', dim(x_train), '\n'))
  cat(paste('y_train.shape = ', dim(y_train), '\n'))
  cat(paste('x_test.shape = ', dim(x_test), '\n'))
  cat(paste('y_test.shape = ', dim(y_test), '\n'))
  #decide hyperparameters
  #input_dim <- 1
  #hidden_dim <- 32
  #num_layers <- 2
  #output_dim <- 1
  #num_epochs <- 100
  input_dim= Input_dim
  hidden_dim <- Hidden_dim
  num_layers <- Num_layers
  output_dim <- Output_dim
  num_epochs <- Num_epochs
  # Reshape the training and test data to have a 3D tensor shape
  x_train <- array_reshape(x_train, c(dim(x_train)[1], lookback-1, input_dim))
  x_test <- array_reshape(x_test, c(dim(x_test)[1], lookback-1, input_dim))

  # Define the LSTM model using Keras
  model <- keras_model_sequential() %>%
    layer_lstm(units = hidden_dim, return_sequences = TRUE, input_shape = c(lookback-1, input_dim)) %>%
    layer_lstm(units = hidden_dim) %>%
    layer_dense(units = output_dim)
  # Compile the model using the mean squared error loss and the Adam optimizer
  model %>% compile(loss = "mean_squared_error", optimizer = optimizer_adam(learning_rate = 0.01))
  model
  # Train the model on the training data
  history <- model %>% fit(x_train, y_train, epochs = num_epochs, batch_size = 16, validation_data = list(x_test, y_test), view_metrics=View_Metrics, verbose=Verbose)


  # Calculate the predicted returns using the LSTM model
  y_train_pred_returns <- model %>% predict(x_train)
  y_test_pred_returns <- model %>% predict(x_test)


  if(Plot=='Yes'){
   # Set up the layout of the plots
  par(mfrow = c(1,2))
  options(repr.plot.width=15, repr.plot.height=8)
  # Plot the training and predicted values
  plot(y_train, type = "l", col = "green",main="Apple daily Log Returns", xlab = "Day", ylab = "Returns",lwd =3)
  lines(y_train_pred_returns, col = "red")
  legend(x = "topleft", legend = c("Train", "Train Predictions"), col = c("green", "red"), lwd = 3, bty='n', cex=0.5)
  grid()
  # Plot the loss of training data
  plot(history$metrics$loss, type = "l", xlab = "Epochs",main="Training Loss", ylab = "Loss", col = "blue", lwd =3)
  grid()
  }

  # Calculate the mean and standard deviation of the original dataset
  mean_return <- mean(stock[,1])
  sd_return <- sd(stock[,1])
  # Rescale the predicted and original values
  y_train_pred_rescaled <- y_train_pred_returns * sd_return + mean_return
  y_test_pred_rescaled <- y_test_pred_returns * sd_return + mean_return
  y_train_rescaled<-y_train * sd_return + mean_return
  y_test_rescaled<-y_test * sd_return + mean_return
  # Shift the predicted values to start from where the training data predictions end
  shift <- length(y_train_pred_rescaled)
  y_test_pred_shifted <- c(rep(NA, shift), y_test_pred_rescaled[,1])

  # Plot the training and predicted values
  if(Plot=='Yes'){
      par(mfrow = c(1,1))
      options(repr.plot.width=20, repr.plot.height=8)
      plot(stock[,1], type = "l", col = "green",main="LSTM-APPLE-RETURNS PREDICTION", xlab = "Day", ylab = "Returns",lwd=3)
      lines(y_train_pred_rescaled, col = "blue",lwd=3)
      lines(y_test_pred_shifted, col = "red",lwd=3)
      legend(x = "topleft", legend = c("Original", "Train Predictions","Test-Prediction"),
             col = c("green","blue" ,"red"), lwd = 2, bty='n', cex=0.5)
      grid()
  }
  colnames(y_train_pred_rescaled)=nome
  colnames(y_test_pred_rescaled)=nome
  r_nomes_train = scenario.set[Inicio_train:Final_train,]
  rownames(y_train_pred_rescaled)=rownames(r_nomes_train)
  r_nomes_test = scenario.set[(Final_train+1):Final_test,]
  rownames(y_test_pred_rescaled)=rownames(r_nomes_test)
  save(y_train_pred_rescaled,file='~/y_train_pred_rescaled.rda')
  save(y_test_pred_rescaled,file='~/y_test_pred_rescaled.rda')

  #calculate mean squre error
  mse <- history$metrics$val_loss[length(history$metrics$val_loss)]
  mse <- round(mse, 7)
  mse
}
