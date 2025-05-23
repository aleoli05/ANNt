% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ANNt_Oliveira_Ceretta_Out.R
\name{ANNt_Oliveira_Ceretta_Out}
\alias{ANNt_Oliveira_Ceretta_Out}
\title{ANNt_Oliveira_Ceretta_Out
Command that realize all operations of the package (including Out-of-sample) and save all in a specific past into user past}
\usage{
ANNt_Oliveira_Ceretta_Out(
  Tickers,
  RM,
  Rf,
  Initial_Date,
  Final_Date_Training,
  Final_Date,
  Periodicity,
  Hidden,
  Stepmax,
  Asymmetry = "Negative",
  Type_ANNt,
  N_Assets,
  Base = "yahoo",
  Import = "Yes",
  Exclude_ticket = ""
)
}
\arguments{
\item{Tickers}{Name of the assets or "Current_SP500_Tickers" for all S&P 500 assets}

\item{RM}{Proxy of the market}

\item{Rf}{Risk free rate}

\item{Initial_Date}{Series start Date, format ('Year-Month-Day'). Assets with values not observed in the series are excluded}

\item{Final_Date}{End date of the treatment series}

\item{Periodicity}{should be one of “daily”, “weekly”, “monthly”}

\item{Hidden}{Number of hidden neurons (If ” is the length series). For a good performance use '' to form a square input x hidden matrix of neurons}

\item{Stepmax}{Number of replications per asset to train the ANN. For a good performance, use 7500}

\item{Asymmetry}{"Negative" or "Positive". Shifts the probability of the return being greater than the proxy to the right or left, "Negative" or "Positive". Default is to the right, "Negative"}

\item{Type_ANNt}{Select type ANNt: "T1"= NNet_Signal_Traning; "T2"= NNet_t_Training; "T3"= MC_Signal_Training; "T4"= MC_t_Training; "T5"= NNet_Signal_Test; "T6"= NNet_t_Test; "T7"= MC_Signal_Test; "T8"= Type_ANNt: MC_t_Test}

\item{N_Assets}{Limit of asset numbers in the portfolio}

\item{Base}{Database to use: "yahoo"or "Rus"}

\item{Import}{Import dates from external data base after first import. "Yes"
or "No". "Yes" is the standard.}

\item{Exclude_ticket}{Deletes any ticket from the ticket list that you want to remove for some reason}

\item{Initial_Date_Training}{Training series start Date}
}
\description{
ANNt_Oliveira_Ceretta_Out
Command that realize all operations of the package (including Out-of-sample) and save all in a specific past into user past
}
\examples{
Tickers <-c('AAPL','XOM','TSLA','KO', 'F')
RM <-c('^GSPC') #RM the S&P500
Rf <- 0
Initial_Date <-c('2018-01-03')
Final_Date_Training <- c('2022-12-29')
Final_Date <-c('')
Periodicity <- c('daily')
Hidden <- 5
Stepmax <- 7500
Type_ANNt <- 'T8'
N_Assets <- 3
ANNt_Oliveira_Ceretta_Out(c('AAPL','XOM','TSLA','KO', 'F'), '^GSPC', 0, '2018-01-03', '2022-12-29', '', 'daily',5,7500,'T8',3)
}
