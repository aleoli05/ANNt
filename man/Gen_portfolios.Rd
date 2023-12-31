% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gen_portfolios.R
\name{Gen_portfolios}
\alias{Gen_portfolios}
\title{Gen_portfolios}
\usage{
Gen_portfolios(
  N_Assets,
  Initial_Date_Testing,
  Final_Date_Testing,
  Rf,
  type_ANNt
)
}
\arguments{
\item{N_Assets}{limit of asset numbers in the portfolio}

\item{Initial_Date_Testing}{Initial Date of Test Series}

\item{Final_Date_Testing}{Final Date Test Series, if '' is the system date}

\item{Rf}{Risk free rate}

\item{type_ANNt}{Select type ANNt:
"T1"= NNet_Signal_Traning;
"T2"= NNet_t_Training;
"T3"= MC_Signal_Training;
"T4"= MC_t_Training;
"T5"= NNet_Signal_Test;
"T6"= NNet_t_Test;
"T7"= MC_Signal_Test;
"T8"= Type_ANNt: MC_t_Test}
}
\description{
Gen_portfolios
}
\examples{
N_Assets <- 3
Initial_Date_Testing <- c('2023-01-03')
Final_Date_Testing <- c('')
Rf <- 0
type_ANNt <- 'T8'
# Generate assets portfolio (maximum N assets specified)
Gen_portfolios(3,'2023-01-03','',0,'T8')

}
\author{
Alexandre Silva de Oliveira
}
